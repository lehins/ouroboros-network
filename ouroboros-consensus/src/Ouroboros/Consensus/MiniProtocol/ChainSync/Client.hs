{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-strictness #-}
-- NOTE: With @-fstrictness@ optimisation (enabled by default for -O1), we get
-- an unexplained thunk in 'KnownIntersectionState' and thus a space leak. See
-- #1356.

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client (
    ChainDbView (..)
  , ChainSyncClientException (..)
  , ChainSyncClientResult (..)
  , Consensus
  , Our (..)
  , Their (..)
  , bracketChainSyncClient
  , chainSyncClient
  , defaultChainDbView
    -- * Trace events
  , InvalidBlockReason
  , TraceChainSyncClientEvent (..)
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (unsafeNoThunks)

import           Network.TypedProtocol.Pipelined
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block (Tip, getTipBlockNo)
import           Ouroboros.Network.Mux (ControlMessage (..), ControlMessageSTM)
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HeaderStateHistory
                     (HeaderStateHistory (..), validateHeader)
import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
import           Ouroboros.Consensus.HeaderValidation hiding (validateHeader)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerConfig, LedgerState, applyChainTick, tickThenReapply)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Types
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Assert (assertWithMsg)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Fingerprint, Watcher (..),
                     WithFingerprint (..), withWatcher)

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB,
                     InvalidBlockReason)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB

type Consensus (client :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type) blk m =
   client (Header blk) (Point blk) (Tip blk) m ChainSyncClientResult

-- | Abstract over the ChainDB
data ChainDbView m blk = ChainDbView {
      getCurrentChain       :: STM m (AnchoredFragment (Header blk))
    , getHeaderStateHistory :: STM m (HeaderStateHistory blk)
    , getPastLedger         :: Point blk -> STM m (Maybe (ExtLedgerState blk))
    , getIsInvalidBlock     :: STM m (WithFingerprint (HeaderHash blk -> Maybe (InvalidBlockReason blk)))
    , getIsFetched          :: STM m (Point blk -> Bool)
    , getBlock              :: RealPoint blk -> m (Maybe blk)
    }

defaultChainDbView ::
     (IOLike m, LedgerSupportsProtocol blk)
  => ChainDB m blk -> ChainDbView m blk
defaultChainDbView chainDB = ChainDbView {
      getCurrentChain       = ChainDB.getCurrentChain       chainDB
    , getHeaderStateHistory = ChainDB.getHeaderStateHistory chainDB
    , getPastLedger         = ChainDB.getPastLedger         chainDB
    , getIsInvalidBlock     = ChainDB.getIsInvalidBlock     chainDB
    , getIsFetched          = ChainDB.getIsFetched          chainDB
    , getBlock              = ChainDB.getBlockComponent     chainDB ChainDB.GetVerifiedBlock
    }

-- newtype wrappers to avoid confusing our tip with their tip.
newtype Their a = Their { unTheir :: a }
  deriving stock   (Eq)
  deriving newtype (Show, NoThunks)

newtype Our   a = Our   { unOur   :: a }
  deriving stock   (Eq)
  deriving newtype (Show, NoThunks)

bracketChainSyncClient
    :: ( IOLike m
       , Ord peer
       , BlockSupportsProtocol blk
       , LedgerSupportsProtocol blk
       )
    => Tracer m (TraceChainSyncClientEvent blk)
    -> ChainDbView m blk
    -> StrictTVar m (Map peer (StrictTVar m (CandidateFragment (Header blk))))
       -- ^ The candidate chains, we need the whole map because we
       -- (de)register nodes (@peer@).
    -> peer
    -> (    (CandidateFragment (Header blk) -> m ())
         -> m a
       )
    -> m a
bracketChainSyncClient tracer ChainDbView { getIsInvalidBlock } varCandidates
                       peer body =
    bracket newCandidateVar releaseCandidateVar
      $ \varCandidate ->
      withWatcher
        "ChainSync.Client.rejectInvalidBlocks"
        (invalidBlockWatcher varCandidate)
        $ body (atomically . writeTVar varCandidate)
  where
    newCandidateVar = do
      varCandidate <- newTVarIO $ CandidateFragment
        { candidateChain        = AF.Empty AF.AnchorGenesis
        , candidateIsLowDensity = False
        }
      atomically $ modifyTVar varCandidates $ Map.insert peer varCandidate
      return varCandidate

    releaseCandidateVar _ = do
      atomically $ modifyTVar varCandidates $ Map.delete peer

    invalidBlockWatcher varCandidate =
      invalidBlockRejector
        tracer
        getIsInvalidBlock
        (candidateChain <$> readTVar varCandidate)

-- Our task: after connecting to an upstream node, try to maintain an
-- up-to-date header-only fragment representing their chain. We maintain
-- such candidate chains in a map with upstream nodes as keys.
--
-- The block fetch logic will use these candidate chains to download
-- blocks from, prioritising certain candidate chains over others using
-- the consensus protocol. Whenever such a block has been downloaded and
-- added to the local 'ChainDB', the 'ChainDB' will perform chain
-- selection.
--
-- We also validate the headers of a candidate chain by advancing the
-- 'ChainDepState' with the headers, which returns an error when validation
-- failed. Thus, in addition to the chain fragment of each candidate, we also
-- store a 'ChainDepState' corresponding to the head of the candidate chain.
--
-- We must keep the candidate chain synchronised with the corresponding
-- upstream chain. The upstream node's chain might roll forward or
-- backwards, and they will inform us about this. When we get these
-- messages, we will replicate these actions on our candidate chain.
--
-- INVARIANT:
--
-- >           our tip
-- >             v
-- >   /--* .... *
-- >   |
-- > --*
-- >   |
-- >   \--* .... *
-- >        fragment tip
--
-- The distance from our tip to the intersection between our chain and the
-- fragment maintained for the upstream node cannot exceed @k@ blocks. When
-- this invariant cannot be maintained, the upstream node is on a fork that
-- is too distant and we should disconnect.
--
-- TODO #423 rate-limit switching chains, otherwise we can't place blame (we
-- don't know which candidate's chain included the point that was
-- poisoned). E.g. two rollbacks per time slot -> make it configurable ->
-- just a simple argument for now.
--
-- TODO #467 if the 'theirTip' that they sent us is on our chain, just
-- switch to it.


-- = Candidate fragment size
-- -------------------------
--
-- The size of the downloaded candidate fragment ('theirFrag') and the
-- corresponding header state history ('theirHeaderStateHistory', which has the
-- same size as 'theirFrag') is limited by how far in the future the ledger view
-- can forecast.
--
-- For PBFT (Byron), we can forecast up to @2k@ slots ahead. Assuming a chain
-- density of 100%, this means the look-ahead is @2k@ headers. For mainnet this
-- means @2 * 2160 = 4320@ headers.
--
-- For TPraos (Shelley), we can forecast up to @3k/f@ slots ahead. Assuming a
-- density of @f@, this means the look-ahead is @3k@ headers. For mainnet, this
-- means @3 * 2160 = 6480@ headers.
--
-- The figure below shows the relation between 'ourFrag' and 'theirFrag':
--
-- >                       k headers or less, when A is genesis
-- >              <--------------------->
-- >            anchor    header       tip
-- >              |         |           |
-- >              V         V           V
-- > 'ourFrag':   A-H-H-H-H-H-H-H-H-...-H
-- >                     \
-- > 'theirFrag':         H-H-H-H-...   ...   ...
-- >                    ^
-- >                    |
-- >           most recent intersection (between A and the tip)
--
-- Note that the 'ourFrag' and 'theirFrag' share anchors /at all times/. In the
-- figure above, the first three headers on 'ourFrag' are thus also on
-- 'theirFrag'. The further away the most recent intersection is from the anchor
-- point, the more headers 'theirFrag' and 'ourFrag' will have in common.
--
-- In the \"worst\" case 'theirFrag' has the following length:
--
-- >                        k
-- >              <--------------------->
-- > 'ourFrag':   A-H-H-H-H-H-H-H-H-...-H
-- >                                    \
-- > 'theirFrag':                        H-H-H-H-H-H-H-H-H-H-H-H-H-H-H...-H
-- >                                     <-------------------------------->
-- >                                               max look-ahead
-- > max length   <------------------------------------------------------->
-- > of 'theirFrag'         k + max look-ahead
--
-- For PBFT this is @2160 + 4320 = 6480@ headers, for TPraos this is @2160 +
-- 6480 = 8640@ headers. The header state history will have the same length.
--
-- This worst case can happen when:
-- * We are more than 6480 or respectively 8640 blocks behind, bulk syncing, and
--   the BlockFetch client and/or the ChainDB can't keep up with the ChainSync
--   client.
-- * When our clock is running behind such that we are not adopting the
--   corresponding blocks because we think they are from the future.
-- * When an attacker is serving us headers from the future.
--
-- When we are in sync with the network, the fragment will typically be @k@ to
-- @k + 1@ headers long.

-- | State used when the intersection between the candidate and the current
-- chain is unknown.
data UnknownIntersectionState blk = UnknownIntersectionState
  { ourFrag               :: !(AnchoredFragment (Header blk))
    -- ^ A view of the current chain fragment. Note that this might be
    -- temporarily out of date w.r.t. the actual current chain until we update
    -- it again.
    --
    -- This fragment is used to select points from to find an intersection
    -- with the candidate.
    --
    -- INVARIANT: 'ourFrag' contains @k@ headers, unless close to genesis.
  , ourHeaderStateHistory :: !(HeaderStateHistory blk)
    -- ^ 'HeaderStateHistory' corresponding to the tip (most recent block) of
    -- 'ourFrag'.
  }
  deriving (Generic)

instance ( LedgerSupportsProtocol blk
         ) => NoThunks (UnknownIntersectionState blk) where
  showTypeOf _ = show $ typeRep (Proxy @(UnknownIntersectionState blk))

-- | State used when the intersection between the candidate and the current
-- chain is known.
data KnownIntersectionState blk = KnownIntersectionState
  { theirFrag               :: !(AnchoredFragment (Header blk))
    -- ^ The candidate, the synched fragment of their chain.
    --
    -- See the \"Candidate fragment size\" note above.
  , theirHeaderStateHistory :: !(HeaderStateHistory blk)
    -- ^ 'HeaderStateHistory' corresponding to the tip (most recent block) of
    -- 'theirFrag'.
    --
    -- INVARIANT: the tips in 'theirHeaderStateHistory' correspond to the
    -- headers in 'theirFrag', including the anchor.
    --
    -- See the \"Candidate fragment size\" note above.
  , ourFrag                 :: !(AnchoredFragment (Header blk))
    -- ^ A view of the current chain fragment used to maintain the invariants
    -- with. Note that this might be temporarily out of date w.r.t. the actual
    -- current chain until we update it again.
    --
    -- INVARIANT: 'ourFrag' contains @k@ headers, unless close to genesis.
    --
    -- INVARIANT: 'theirFrag' and 'ourFrag' have the same anchor point. From
    -- this follows that both fragments intersect. This also means that
    -- 'theirFrag' forks off within the last @k@ headers/blocks of the
    -- 'ourFrag'.
  , mostRecentIntersection  :: !(Point blk)
    -- ^ The most recent intersection point between 'theirFrag' and 'ourFrag'.
    -- Note that this is not necessarily the anchor point of both 'theirFrag'
    -- and 'ourFrag', they might have many more headers in common.
    --
    -- INVARIANT:
    -- > Just 'mostRecentIntersection' == 'AF.intersectionPoint' 'theirFrag' 'ourFrag'
    --
    -- It follows from the invariants on 'ourFrag' that this point is within
    -- the last @k@ headers of the current chain fragment, at time of
    -- computing the 'KnownIntersectionState'.
  }
  deriving (Generic)

instance ( LedgerSupportsProtocol blk
         ) => NoThunks (KnownIntersectionState blk) where
  showTypeOf _ = show $ typeRep (Proxy @(KnownIntersectionState blk))

checkKnownIntersectionInvariants
  :: ( HasHeader blk
     , HasHeader (Header blk)
     , HasAnnTip blk
     , ConsensusProtocol (BlockProtocol blk)
     )
  => ConsensusConfig (BlockProtocol blk)
  -> KnownIntersectionState blk
  -> Either String ()
checkKnownIntersectionInvariants cfg KnownIntersectionState
                                     { ourFrag
                                     , theirFrag
                                     , theirHeaderStateHistory
                                     , mostRecentIntersection
                                     }
    -- 'theirHeaderStateHistory' invariant
    | let HeaderStateHistory snapshots = theirHeaderStateHistory
          historyTips  = headerStateTip        <$> AS.toOldestFirst snapshots
          fragmentTips = NotOrigin . getAnnTip <$> AF.toOldestFirst theirFrag
          historyAnchorPoint =
            withOriginRealPointToPoint $
              annTipRealPoint <$> headerStateTip (AS.anchor snapshots)
          fragmentAnchorPoint = castPoint $ AF.anchorPoint theirFrag
    , historyTips /= fragmentTips || historyAnchorPoint /= fragmentAnchorPoint
    = throwError $ unwords
      [ "The tips in theirHeaderStateHistory didn't match the headers in theirFrag:"
      , show historyTips
      , "vs"
      , show fragmentTips
      , "with anchors"
      , show historyAnchorPoint
      , "vs"
      , show fragmentAnchorPoint
      ]

    -- 'ourFrag' invariants
    | let nbHeaders = AF.length ourFrag
          ourAnchorPoint = AF.anchorPoint ourFrag
    , nbHeaders < fromIntegral k
    , ourAnchorPoint /= GenesisPoint
    = throwError $ unwords
      [ "ourFrag contains fewer than k headers and not close to genesis:"
      , show nbHeaders
      , "vs"
      , show k
      , "with anchor"
      , show ourAnchorPoint
      ]

    | let ourFragAnchor = AF.anchorPoint ourFrag
          theirFragAnchor = AF.anchorPoint theirFrag
    , ourFragAnchor /= theirFragAnchor
    = throwError $ unwords
      [ "ourFrag and theirFrag have different anchor points:"
      , show ourFragAnchor
      , "vs"
      , show theirFragAnchor
      ]

    -- 'mostRecentIntersection' invariant
    | let actualMostRecentIntersection =
            castPoint <$> AF.intersectionPoint theirFrag ourFrag
    , Just mostRecentIntersection /= actualMostRecentIntersection
    = throwError $ unwords
      [ "mostRecentIntersection not the most recent intersection"
      , "of theirFrag and ourFrag:"
      , show mostRecentIntersection
      , "vs"
      , show actualMostRecentIntersection
      ]

    | otherwise
    = return ()
  where
    SecurityParam k = protocolSecurityParam cfg

assertKnownIntersectionInvariants
  :: ( HasHeader blk
     , HasHeader (Header blk)
     , HasAnnTip blk
     , ConsensusProtocol (BlockProtocol blk)
     , HasCallStack
     )
  => ConsensusConfig (BlockProtocol blk)
  -> KnownIntersectionState blk
  -> KnownIntersectionState blk
assertKnownIntersectionInvariants cfg kis =
    assertWithMsg (checkKnownIntersectionInvariants cfg kis) kis

-- | Chain sync client
--
-- This never terminates. In case of a failure, a 'ChainSyncClientException'
-- is thrown. The network layer classifies exception such that the
-- corresponding peer will never be chosen again.
chainSyncClient
    :: forall m blk.
       ( IOLike m
       , LedgerSupportsProtocol blk
       )
    => MkPipelineDecision
    -> Tracer m (TraceChainSyncClientEvent blk)
    -> TopLevelConfig blk
    -> ChainDbView m blk
    -> NodeToNodeVersion
    -> ControlMessageSTM m
    -> (CandidateFragment (Header blk) -> m ())
    -> Consensus ChainSyncClientPipelined blk m
chainSyncClient mkPipelineDecision0 tracer cfg
                cdbView@ChainDbView
                { getCurrentChain
                , getHeaderStateHistory
                , getIsInvalidBlock
                }
                _version
                controlMessageSTM
                setCandidate = ChainSyncClientPipelined $
    continueWithState () $ initialise
  where
    -- | Start ChainSync by looking for an intersection between our current
    -- chain fragment and their chain.
    initialise :: StatefulConsensus m blk () (ClientPipelinedStIdle 'Z)
    initialise = findIntersection (ForkTooDeep GenesisPoint)

    -- | Try to find an intersection by sending points of our current chain to
    -- the server, if any of them intersect with their chain, roll back our
    -- chain to that point and start synching using that fragment. If none
    -- intersect, disconnect by throwing the exception obtained by calling the
    -- passed function.
    findIntersection
      :: (Our (Tip blk) -> Their (Tip blk) -> ChainSyncClientResult)
         -- ^ Exception to throw when no intersection is found.
      -> StatefulConsensus m blk () (ClientPipelinedStIdle 'Z)
    findIntersection mkResult = Stateful $ \() -> do
      (ourFrag, ourHeaderStateHistory) <- atomically $ (,)
        <$> getCurrentChain
        <*> getHeaderStateHistory
      -- We select points from the last @k@ headers of our current chain. This
      -- means that if an intersection is found for one of these points, it
      -- was an intersection within the last @k@ blocks of our current chain.
      -- If not, we could never switch to this candidate chain anyway.
      let maxOffset = fromIntegral (AF.length ourFrag)
          points    = map castPoint
                    $ AF.selectPoints
                        (map fromIntegral (offsets maxOffset))
                        ourFrag
          uis = UnknownIntersectionState {
              ourFrag               = ourFrag
            , ourHeaderStateHistory = ourHeaderStateHistory
            }
      return $ SendMsgFindIntersect points $ ClientPipelinedStIntersect
        { recvMsgIntersectFound = \i theirTip' ->
            continueWithState uis $
              intersectFound (castPoint i) (Their theirTip')
        , recvMsgIntersectNotFound = \theirTip' ->
            terminate $
              mkResult
                (ourTipFromChain ourFrag)
                (Their theirTip')
        }

    -- | One of the points we sent intersected our chain. This intersection
    -- point will become the new tip of the candidate chain.
    intersectFound :: Point blk  -- ^ Intersection
                   -> Their (Tip blk)
                   -> StatefulConsensus m blk
                        (UnknownIntersectionState blk)
                        (ClientPipelinedStIdle 'Z)
    intersectFound intersection theirTip
                 = Stateful $ \UnknownIntersectionState
                     { ourFrag
                     , ourHeaderStateHistory
                     } -> do
      traceWith tracer $
        TraceFoundIntersection intersection (ourTipFromChain ourFrag) theirTip
      traceException $ do
        -- Roll back the current chain fragment to the @intersection@.
        --
        -- While the primitives in the ChainSync protocol are "roll back",
        -- "roll forward (apply block)", etc. The /real/ primitive is "switch
        -- to fork", which means that a roll back is always followed by
        -- applying at least as many blocks that we rolled back.
        --
        -- This is important for 'rewindHeaderStateHistory', which can only roll
        -- back up to @k@ blocks, /once/, i.e., we cannot keep rolling back the
        -- same chain state multiple times, because that would mean that we
        -- store the chain state for the /whole chain/, all the way to genesis.
        --
        -- So the rewind below is fine when we are switching to a fork (i.e.
        -- it is followed by rolling forward again), but we need some
        -- guarantees that the ChainSync protocol /does/ in fact give us a
        -- switch-to-fork instead of a true rollback.
        (theirFrag, theirHeaderStateHistory) <- do
          case attemptRollback intersection (ourFrag, ourHeaderStateHistory) of
            Just (c, d) -> return (c, d)
            -- The @intersection@ is not on the candidate chain, even though
            -- we sent only points from the candidate chain to find an
            -- intersection with. The node must have sent us an invalid
            -- intersection point.
            Nothing -> disconnect $
              InvalidIntersection
                intersection
                (ourTipFromChain ourFrag)
                theirTip
        let kis = assertKnownIntersectionInvariants (configConsensus cfg) $
              KnownIntersectionState
                { theirFrag               = theirFrag
                , theirHeaderStateHistory = theirHeaderStateHistory
                , ourFrag                 = ourFrag
                , mostRecentIntersection  = intersection
                }

        continueWithState kis $ nextStep mkPipelineDecision0 Zero theirTip

    -- | Request the next message (roll forward or backward), unless our chain
    -- has changed such that it no longer intersects with the candidate, in
    -- which case we initiate the intersection finding part of the protocol.
    --
    -- This is the main place we check whether our current chain has changed.
    -- We also check it in 'rollForward' to make sure we have an up-to-date
    -- intersection before calling 'getLedgerView'.
    --
    -- This is also the place where we checked whether we're asked to terminate
    -- by the mux layer.
    nextStep :: MkPipelineDecision
             -> Nat n
             -> Their (Tip blk)
             -> StatefulConsensus m blk
                  (KnownIntersectionState blk)
                  (ClientPipelinedStIdle n)
    nextStep mkPipelineDecision n theirTip = Stateful $ \kis -> do
      setCandidate CandidateFragment
        { candidateChain        = theirFrag kis
        , candidateIsLowDensity = False
            -- Note that we only call 'nextStep' after having fully validated
            -- the previous message.
        }
      atomically controlMessageSTM >>= \case
        -- We have been asked to terminate the client
        Terminate ->
          terminateAfterDrain n $ AskedToTerminate
        _continue -> do
          rechk <- atomically $ stillIntersectsWithCurrentChain cfg cdbView kis
          case recheckToMaybe kis rechk of
            Just kis'@KnownIntersectionState { theirFrag } -> do
              -- Our chain (tip) didn't change or if it did, it still intersects
              -- with the candidate fragment, so we can continue requesting the
              -- next block.
              let candTipBlockNo = AF.headBlockNo theirFrag
              return $
                requestNext kis' mkPipelineDecision n theirTip candTipBlockNo
            Nothing ->
              -- Our chain (tip) has changed and it no longer intersects with
              -- the candidate fragment, so we have to find a new intersection,
              -- but first drain the pipe.
              continueWithState ()
                $ drainThePipe n
                $ findIntersection NoMoreIntersection

    -- | "Drain the pipe": collect and discard all in-flight responses and
    -- finally execute the given action.
    drainThePipe :: forall s n. NoThunks s
                 => Nat n
                 -> StatefulConsensus m blk s (ClientPipelinedStIdle 'Z)
                 -> StatefulConsensus m blk s (ClientPipelinedStIdle n)
    drainThePipe n0 m = Stateful $ go n0
      where
        go :: forall n'. Nat n'
           -> s
           -> m (Consensus (ClientPipelinedStIdle n') blk m)
        go n s = case n of
          Zero    -> continueWithState s m
          Succ n' -> return $ CollectResponse Nothing $ ClientStNext
            { recvMsgRollForward  = \_hdr _tip -> go n' s
            , recvMsgRollBackward = \_pt  _tip -> go n' s
            }

    requestNext :: KnownIntersectionState blk
                -> MkPipelineDecision
                -> Nat n
                -> Their (Tip blk)
                -> WithOrigin BlockNo
                -> Consensus (ClientPipelinedStIdle n) blk m
    requestNext kis mkPipelineDecision n theirTip candTipBlockNo =
        case (n, decision) of
          (Zero, (Request, mkPipelineDecision')) ->
            SendMsgRequestNext
              (handleNext kis mkPipelineDecision' Zero)
              (return $ handleNext kis mkPipelineDecision' Zero) -- when we have to wait
          (_, (Pipeline, mkPipelineDecision')) ->
            SendMsgRequestNextPipelined
              (requestNext kis mkPipelineDecision' (Succ n) theirTip candTipBlockNo)
          (Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
            CollectResponse
              (Just $ pure $ SendMsgRequestNextPipelined $
                requestNext kis mkPipelineDecision' (Succ n) theirTip candTipBlockNo)
              (handleNext kis mkPipelineDecision' n')
          (Succ n', (Collect, mkPipelineDecision')) ->
            CollectResponse
              Nothing
              (handleNext kis mkPipelineDecision' n')
      where
        theirTipBlockNo = getTipBlockNo (unTheir theirTip)
        decision = runPipelineDecision
          mkPipelineDecision
          n
          candTipBlockNo
          theirTipBlockNo

    handleNext :: KnownIntersectionState blk
               -> MkPipelineDecision
               -> Nat n
               -> Consensus (ClientStNext n) blk m
    handleNext kis mkPipelineDecision n = ClientStNext
      { recvMsgRollForward  = \hdr theirTip -> do
          traceWith tracer $ TraceDownloadedHeader hdr
          continueWithState kis $
            rollForward mkPipelineDecision n hdr (Their theirTip)
      , recvMsgRollBackward = \intersection theirTip -> do
          let intersection' :: Point blk
              intersection' = castPoint intersection
          traceWith tracer $ TraceRolledBack intersection'
          continueWithState kis $
            rollBackward mkPipelineDecision n intersection' (Their theirTip)
      }

    -- This handler validates the received header. It cannot do so before
    -- acquiring a 'LedgerView' for that header's slot.
    --
    -- Under normal circumstances, the 'LedgerView' is forecast from the
    -- (cached) 'LedgerState' that resulted from applying the block that is the
    -- 'mostRecentIntersection' of the peer's chain with the (local node's)
    -- current chain. However, if the peer's chain has reached a low enough
    -- density (low enough to indicate a disaster of some sort), we will not be
    -- able to forecast from the intersection's ledger state to the new
    -- header's slot.
    --
    -- There are two possible ways foward in the low-density case. First, we
    -- can reactively wait until our current chain evolves such that the
    -- intersection becomes recent enough that the new forecast range reaches
    -- the RollForward header. (Note that this is not gauranteed to happen.)
    -- Second, we can proactively begin fetching the blocks on their chain. In
    -- general, these fetchs will not affect the local node's current chain
    -- selection. Each block we fetch lets us advance the intersection's ledger
    -- state through time by applying block to it (via 'tickThenReapply'). This
    -- advancement is happening temporarily, in this ChainSync client's
    -- ephemeral local state; we're not affecting the local node's selected
    -- chain at all. Eventually that advanced ledger state will either be able
    -- to forecast the ledger view for the RollForward header's slot or else it
    -- will actually be the ledger state in which that header was forged and so
    -- we can simply tick it up to the necessary slot. In that latter case,
    -- since there are no blocks in between, we need not do any /forecasting/.
    --
    -- We support both of those ways forward: we react to changes in the local
    -- node's current chain and, for a low density chain, we /simultaneously/
    -- instruct BlockFetch to fetch the blocks on the peer's chain (but only up
    -- to @k@ blocks after the intersection) and ephemerally apply them as
    -- discussed above they become available.
    rollForward :: MkPipelineDecision
                -> Nat n
                -> Header blk
                -> Their (Tip blk)
                -> StatefulConsensus m blk
                     (KnownIntersectionState blk)
                     (ClientPipelinedStIdle n)
    rollForward mkPipelineDecision n hdr theirTip
              = Stateful $ \kis -> traceException $ do
      let hdrSlot  = blockSlot   hdr
          hdrPoint = headerPoint hdr
      -- Reject the block if invalid
      isInvalidBlock <- atomically $ forgetFingerprint <$> getIsInvalidBlock
      whenJust (isInvalidBlock (headerHash  hdr)) $ \reason ->
        disconnect $ InvalidBlock hdrPoint reason

      -- Get the ledger view required to validate the header
      intersectCheck <- join $ atomically $ do
        rechk <- stillIntersectsWithCurrentChain cfg cdbView kis
        case recheckToMaybe kis rechk of
          Nothing   -> return $ return NoLongerIntersects
          Just kis' ->
              continueWithState kis'
                $ checkDensityOrContinue cfg cdbView hdrSlot
                $ lowDensityHandler cfg cdbView setCandidate hdrSlot

      case intersectCheck of
        NoLongerIntersects ->
          -- Our chain (tip) has changed and it no longer intersects with the
          -- candidate fragment, so we have to find a new intersection, but
          -- first drain the pipe.
          continueWithState ()
            $ drainThePipe n
            $ findIntersection NoMoreIntersection

        LedgerViewAcquired kis' ledgerView -> do
          -- Our chain still intersects with the candidate fragment and we
          -- have obtained a 'LedgerView' that we can use to validate @hdr@.

          let KnownIntersectionState {
                  ourFrag
                , theirFrag
                , theirHeaderStateHistory
                , mostRecentIntersection
                } = kis'

          -- Validate header
          let expectPrevHash = castHash (AF.headHash theirFrag)
              actualPrevHash = headerPrevHash hdr
          when (actualPrevHash /= expectPrevHash) $
            disconnect $
              DoesntFit
                actualPrevHash
                expectPrevHash
                (ourTipFromChain ourFrag)
                theirTip

          theirHeaderStateHistory' <-
            case runExcept $ validateHeader cfg ledgerView hdr theirHeaderStateHistory of
              Right theirHeaderStateHistory' -> return theirHeaderStateHistory'
              Left  vErr ->
                disconnect $
                  HeaderError hdrPoint vErr (ourTipFromChain ourFrag) theirTip

          let theirFrag' = theirFrag :> hdr
              -- Advance the most recent intersection if we have the same header
              -- on our fragment too. This is cheaper than recomputing the
              -- intersection from scratch.
              mostRecentIntersection'
                | Just ourSuccessor <-
                    AF.successorBlock (castPoint mostRecentIntersection) ourFrag
                , headerHash ourSuccessor == headerHash hdr
                = headerPoint hdr
                | otherwise
                = mostRecentIntersection
              kis'' = assertKnownIntersectionInvariants (configConsensus cfg) $
                KnownIntersectionState {
                    theirFrag               = theirFrag'
                  , theirHeaderStateHistory = theirHeaderStateHistory'
                  , ourFrag                 = ourFrag
                  , mostRecentIntersection  = mostRecentIntersection'
                  }

          continueWithState kis'' $ nextStep mkPipelineDecision n theirTip

    rollBackward :: MkPipelineDecision
                 -> Nat n
                 -> Point blk
                 -> Their (Tip blk)
                 -> StatefulConsensus m blk
                      (KnownIntersectionState blk)
                      (ClientPipelinedStIdle n)
    rollBackward mkPipelineDecision n rollBackPoint
                 theirTip
               = Stateful $ \KnownIntersectionState
                   { theirFrag
                   , theirHeaderStateHistory
                   , ourFrag
                   , mostRecentIntersection
                   } -> traceException $ do
        case attemptRollback rollBackPoint (theirFrag, theirHeaderStateHistory) of
          -- Remember that we use our current chain fragment as the starting
          -- point for the candidate's chain. Our fragment contained @k@
          -- headers. At this point, the candidate fragment might have grown to
          -- more than @k@ or rolled back to less than @k@ headers.
          --
          -- But now, it rolled back to some point that is not on the fragment,
          -- which means that it tried to roll back to some point before one of
          -- the last @k@ headers we initially started from. We could never
          -- switch to this fork anyway, so just disconnect. Furthermore, our
          -- current chain might have advanced in the meantime, so the point we
          -- would have to roll back to might have been much further back than
          -- @k@ blocks (> @k@ + the number of blocks we have advanced since
          -- starting syncing).
          --
          -- INVARIANT: a candidate fragment contains @>=k@ headers (unless
          -- near genesis, in which case we mean the total number of blocks in
          -- the fragment) minus @r@ headers where @r <= k@. This ghost
          -- variable @r@ indicates the number of headers we temporarily
          -- rolled back. Such a rollback must always be followed by rolling
          -- forward @s@ new headers where @s >= r@.
          --
          -- Thus, @k - r + s >= k@.
          Nothing ->
            terminateAfterDrain n $
              RolledBackPastIntersection
                rollBackPoint
                (ourTipFromChain ourFrag)
                theirTip

          Just (theirFrag', theirHeaderStateHistory') -> do
            -- We just rolled back to @intersection@, either our most recent
            -- intersection was after or at @intersection@, in which case
            -- @intersection@ becomes the new most recent intersection.
            --
            -- But if the most recent intersection was /before/ @intersection@,
            -- then the most recent intersection doesn't change.
            let mostRecentIntersection'
                  | AF.withinFragmentBounds (castPoint rollBackPoint) ourFrag
                  = rollBackPoint
                  | otherwise
                  = mostRecentIntersection
                kis' = assertKnownIntersectionInvariants (configConsensus cfg) $
                  KnownIntersectionState {
                      theirFrag               = theirFrag'
                    , theirHeaderStateHistory = theirHeaderStateHistory'
                    , ourFrag                 = ourFrag
                    , mostRecentIntersection  = mostRecentIntersection'
                    }

            continueWithState kis' $ nextStep mkPipelineDecision n theirTip

    -- | Gracefully terminate the connection with the upstream node with the
    -- given result.
    terminate :: ChainSyncClientResult -> m (Consensus (ClientPipelinedStIdle 'Z) blk m)
    terminate res = do
      traceWith tracer (TraceTermination res)
      pure (SendMsgDone res)

    -- | Same as 'terminate', but first 'drainThePipe'.
    terminateAfterDrain :: Nat n -> ChainSyncClientResult -> m (Consensus (ClientPipelinedStIdle n) blk m)
    terminateAfterDrain n result =
          continueWithState ()
        $ drainThePipe n
        $ Stateful $ const $ terminate result

    -- | Disconnect from the upstream node by throwing the given exception.
    -- The cleanup is handled in 'bracketChainSyncClient'.
    disconnect :: forall m' x'. MonadThrow m'
               => ChainSyncClientException -> m' x'
    disconnect = throwIO

    -- | Trace any 'ChainSyncClientException' if thrown.
    traceException :: m a -> m a
    traceException m = m `catch` \(e :: ChainSyncClientException) -> do
      traceWith tracer $ TraceException e
      throwIO e

    ourTipFromChain :: AnchoredFragment (Header blk) -> Our (Tip blk)
    ourTipFromChain = Our . AF.anchorToTip . AF.headAnchor

    -- Recent offsets
    --
    -- These offsets are used to find an intersection point between our chain
    -- and the upstream node's. We use the fibonacci sequence to try blocks
    -- closer to our tip, and fewer blocks further down the chain. It is
    -- important that this sequence constains at least a point @k@ back: if no
    -- intersection can be found at most @k@ back, then this is not a peer
    -- that we can sync with (since we will never roll back more than @k).
    --
    -- For @k = 2160@, this evaluates to
    --
    -- > [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2160]
    --
    -- For @k = 5@ (during testing), this evaluates to
    --
    -- > [0,1,2,3,5]
    --
    -- In case the fragment contains less than @k@ blocks, we use the length
    -- of the fragment as @k@. This ensures that the oldest rollback point is
    -- selected.
    offsets :: Word64 -> [Word64]
    offsets maxOffset = [0] ++ takeWhile (< l) [fib n | n <- [2..]] ++ [l]
      where
        l = k `min` maxOffset

    k :: Word64
    k = maxRollbacks $ configSecurityParam cfg

{-------------------------------------------------------------------------------
  Reacting to changes in the local node's current chain
-------------------------------------------------------------------------------}

data IntersectsRecheck blk
    = LostIntersection
    | NewIntersection !(KnownIntersectionState blk)
    | SameIntersection

recheckToMaybe ::
     KnownIntersectionState blk
  -> IntersectsRecheck blk
  -> Maybe (KnownIntersectionState blk)
recheckToMaybe old = \case
    LostIntersection    -> Nothing
    NewIntersection new -> Just new
    SameIntersection    -> Just old

-- | Look at the current chain fragment that may have been updated in the
-- background. Check whether the candidate fragment still intersects with
-- it. If so, update the 'KnownIntersectionState' and trim the candidate
-- fragment to the new current chain fragment's anchor point. If not,
-- return 'Nothing'.
stillIntersectsWithCurrentChain :: forall blk m.
     (MonadSTM m, LedgerSupportsProtocol blk)
  => TopLevelConfig blk
  -> ChainDbView m blk
  -> KnownIntersectionState blk
  -> STM m (IntersectsRecheck blk)
stillIntersectsWithCurrentChain cfg cdbView kis = do
    ourFrag' <- getCurrentChain
    if
      | AF.headPoint ourFrag == AF.headPoint ourFrag' ->
        -- Our current chain didn't change, and changes to their chain that
        -- might affect the intersection point are handled elsewhere
        -- ('rollBackward'), so we have nothing to do.
        return SameIntersection

      | Just intersection <- AF.intersectionPoint ourFrag' theirFrag ->
        -- Our current chain changed, but it still intersects with candidate
        -- fragment, so update the 'ourFrag' field and trim to the candidate
        -- fragment to the same anchor point.
        --
        -- Note that this is the only place we need to trim. Headers on their
        -- chain can only become unnecessary (eligible for trimming) in two
        -- ways: 1. we adopted them, i.e., our chain changed (handled in this
        -- function); 2. we will /never/ adopt them, which is handled in the
        -- "no more intersection case".
        case AF.splitAfterPoint theirFrag (AF.anchorPoint ourFrag') of
         -- + Before the update to our fragment, both fragments were
         --   anchored at the same anchor.
         -- + We still have an intersection.
         -- + The number of blocks after the intersection cannot have
         --   shrunk, but could have increased.
         -- + If it did increase, the anchor point will have shifted up.
         -- + It can't have moved up past the intersection point (because
         --   then there would be no intersection anymore).
         -- + This means the new anchor point must be between the old anchor
         --   point and the new intersection point.
         -- + Since we know both the old anchor point and the new
         --   intersection point exist on their fragment, the new anchor
         --   point must also.
         Nothing -> error
             "anchor point must be on candidate fragment if they intersect"
         Just (_, trimmedCandidateFrag) -> return $ NewIntersection $
             assertKnownIntersectionInvariants (configConsensus cfg) $
               KnownIntersectionState {
                   ourFrag                 = ourFrag'
                 , theirFrag               = trimmedCandidateFrag
                 , theirHeaderStateHistory = trimmedHeaderStateHistory'
                 , mostRecentIntersection  = castPoint intersection
                 }
           where
             -- We trim the 'HeaderStateHistory' to the same size as our
             -- fragment so they keep in sync.
             trimmedHeaderStateHistory' =
               HeaderStateHistory.trim
                 (AF.length trimmedCandidateFrag)
                 theirHeaderStateHistory

      | otherwise ->
        -- No more intersection with the current chain
        return LostIntersection
  where
    ChainDbView {
        getCurrentChain
      } = cdbView
    KnownIntersectionState {
        theirFrag
      , theirHeaderStateHistory
      , ourFrag
      } = kis

{-------------------------------------------------------------------------------
  Acquiring a ledger view for RollForward validation
-------------------------------------------------------------------------------}

data LedgerViewCheck blk =
    -- | The upstream chain no longer intersects with our current chain because
    -- our current chain changed in the background.
    NoLongerIntersects
    -- | The upstream chain still intersects with our chain, return the latest
    -- 'KnownIntersectionState' and the 'LedgerView' necessary to validate the
    -- RollForward header.
  | LedgerViewAcquired
      (KnownIntersectionState blk)
      (Ticked (LedgerView (BlockProtocol blk)))

-- | The peer has sent a RollForward header but their chain is not dense enough
-- for us to validate the header in the usual way (via forecasting). We'll have
-- to pre-emptively fetch some of their blocks in order to do so.
--
-- This is the state type for the logic requests fetchs and processes the
-- fetched blocks.
data LowDensity blk
  = LowDensity
      !(KnownIntersectionState blk)
      -- ^ The 'mostRecentIntersection' via which we obtain the
      -- initially-unadvanced 'LedgerState' (whose 'LedgerView' forecast did
      -- not reach the slot of the RollForward header).
      !(LedgerState blk)
      -- ^ The advanced ledger state so far.
      ![RealPoint blk]
      -- ^ The remaining blocks on the peer's chain to fetch and then apply in
      -- order to further advance this ledger state.
      !RequestStatus
      -- ^ Whether the candidate fragment is certainly set to the next point.
  deriving (Generic)
  deriving anyclass (NoThunks)

data RequestStatus = RequestReady | RequestStale
  deriving (Generic, Eq)
  deriving anyclass (NoThunks)

-- | Attempt to forecast the necessary ledger view from the
-- 'mostRecentIntersection'. If that doesn't work, enter the supplied
-- 'LowDensity' handler.
checkDensityOrContinue :: forall blk m.
     (MonadSTM m, LedgerSupportsProtocol blk)
  => TopLevelConfig blk
  -> ChainDbView m blk
  -> SlotNo
     -- ^ forecast to this slot
  -> Stateful (STM m) (LowDensity blk) (m (LedgerViewCheck blk))
     -- ^ how to handle a low density chain
  -> Stateful (STM m) (KnownIntersectionState blk) (m (LedgerViewCheck blk))
checkDensityOrContinue cfg cdbView hdrSlot kont = Stateful $ \kis -> do
    let KnownIntersectionState {
            mostRecentIntersection
          , theirFrag
          } = kis

        theirSuffix :: AnchoredFragment (Header blk)
        theirSuffix =
            case AF.splitAfterPoint theirFrag mostRecentIntersection of
              Nothing              -> error "impossible!"
              Just (_upto, suffix) -> suffix

    st <- getIntersectionLedgerState cdbView kis
    case tryForecastLedgerView (Proxy @blk) lCfg st hdrSlot of
      Just v  -> return $ return $ LedgerViewAcquired kis v
      Nothing -> do
          -- If their suffix is so long that the normal ChainSync/BlockFetch
          -- logic should already be requesting it, then just wait for our
          -- chain to catch up, which will change our intersection point, which
          -- will abort this STM transaction.
          check $ AF.length theirSuffix <= fromIntegral k
          let toApply =
                map headerRealPoint $
                AF.toOldestFirst $
                theirSuffix
              ld      = LowDensity kis st toApply RequestStale
          continueWithState ld kont
  where
    SecurityParam k = protocolSecurityParam (configConsensus cfg)
    lCfg            = configLedger cfg

-- | Acquire the requisite 'LedgerView' despite the peer having a low density
-- chain.
--
-- TODO If we receive a RollForward that involves low density, and then the
-- next reply from the server (NB the RequestNext was already sent due to
-- pipelining) is a RollBackward, will we be able to process that, aborting our
-- now-expensive low density RollForward handler? We don't think so. Is that
-- OK? Seems suboptimal, but fine considering the narrow (disastrous)
-- circumstances of a low density chain.
lowDensityHandler :: forall blk m.
     (MonadSTM m, LedgerSupportsProtocol blk)
  => TopLevelConfig blk
  -> ChainDbView m blk
  -> (CandidateFragment (Header blk) -> m ())
  -> SlotNo
  -> Stateful (STM m) (LowDensity blk) (m (LedgerViewCheck blk))
lowDensityHandler cfg cdbView setCandidate hdrSlot =
    go1
  where
    ChainDbView {
        getBlock
      , getIsFetched
      } = cdbView

    lCfg = configLedger cfg

    go1 :: Stateful (STM m) (LowDensity blk) (m (LedgerViewCheck blk))
    go1 = Stateful worker

    -- NOTE This binding has its own where clause.
    worker ld = case toApply of

        -- We have fetched and applied all blocks between the intersection
        -- and the RollForward header.
        [] ->
            -- We can simply tick up to it, since there are no remaining
            -- blocks in between.
            return
              $ return
              $ LedgerViewAcquired kis
              $ protocolLedgerView lCfg   -- project without forecasting
              $ applyChainTick lCfg hdrSlot
              $ st

        -- Once the next block is fetched, advance the state and check if the
        -- density is still too low.
        rp:toApply' -> do
            let p = realPointToPoint rp
            -- TODO Will the blocks necessarily be in the restricted domain
            -- of ChainDB.getIsFetched? We may need to widen it, or maybe
            -- merely its current comment is too strong a statement.
            isFetched <- getIsFetched <*> pure p

            if

              | isFetched -> return $ advanceAndRetryForecast rp toApply'

              | RequestReady == reqStatus -> do
                -- Block this STM transaction until the next block is fetched.
                --
                -- TODO what if the upstream peer is unable to serve the
                -- block? Does BlockFetch silently ignore that? How could we
                -- notice here?
                --
                -- TODO separate but related, we need a timeout here. Maybe
                -- this will also handle the case where the peer actually can't
                -- provide the block after all.
                check isFetched
                return $ advanceAndRetryForecast rp toApply'

              | otherwise -> return $ do
                -- Tell BlockFetch to fetch the next block on the peer's chain
                -- regardless of their chain's plausibility.
                --
                -- NB All BlockFetch requests should be anchored at
                -- 'mostRecentIntersection'.
                setCandidate CandidateFragment
                  { candidateChain        =
                      -- We only request the next block. We'll only request the
                      -- block after that if this next block is still
                      -- insufficient. And so on. This doesn't allow
                      -- pipelining, but that's OK in disaster mode. This way
                      -- avoids unnecessary work.
                      maybe (error "impossible!") fst
                        $ AF.splitAfterPoint theirFrag p
                  , candidateIsLowDensity = True
                  }
                let ld' = LowDensity kis st toApply RequestReady
                continueWithState ld' go2

      where
        LowDensity kis st toApply reqStatus = ld

        KnownIntersectionState {
            theirFrag
          } = kis

        -- Get the block, advance the state, and try again to forecast.
        --
        -- PREREQUISITE: The block is fetched.
        advanceAndRetryForecast ::
          RealPoint blk -> [RealPoint blk] -> m (LedgerViewCheck blk)
        advanceAndRetryForecast rp toApply' = do
            -- TODO We don't think ChainDB GC will remove them. Explain why
            -- not.
            blk <- fromMaybe (error "impossible!") <$> getBlock rp
            let st' = tickThenReapply lCfg blk st
            case tryForecastLedgerView (Proxy @blk) lCfg st' hdrSlot of
              Just v  -> return $ LedgerViewAcquired kis v
              Nothing -> do
                  let ld' = LowDensity kis st' toApply' RequestStale
                  continueWithState ld' go2

    -- Restart the STM transaction.
    --
    -- Note that this STM transaction is sensitive simultaneously to both the
    -- current chain ('stillIntersectsWithCurrentChain') and also which of the
    -- peer's blocks have been fetched (via 'go1').
    go2 :: Stateful m (LowDensity blk) (LedgerViewCheck blk)
    go2 = Stateful $ \ld -> join $ atomically $ do
        let LowDensity kis _st _toApply _reqStatus = ld
        stillIntersectsWithCurrentChain cfg cdbView kis >>= \case
          LostIntersection     -> return $ return NoLongerIntersects
          NewIntersection kis' ->
              -- We throw away our advance 'LedgerState' and resume from that
              -- of the new intersection. We could check if we can carry on
              -- from where we are, but it doesn't seem worth the extra
              -- complexity.
              --
              -- Note that, if we could have kept the old state, the loop will
              -- most likely catch up to where it was without having to refetch
              -- any blocks.
              continueWithState kis' $
                checkDensityOrContinue cfg cdbView hdrSlot go1
          SameIntersection     -> continueWithState ld go1

{-------------------------------------------------------------------------------
  Local abbreviations
-------------------------------------------------------------------------------}

attemptRollback ::
     ( BlockSupportsProtocol blk
     , HasAnnTip blk
     )
  => Point blk
  -> (AnchoredFragment (Header blk), HeaderStateHistory blk)
  -> Maybe (AnchoredFragment (Header blk), HeaderStateHistory blk)
attemptRollback rollBackPoint (frag, state) = do
    frag'  <- AF.rollback (castPoint rollBackPoint) frag
    state' <- HeaderStateHistory.rewind rollBackPoint state
    return (frag', state')

-- | Watch the invalid block checker function for changes (using its
-- fingerprint). Whenever it changes, i.e., a new invalid block is detected,
-- check whether the current candidate fragment contains any header that is
-- invalid, if so, disconnect by throwing an 'InvalidBlock' exception.
--
-- Note that it is possible, yet unlikely, that the candidate fragment
-- contains a header that corresponds to an invalid block, but before we have
-- discovered this (after downloading and validating the block), the upstream
-- node could have rolled back such that its candidate chain no longer
-- contains the invalid block, in which case we do not disconnect from it.
--
-- The cost of this check is \( O(cand * check) \) where /cand/ is the size of
-- the candidate fragment and /check/ is the cost of checking whether a block
-- is invalid (typically \( O(\log(invalid)) \) where /invalid/ is the number
-- of invalid blocks).
invalidBlockRejector
    :: forall m blk.
       ( IOLike m
       , BlockSupportsProtocol blk
       , LedgerSupportsProtocol blk
       )
    => Tracer m (TraceChainSyncClientEvent blk)
    -> STM m (WithFingerprint (HeaderHash blk -> Maybe (InvalidBlockReason blk)))
       -- ^ Get the invalid block checker
    -> STM m (AnchoredFragment (Header blk))
    -> Watcher m
         (WithFingerprint (HeaderHash blk -> Maybe (InvalidBlockReason blk)))
         Fingerprint
invalidBlockRejector tracer getIsInvalidBlock getCandidate =
    Watcher {
        wFingerprint = getFingerprint
      , wInitial     = Nothing
      , wNotify      = checkInvalid . forgetFingerprint
      , wReader      = getIsInvalidBlock
      }
  where
    checkInvalid :: (HeaderHash blk -> Maybe (InvalidBlockReason blk)) -> m ()
    checkInvalid isInvalidBlock = do
      theirFrag <- atomically getCandidate
      -- The invalid block is likely to be a more recent block, so check from
      -- newest to oldest.
      mapM_ (uncurry disconnect) $ firstJust
        (\hdr -> (hdr,) <$> isInvalidBlock (headerHash hdr))
        (AF.toNewestFirst theirFrag)

    disconnect :: Header blk -> InvalidBlockReason blk -> m ()
    disconnect invalidHeader reason = do
      let ex = InvalidBlock (headerPoint invalidHeader) reason
      traceWith tracer $ TraceException ex
      throwIO ex

tryForecastLedgerView :: forall proxy blk.
     LedgerSupportsProtocol blk
  => proxy blk
  -> LedgerConfig blk
  -> LedgerState blk
  -> SlotNo
  -> Maybe (Ticked (LedgerView (BlockProtocol blk)))
tryForecastLedgerView _ cfg st s =
    case runExcept $ forecastFor forecast s of
      Left OutsideForecastRange{}  -> Nothing
      Right                      v -> Just v
  where
    forecast :: Forecast (LedgerView (BlockProtocol blk))
    forecast = ledgerViewForecastAt cfg st

getIntersectionLedgerState :: forall blk m.
     (MonadSTM m, LedgerSupportsProtocol blk)
  => ChainDbView m blk
  -> KnownIntersectionState blk
  -> STM m (LedgerState blk)
getIntersectionLedgerState cdbView kis = do
    getPastLedger mostRecentIntersection >>= \case
      Just st ->
          pure $ ledgerState st
      Nothing    ->
          error $
            "intersection not within last k blocks: "
            <> show mostRecentIntersection
  where
    ChainDbView {
        getPastLedger
      } = cdbView
    KnownIntersectionState {
        mostRecentIntersection
      } = kis

{-------------------------------------------------------------------------------
  Explicit state
-------------------------------------------------------------------------------}

-- | Make the state maintained by the chain sync client explicit
--
-- The chain sync client contains of a bunch of functions that basically look
-- like "do some network stuff, compute some stuff, and then continue with
-- such-and-such a new state". We want to make sure to keep that state in NF
-- at all times, but since we don't use a TVar to store it, we cannot reuse
-- the existing infrastructure for checking TVars for NF. Instead, we make
-- the state explicit in the types and do the check in 'continueWithState'.
newtype Stateful m s r = Stateful (s -> m r)

type StatefulConsensus m blk s st = Stateful m s (Consensus st blk m)

-- | See 'Stateful'.
continueWithState :: forall m s r.
     NoThunks s
  => s -> Stateful m s r -> m r
continueWithState !s (Stateful f) =
    checkInvariant (show <$> unsafeNoThunks s) $ f s

{-------------------------------------------------------------------------------
  Return value
-------------------------------------------------------------------------------}

-- | The Chain sync client only _gracefully_ terminates when the upstream node's
-- chain is not interesting (e.g., forked off too far in the past). By
-- gracefully terminating, the network layer can keep the other mini-protocols
-- connect to the same upstream node running.
--
-- For example, a relay node will often receive connections from nodes syncing
-- from scratch or an old chain. Since these nodes have a chain that is shorter
-- than the relay node's chain, it's useless for the relay node to run the
-- client-side of the chain sync protocol. However, the other direction of the
-- protocol, and, e.g., the transaction submission protocol, should keep
-- running.
data ChainSyncClientResult =
      -- | The server we're connecting to forked more than @k@ blocks ago.
      forall blk. BlockSupportsProtocol blk =>
        ForkTooDeep
          (Point blk)  -- ^ Intersection
          (Our   (Tip blk))
          (Their (Tip blk))

      -- | Our chain changed such that it no longer intersects with the
      -- candidate's fragment, and asking for a new intersection did not yield
      -- one.
    | forall blk. BlockSupportsProtocol blk =>
        NoMoreIntersection
          (Our   (Tip blk))
          (Their (Tip blk))

      -- | We were asked to roll back past the anchor point of the candidate's
      -- fragment. This means the candidate chain no longer forks off within
      -- @k@, making it impossible to switch to.
    | forall blk. BlockSupportsProtocol blk =>
        RolledBackPastIntersection
          (Point blk)  -- ^ Point asked to roll back to
          (Our   (Tip blk))
          (Their (Tip blk))

      -- | We were asked to terminate via the 'ControlMessageSTM'
    | AskedToTerminate

deriving instance Show ChainSyncClientResult

instance Eq ChainSyncClientResult where
  ForkTooDeep (a :: Point blk) b c == ForkTooDeep (a' :: Point blk') b' c' =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> (a, b, c) == (a', b', c')
  ForkTooDeep{} == _ = False

  NoMoreIntersection (a :: Our (Tip blk)) b == NoMoreIntersection (a' :: Our (Tip blk')) b' =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> (a, b) == (a', b')
  NoMoreIntersection{} == _ = False

  RolledBackPastIntersection (a :: Point blk) b c == RolledBackPastIntersection (a' :: Point blk') b' c' =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> (a, b, c) == (a', b', c')
  RolledBackPastIntersection{} == _ = False

  AskedToTerminate == AskedToTerminate = True
  AskedToTerminate == _ = False

{-------------------------------------------------------------------------------
  Exception
-------------------------------------------------------------------------------}

-- | When the upstream node violates the protocol or exhibits malicious
-- behaviour, e.g., serving an invalid header or a header corresponding to a
-- known invalid block, we throw an exception to disconnect. This will bring
-- down all miniprotocols in both directions with that node.
data ChainSyncClientException =
      -- | Header validation threw an error.
      forall blk. (BlockSupportsProtocol blk, ValidateEnvelope blk) =>
        HeaderError
          (Point blk)  -- ^ Invalid header
          (HeaderError blk)
          (Our   (Tip blk))
          (Their (Tip blk))

      -- | We send the upstream node a bunch of points from a chain fragment and
      -- the upstream node responded with an intersection point that is not on
      -- our chain fragment, and thus not among the points we sent.
      --
      -- We store the intersection point the upstream node sent us.
    | forall blk. BlockSupportsProtocol blk =>
        InvalidIntersection
          (Point blk)  -- ^ Intersection
          (Our   (Tip blk))
          (Their (Tip blk))

      -- | The received header to roll forward doesn't fit onto the previous
      -- one.
      --
      -- The first 'ChainHash' is the previous hash of the received header and
      -- the second 'ChainHash' is that of the previous one.
    | forall blk. BlockSupportsProtocol blk =>
        DoesntFit
          (ChainHash blk)  -- ^ Received hash
          (ChainHash blk)  -- ^ Expected hash
          (Our   (Tip blk))
          (Their (Tip blk))

      -- | The upstream node's chain contained a block that we know is invalid.
    | forall blk. LedgerSupportsProtocol blk =>
        InvalidBlock
          (Point blk)  -- ^ Invalid block
          (InvalidBlockReason blk)

deriving instance Show ChainSyncClientException

instance Eq ChainSyncClientException where
  HeaderError (a :: Point blk) b c d == HeaderError (a' :: Point blk') b' c' d' =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> (a, b, c, d) == (a', b', c', d')
  HeaderError{} == _ = False

  InvalidIntersection (a :: Point blk) b c == InvalidIntersection (a' :: Point blk') b' c' =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> (a, b, c) == (a', b', c')
  InvalidIntersection{} == _ = False

  DoesntFit (a :: ChainHash blk) b c d == DoesntFit (a' :: ChainHash blk') b' c' d' =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> (a, b, c, d) == (a', b', c', d')
  DoesntFit{} == _ = False

  InvalidBlock (a :: Point blk) b == InvalidBlock (a' :: Point blk') b' =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> (a, b) == (a', b')
  InvalidBlock{} == _ = False

instance Exception ChainSyncClientException

{-------------------------------------------------------------------------------
  TODO #221: Implement genesis

  Genesis in paper:

    When we compare a candidate to our own chain, and that candidate forks off
    more than k in the past, we compute the intersection point between that
    candidate and our chain, select s slots from both chains, and compare the
    number of blocks within those s slots. If the candidate has more blocks
    in those s slots, we prefer the candidate, otherwise we stick with our own
    chain.

  Genesis as we will implement it:

    * We decide we are in genesis mode if the head of our chain is more than
      @k@ blocks behind the blockchain time. We will have to approximate this
      as @k/f@ /slots/ behind the blockchain time time.
    * In this situation, we must make sure we have a sufficient number of
      upstream nodes "and collect chains from all of them"
    * We still never consider chains that would require /us/ to rollback more
      than k blocks.
    * In order to compare two candidates, we compute the intersection point of
      X of those two candidates and compare the density at point X.




  Scribbled notes during meeting with Duncan:

   geensis mode: compare clock to our chain
   do we have enough peers?
   still only interested in chains that don't fork more than k from our own chain

     downloading headers from a /single/ node, download at least s headers
     inform /other/ peers: "here is a point on our chain"
     if all agree ("intersection imporved") -- all peers agree
     avoid downloading tons of headers
     /if/ there is a difference, get s headers from the peer who disagrees,
       pick the denser one, and ignore the other
       PROBLEM: what if the denser node has invalid block bodies??
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Chain Sync Client.
data TraceChainSyncClientEvent blk
  = TraceDownloadedHeader (Header blk)
    -- ^ While following a candidate chain, we rolled forward by downloading a
    -- header.
  | TraceRolledBack (Point blk)
    -- ^ While following a candidate chain, we rolled back to the given point.
  | TraceFoundIntersection (Point blk) (Our (Tip blk)) (Their (Tip blk))
    -- ^ We found an intersection between our chain fragment and the
    -- candidate's chain.
  | TraceException ChainSyncClientException
    -- ^ An exception was thrown by the Chain Sync Client.
  | TraceTermination ChainSyncClientResult
    -- ^ The client has terminated.

deriving instance ( BlockSupportsProtocol blk
                  , Eq (ValidationErr (BlockProtocol blk))
                  , Eq (Header blk)
                  )
               => Eq   (TraceChainSyncClientEvent blk)
deriving instance ( BlockSupportsProtocol blk
                  , Show (Header blk)
                  )
               => Show (TraceChainSyncClientEvent blk)
