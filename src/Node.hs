{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Node where

import Data.List
import Data.Graph
import Data.Semigroup (Semigroup (..))
import Data.Maybe (listToMaybe, catMaybes)
import Data.Functor (($>))
import Data.Tuple (swap)
import Control.Applicative
import Control.Monad
import Control.Exception (evaluate)

import MonadClass hiding (sendMsg, recvMsg)
import Block
import qualified Chain
import           Chain (Chain (..), Point)
import Protocol
import ConsumersAndProducers
import ChainProducerState (ChainProducerState (..), ReaderId, initChainProducerState, switchFork)

import qualified Sim

import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC (assert, monadicIO, run)


longestChainSelection :: forall block m stm. MonadSTM m stm
                      => [TVar m (Maybe (Chain block))]
                      -> TVar m (Chain block)
                      -> m ()
longestChainSelection candidateChainVars currentChainVar =
    forever (atomically updateCurrentChain)
  where
    updateCurrentChain :: stm ()
    updateCurrentChain = do
      candidateChains <- mapM readTVar candidateChainVars
      currentChain    <- readTVar currentChainVar
      case longestChain (Chain.length currentChain) (catMaybes candidateChains) of
        Nothing -> retry
        Just c  -> writeTVar currentChainVar c

    longestChain :: Int -> [(Chain block)] -> Maybe (Chain block)
    longestChain curlen
      = fmap fst
      . listToMaybe
      . sortBy (\(_, l1) (_, l2) -> compare l1 l2)
      . filter (\(_, l) -> l > curlen)
      . map (\c -> (c, Chain.length c))

-- |
-- State-full chain selection (@'ChainProducerState'@).
longestChainSelectionS :: forall block m stm.
                          ( HasHeader block
                          , MonadSTM m stm
                          )
                      => [TVar m (Maybe (Chain block))]
                      -> TVar m (ChainProducerState block)
                      -> m ()
longestChainSelectionS candidateChainVars cpsVar =
    forever (atomically updateCurrentChain)
  where
    updateCurrentChain :: stm ()
    updateCurrentChain = do
      candidateChains <- mapM readTVar candidateChainVars
      cps@ChainProducerState{chainState} <- readTVar cpsVar
      case longestChain (Chain.length chainState) (catMaybes candidateChains) of
        Nothing -> retry
        Just c  -> writeTVar cpsVar (switchFork c cps)

    longestChain :: Int -> [(Chain block)] -> Maybe (Chain block)
    longestChain curlen
      = fmap fst
      . listToMaybe
      . sortBy (\(_, l1) (_, l2) -> compare l1 l2)
      . filter (\(_, l) -> l > curlen)
      . map (\c -> (c, Chain.length c))


chainValidation :: forall block m stm. (HasHeader block, MonadSTM m stm)
                => TVar m (Chain block)
                -> TVar m (Maybe (Chain block))
                -> m ()
chainValidation peerChainVar candidateChainVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (atomically (update st))
  where
    update :: TVar m Point -> stm ()
    update stateVar = do
      peerChain      <- readTVar peerChainVar
      candidatePoint <- readTVar stateVar
      check (Chain.headPoint peerChain /= candidatePoint)
      writeTVar stateVar (Chain.headPoint peerChain)
      let candidateChain | Chain.valid peerChain = Just peerChain
                         | otherwise             = Nothing
      writeTVar candidateChainVar (candidateChain)


chainTransferProtocol :: forall block m stm.
                         ( HasHeader block
                         , MonadSTM m stm
                         , MonadTimer m
                         )
                      => Duration (Time m)
                      -> TVar m (Chain block)
                      -> TVar m (Chain block)
                      -> m ()
chainTransferProtocol delay inputVar outputVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m Point -> m ()
    update stateVar = do
      input <- atomically $ do
                input    <- readTVar inputVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint input /= curPoint)
                writeTVar stateVar (Chain.headPoint input)
                return input
      timer delay $ atomically $ writeTVar outputVar input


data Chan m send recv = Chan { sendMsg :: send -> m ()
                             , recvMsg :: m recv
                             }


transferProtocol :: forall send recv m stm.
                    ( MonadSTM m stm
                    , MonadTimer m
                    )
                 => Duration (Time m)
                 -> TVar m [send]
                 -> TVar m [recv]
                 -> Chan m send recv
transferProtocol delay sendVar recvVar
  = Chan sendMsg recvMsg
  where
    sendMsg a = timer delay $ atomically $ modifyTVar' sendVar (++[a])
    recvMsg =
      atomically $ do
        xs <- readTVar recvVar
        case xs of
          x : xs' -> writeTVar recvVar xs' $> x
          []      -> retry


createCoupledChannels :: forall send recv m stm.
                         ( MonadSTM m stm
                         , MonadTimer m
                         )
                      => Duration (Time m)
                      -> Duration (Time m)
                      -> m (Chan m send recv, Chan m recv send)
createCoupledChannels delay1 delay2 = do
  sendVar <- atomically $ newTVar []
  recvVar <- atomically $ newTVar []
  let chan1 = transferProtocol delay1 sendVar recvVar
      chan2 = transferProtocol delay2 recvVar sendVar
  return (chan1, chan2)


data NodeChannels m prodMsg consMsg = NodeChannels
  { consumerChans :: [Chan m consMsg prodMsg]
    -- ^ channels on which the node will play the consumer role:
    -- sending @consMsg@ and receiving @prodMsg@ messages.
  , producerChans :: [Chan m prodMsg consMsg]
    -- ^ channels on which the node will play the producer role:
    -- sending @prodMsg@ and receiving @consMsg@ messages.
  }

instance Semigroup (NodeChannels m prodMsg consMsg) where
  NodeChannels c1 p1 <> NodeChannels c2 p2 = NodeChannels (c1 ++ c2) (p1 ++ p2)

instance Monoid (NodeChannels m prodMsg consMsg) where
  mempty = NodeChannels [] []

  mappend = (<>)


-- |
-- Create a channels n1 → n2, where n1 is a producer and n2 is the consumer.
createOneWaySubscriptionChannels
  :: forall send recv m stm.
     ( MonadSTM m stm
     , MonadTimer m
     )
  => Duration (Time m)
  -> Duration (Time m)
  -> m (NodeChannels m send recv, NodeChannels m send recv)
createOneWaySubscriptionChannels trDelay1 trDelay2 = do
  (cr, rc) <- createCoupledChannels trDelay1 trDelay2
  return
    ( NodeChannels
        { consumerChans = []
        , producerChans = [cr]
        }
    , NodeChannels
        { consumerChans = [rc]
        , producerChans = []
        }
    )

-- |
-- Create channels for n1 ↔ n2 where both nodes are a consumer and a producer
-- simulantously.
createTwoWaySubscriptionChannels
  :: forall send recv m stm.
     ( MonadSTM m stm
     , MonadTimer m
     )
  => Duration (Time m)
  -> Duration (Time m)
  -> m (NodeChannels m send recv, NodeChannels m send recv)
createTwoWaySubscriptionChannels trDelay1 trDelay2 = do
  r12 <- createOneWaySubscriptionChannels trDelay1 trDelay2
  r21 <- createOneWaySubscriptionChannels trDelay2 trDelay1
  return $ r12 <> swap r21

chainGenerator :: forall block m stm.
                  ( MonadSTM m stm
                  , MonadTimer m
                  )
               => Duration (Time m)
               -> Chain block
               -> m (TVar m (Chain block))
chainGenerator offset chain = do
    outputVar <- atomically (newTVar Genesis)
    sequence_ [ timer (offset + fromIntegral n) (atomically (writeTVar outputVar v))
              | (v, n) <- zip (inits chain) [0,2..] ]
    return outputVar
  where
    inits = reverse
          . unfoldr (\c -> case c of
                              Genesis -> Nothing
                              _       -> Just (c, Chain.drop 1 c))


-- |
-- Generate block from a given chain.
blockGenerator :: forall block m stm.
                  ( MonadSTM m stm
                  , MonadTimer m
                  )
               => Duration (Time m)
               -> Chain block
               -> m (TVar m (Maybe block))  -- returns an stm transaction which returns block
blockGenerator offset chain = do
  outputVar <- atomically (newTVar Nothing)
  sequence_ [ timer (offset + fromIntegral n) (atomically (writeTVar outputVar (Just b)))
            | (b, n) <- zip (reverse $ Chain.toList chain) [0,2..] ]
  return outputVar

-- |
-- Read one block from the @'blockGenertor'@
getBlock :: forall block m stm.
            MonadSTM m stm
         => TVar m (Maybe block)
         -> stm block
getBlock v = do
  mb <- readTVar v
  case mb of
    Nothing -> retry
    Just b  -> writeTVar v Nothing $> b


observeChain :: forall block m stm.
                ( HasHeader block
                , MonadSTM m stm
                , MonadSay m
                )
             => String
             -> TVar m (Chain block)
             -> m ()
observeChain labelPrefix chainVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m Point -> m ()
    update stateVar = do
      chain <- atomically $ do
                chain    <- readTVar chainVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint chain /= curPoint)
                writeTVar stateVar (Chain.headPoint chain)
                return chain
      say (labelPrefix ++ ": " ++ show (Chain.length chain, Chain.headPoint chain))


observeChainProducerState
  :: forall block m stm.
     ( HasHeader block
     , MonadSTM m stm
     , MonadSay m
     )
  => NodeId
  -> TVar m (ChainProducerState block)
  -> m ()
observeChainProducerState nid cpsVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m Point -> m ()
    update stateVar = do
      chain <- atomically $ do
                ChainProducerState{chainState = chain} <- readTVar cpsVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint chain /= curPoint)
                writeTVar stateVar (Chain.headPoint chain)
                return chain
      say (show nid ++ ": " ++ show (Chain.length chain, Chain.headPoint chain))

nodeExample1 :: forall block m stm.
                ( HasHeader block
                , MonadSTM m stm
                , MonadTimer m
                , MonadSay m
                )
             => (Chain block)
             -> (Chain block)
             -> m ()
nodeExample1 c1 c2 = do
    generatorVar1 <- chainGenerator 1 c1
    generatorVar2 <- chainGenerator 2 c2

    peerChainVar1 <- atomically $ newTVar Genesis
    peerChainVar2 <- atomically $ newTVar Genesis

    candidateChainVar1 <- atomically $ newTVar Nothing
    candidateChainVar2 <- atomically $ newTVar Nothing
    let candidateChainVars = [candidateChainVar1, candidateChainVar2]

    currentChainVar <- atomically $ newTVar Genesis

    fork $ chainTransferProtocol 1 generatorVar1 peerChainVar1
    fork $ chainTransferProtocol 2 generatorVar2 peerChainVar2

    fork $ chainValidation peerChainVar1 candidateChainVar1
    fork $ chainValidation peerChainVar2 candidateChainVar2

    fork $ longestChainSelection candidateChainVars currentChainVar

    fork $ observeChain "generatorVar1" generatorVar1
    fork $ observeChain "generatorVar2" generatorVar2
    fork $ observeChain "currentChain " currentChainVar

data NodeId = CoreId Int
            | RelayId Int
  deriving (Eq, Ord, Show)

data ConsumerId = ConsumerId NodeId Int
  deriving (Eq, Ord, Show)

data ProducerId = ProducerId NodeId Int
  deriving (Eq, Ord, Show)

relayNode :: forall block m stm.
        ( HasHeader block
        , Eq block
        , Show block
        , MonadSTM m stm
        , MonadTimer m
        , MonadSay m
        )
     => NodeId
     -> NodeChannels m (MsgProducer block) MsgConsumer
     -> m (TVar m (ChainProducerState block))
relayNode nid chans = do

  -- Mutable state
  -- 1. input chains
  chainVars <- zipWithM startConsumer [0..] (consumerChans chans)
  -- 2. candidate chains
  candidateChainVars <- replicateM (length $ consumerChans chans) (atomically (newTVar Nothing))
  -- 3. ChainProducerState
  cpsVar <- atomically $ newTVar (initChainProducerState Genesis)

  -- chain validation threads
  zipWithM_
    (\chain cchain -> fork $ chainValidation chain cchain)
    chainVars
    candidateChainVars
  -- chain selection thread
  fork $ longestChainSelectionS candidateChainVars cpsVar

  -- producers which share @'ChainProducerState'@
  let producer = exampleProducer cpsVar
  mapM_ (uncurry $ startProducer producer) (zip [0..] (producerChans chans))

  -- chain observer
  fork $ observeChainProducerState nid cpsVar

  return cpsVar
  where
    startConsumer :: Int
                  -> Chan m MsgConsumer (MsgProducer block)
                  -> m (TVar m (Chain block))
    startConsumer cid chan = do
      chainVar <- atomically $ newTVar Genesis
      let consumer = exampleConsumer chainVar
      fork $ consumerSideProtocol1 consumer
        (loggingSend (ConsumerId nid cid) (sendMsg chan))
        (loggingRecv (ConsumerId nid cid) (recvMsg chan))
      return chainVar

    startProducer :: ProducerHandlers block m ReaderId
                  -> Int
                  -> Chan m (MsgProducer block) MsgConsumer
                  -> m ()
    startProducer producer pid chan = do
      fork $ producerSideProtocol1 producer
        (loggingSend (ProducerId nid pid) (sendMsg chan))
        (loggingRecv (ProducerId nid pid) (recvMsg chan))

coreNode :: forall m stm.
        ( MonadSTM m stm
        , MonadTimer m
        , MonadSay m
        )
     => NodeId
     -> Duration (Time m)
     -> Chain Block
     -> NodeChannels m (MsgProducer Block) MsgConsumer
     -> m ()
coreNode nid offset chain chans = do
  cpsVar <- relayNode nid chans

  blockVar <- blockGenerator offset chain
  fork $ forever $ applyGeneratedBlock blockVar cpsVar

  where
    applyGeneratedBlock
      :: TVar m (Maybe Block)
      -> TVar m (ChainProducerState Block)
      -> m ()
    applyGeneratedBlock blockVar cpsVar = atomically $ do
      block <- getBlock blockVar
      cps@ChainProducerState{chainState} <- readTVar cpsVar
      let chain = Chain.addBlock (Chain.fixupBlock (Chain.headPoint chainState) (Chain.headBlockNo chainState) block) chainState
      writeTVar cpsVar (switchFork chain cps)

coreToRelaySim :: ( MonadSTM m stm
                  , MonadTimer m
                  , MonadSay m
                  )
               => Bool              -- ^ two way subscription
               -> Chain Block
               -> Duration (Time m) -- ^ core delay (block creation)
               -> Duration (Time m) -- ^ core transport delay
               -> Duration (Time m) -- ^ relay transport delay
               -> m ()
coreToRelaySim duplex chain coreDelay coreTrDelay relayTrDelay = do
  (coreChans, relayChans) <- if duplex
    then createTwoWaySubscriptionChannels relayTrDelay coreTrDelay
    else createOneWaySubscriptionChannels coreTrDelay relayTrDelay

  fork $ coreNode (CoreId 0) coreDelay chain coreChans
  fork $ void $ relayNode (RelayId 1) relayChans

runCoreToRelaySim :: Chain Block
                  -> Sim.VTimeDuration
                  -> Sim.VTimeDuration
                  -> Sim.VTimeDuration
                  -> Sim.Trace
runCoreToRelaySim chain coreDelay coreTransportDelay relayTransportDelay =
  Sim.runSimM $ coreToRelaySim False chain coreDelay coreTransportDelay relayTransportDelay


-- Node graph: c → r → r
coreToRelaySim2 :: ( MonadSTM m stm
                   , MonadTimer m
                   , MonadSay m
                   )
                => Chain Block
                -> Duration (Time m)
                -> Duration (Time m)
                -> Duration (Time m)
                -> m ()
coreToRelaySim2 chain coreDelay coreTrDelay relayTrDelay = do
  (cr1, r1c) <- createOneWaySubscriptionChannels coreTrDelay relayTrDelay
  (r1r2, r2r1) <- createOneWaySubscriptionChannels relayTrDelay relayTrDelay

  fork $ coreNode (CoreId 0) coreDelay chain cr1
  fork $ void $ relayNode (RelayId 1) (r1c <> r1r2)
  fork $ void $ relayNode (RelayId 2) r2r1

runCoreToRelaySim2 :: Chain Block
                   -> Sim.VTimeDuration
                   -> Sim.VTimeDuration
                   -> Sim.VTimeDuration
                   -> Sim.Trace
runCoreToRelaySim2 chain coreDelay coreTransportDelay relayTransportDelay =
  Sim.runSimM $ coreToRelaySim2 chain coreDelay coreTransportDelay relayTransportDelay


-- Node graph: c ↔ r ↔ r ↔ c
coreToCoreViaRelaySim :: ( MonadSTM m stm
                         , MonadTimer m
                         , MonadSay m
                         )
                      => Chain Block
                      -> Chain Block
                      -> Duration (Time m)
                      -> Duration (Time m)
                      -> Duration (Time m)
                      -> m ()
coreToCoreViaRelaySim chain1 chain2 coreDelay coreTrDelay relayTrDelay = do
  (c1r1, r1c1) <- createTwoWaySubscriptionChannels coreTrDelay relayTrDelay
  (r1r2, r2r1) <- createTwoWaySubscriptionChannels relayTrDelay relayTrDelay
  (r2c2, c2r2) <- createTwoWaySubscriptionChannels relayTrDelay coreTrDelay
  -- (r1c2, c2r1) <- createTwoWaySubscriptionChannels relayTrDelay coreTrDelay

  fork $ coreNode (CoreId 1) coreDelay chain1 c1r1
  fork $ void $ relayNode (RelayId 1) (r1c1 <> r1r2)
  fork $ void $ relayNode (RelayId 2) (r2r1 <> r2c2)
  fork $ coreNode (CoreId 2) coreDelay chain2 c2r2
  -- fork $ void $ relayNode (RelayId 1) (r1c1 <> r1c2)
  -- fork $ coreNode (CoreId 2) coreDelay chain2 c2r1

runCoreToCoreViaRelaySim
  :: Chain Block
  -> Chain Block
  -> Sim.VTimeDuration
  -> Sim.VTimeDuration
  -> Sim.VTimeDuration
  -> Sim.Trace
runCoreToCoreViaRelaySim chain1 chain2 coreDelay coreTrDelay relayTrDelay =
  Sim.runSimM $ coreToCoreViaRelaySim chain1 chain2 coreDelay coreTrDelay relayTrDelay


prop_coreToCoreViaRelay :: Chain.TestChainFork -> Property
prop_coreToCoreViaRelay (Chain.TestChainFork _ chain1 chain2) =
  let trace = runCoreToCoreViaRelaySim chain1 chain2 (Sim.VTimeDuration 1) (Sim.VTimeDuration 1) (Sim.VTimeDuration 1)
  in QC.monadicIO $ do
    QC.run (evaluate trace) >>= QC.assert . (\tr -> length tr > 0)
