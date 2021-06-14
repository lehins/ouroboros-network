{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuantifiedConstraints #-}


-- This is already implied by the -Wall in the .cabal file, but lets just be
-- completely explicit about it too, since we rely on the completeness
-- checking in the cases below for the completeness of our proofs.
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Proofs about the typed protocol framework.
--
-- It also provides helpful testing utilities.
--
module Network.TypedProtocol.Proofs (
  -- * About these proofs
  -- $about

  -- * Connect proof
  connect,
  TerminalStates(..),

  -- * Pipelining proofs
  -- | Additional proofs specific to the pipelining features
  connectPipelined,
  forgetPipelined,

  -- ** Pipeline proof helpers
  Queue(..),
  enqueue,

  -- ** Auxilary functions
  pipelineInterleaving,
  ) where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Data.Void (absurd)
import Data.Singletons

-- $about
--
-- Typed languages such as Haskell can embed proofs. In total languages this
-- is straightforward: a value inhabiting a type is a proof of the property
-- corresponding to the type.
--
-- In languages like Haskell that have ⊥ as a value of every type, things
-- are slightly more complicated. We have to demonstrate that the value that
-- inhabits the type of interest is not ⊥ which we can do by evaluation.
--
-- This idea crops up frequently in advanced type level programming in Haskell.
-- For example @Refl@ proofs that two types are equal have to have a runtime
-- representation that is evaluated to demonstrate it is not ⊥ before it
-- can be relied upon.
--
-- The proofs here are about the nature of typed protocols in this framework.
-- The 'connect' and 'connectPipelined' proofs rely on a few lemmas about
-- the individual protocol. See 'AgencyProofs'.




-- | The 'connect' function takes two peers that agree on a protocol and runs
-- them in lock step, until (and if) they complete.
--
-- The 'connect' function serves a few purposes.
--
-- * The fact we can define this function at at all proves some minimal
-- sanity property of the typed protocol framework.
--
-- * It demonstrates that all protocols defined in the framework can be run
-- with synchronous communication rather than requiring buffered communication.
--
-- * It is useful for testing peer implementations against each other in a
-- minimalistic setting.
--
connect :: forall ps (pr :: PeerRole) (st :: ps) m a b.
           ( Monad m
           )
        => TokPeerRole pr
        -> Peer ps pr st m a
        -> Peer ps (FlipAgency pr) st m b
        -> m (a, b, TerminalStates ps pr)
connect tokPeerRole = go
  where
    go :: forall (st' :: ps).
          Peer ps             pr  st' m a
       -> Peer ps (FlipAgency pr) st' m b
       -> m (a, b, TerminalStates ps pr)
    go (Done reflA a)  (Done reflB b)  = return (a, b, terminals)
      where
        terminals :: TerminalStates ps pr
        terminals = TerminalStates (sing :: Sing st') reflA reflB

    go (Effect a )     b               = a >>= \a' -> go a' b
    go  a              (Effect b)      = b >>= \b' -> go a  b'
    go (Yield _ msg a) (Await _ b)     = go  a     (b msg)
    go (Await _ a)     (Yield _ msg b) = go (a msg) b

    -- By appealing to the proofs about agency for this protocol we can
    -- show that these other cases are impossible
    go (Yield reflA _ _) (Yield reflB _ _) =
      absurd $
        exclusionLemmaClientAndServerHaveAgency_1 tokPeerRole reflA reflB

    go (Await reflA _)   (Await reflB _)   =
      absurd $
        exclusionLemmaClientAndServerHaveAgency_2 tokPeerRole reflA reflB

    go (Done  reflA _) (Yield reflB _ _)   =
      absurd $
        exclusionLemmaWeHaveAgencyAndNobodyHasAgency_2 tokPeerRole reflB reflA

    go (Done  reflA _) (Await reflB _)     =
      absurd $
        exclusionLemmaTheyHaveAgencyAndNobodyHasAgency_2 tokPeerRole reflB reflA

    go (Yield reflA _ _) (Done reflB _)    =
      absurd $
        exclusionLemmaWeHaveAgencyAndNobodyHasAgency_1 tokPeerRole reflA reflB

    go (Await reflA _)   (Done reflB _)    =
      absurd $
        exclusionLemmaTheyHaveAgencyAndNobodyHasAgency_1 tokPeerRole reflA reflB


-- | The terminal states for the protocol. Used in 'connect' and
-- 'connectPipelined' to return the states in which the peers terminated.
--
data TerminalStates ps (pr :: PeerRole) where
     TerminalStates
       :: forall ps (pr :: PeerRole) (st :: ps).
          Sing st
       -> RelativeAgencyEq (StateAgency st)
                            NobodyHasAgency
                           (Relative             pr  (StateAgency st))
       -> RelativeAgencyEq (StateAgency st)
                            NobodyHasAgency
                           (Relative (FlipAgency pr) (StateAgency st))
       -> TerminalStates ps pr


-- | Analogous to 'connect' but for pipelined peers.
--
-- Since pipelining allows multiple possible interleavings, we provide a
-- @[Bool]@ parameter to control the choices. Each @True@ will trigger picking
-- the first choice in the @SenderCollect@ construct (if possible), leading
-- to more results outstanding. This can also be interpreted as a greater
-- pipeline depth, or more messages in-flight.
--
-- This can be exercised using a QuickCheck style generator.
--
connectPipelined :: forall ps (pr :: PeerRole) (st :: ps) m a b.
                    Monad m
                 => TokPeerRole pr
                 -> [Bool] -- ^ Interleaving choices. [] gives no pipelining.
                 -> PeerPipelined ps pr st m a
                 -> Peer          ps (FlipAgency pr) st m b
                 -> m (a, b, TerminalStates ps pr)

connectPipelined tokPeerRole cs0 (PeerPipelined peerA) peerB =
    goSender cs0 EmptyQ peerA peerB
  where
    goSender :: forall (st' :: ps) n c.
                [Bool]
             -> Queue                      n c
             -> PeerSender ps pr st' n c m a
             -> Peer       ps (FlipAgency pr) st'     m b
             -> m (a, b, TerminalStates ps pr)

    goSender _ EmptyQ (SenderDone reflA a) (Done reflB b) = return (a, b, terminals)
      where
        terminals :: TerminalStates ps pr
        terminals = TerminalStates (sing :: Sing st') reflA reflB

    goSender cs q (SenderEffect a) b  = a >>= \a' -> goSender cs q a' b
    goSender cs q a        (Effect b) = b >>= \b' -> goSender cs q a  b'

    goSender cs q (SenderYield _ msg a) (Await _ b) = goSender cs q a (b msg)
    goSender cs q (SenderAwait _ a) (Yield _ msg b) = goSender cs q (a msg) b

    -- This does the receiver effects immediately, as if there were no
    -- pipelining.
    goSender cs q (SenderPipeline _ msg r a) (Await _ b) =
      goReceiver r (b msg) >>= \(b', x) -> goSender cs (enqueue x q) a b'

    -- However we make it possible to exercise the choice the environment has
    -- in the non-determinism of the pipeline interleaving of collecting
    -- results. Always picking the second continuation gives the fully serial
    -- order. Always picking the first leads to a maximal (and possibly
    -- unbounded) number of pending replies. By using a list of bools to
    -- control the choices here, we can test any other order:
    goSender (True:cs) q (SenderCollect (Just a) _) b = goSender cs q  a    b
    goSender (_:cs) (ConsQ x q) (SenderCollect _ a) b = goSender cs q (a x) b
    goSender    []  (ConsQ x q) (SenderCollect _ a) b = goSender [] q (a x) b

    -- Proofs that the remaining cases are impossible
    goSender _ _ (SenderDone reflA _) (Yield reflB _ _) =
      absurd $
        exclusionLemmaWeHaveAgencyAndNobodyHasAgency_2 tokPeerRole reflB reflA

    goSender _ _ (SenderDone reflA _) (Await reflB _) =
      absurd $
        exclusionLemmaTheyHaveAgencyAndNobodyHasAgency_2 tokPeerRole reflB reflA

    goSender _ _ (SenderYield reflA _ _) (Done reflB _) =
      absurd $
        exclusionLemmaWeHaveAgencyAndNobodyHasAgency_1 tokPeerRole reflA reflB

    goSender _ _ (SenderYield reflA _ _) (Yield reflB _ _) =
      absurd $
        exclusionLemmaClientAndServerHaveAgency_1 tokPeerRole reflA reflB

    goSender _ _ (SenderAwait reflA _) (Done reflB _) =
      absurd $
        exclusionLemmaTheyHaveAgencyAndNobodyHasAgency_1 tokPeerRole reflA reflB

    goSender _ _ (SenderAwait reflA _) (Await reflB _) =
      absurd $
        exclusionLemmaClientAndServerHaveAgency_2 tokPeerRole reflA reflB

    goSender _ _ (SenderPipeline reflA _ _ _) (Done reflB _) =
      absurd $
        exclusionLemmaWeHaveAgencyAndNobodyHasAgency_1 tokPeerRole reflA reflB

    goSender _ _ (SenderPipeline reflA _ _ _) (Yield reflB _ _) =
      absurd $
        exclusionLemmaClientAndServerHaveAgency_1 tokPeerRole reflA reflB


    goReceiver :: forall (st' :: ps) (stdone :: ps) c.
                  PeerReceiver ps pr st' stdone m c
               -> Peer         ps (FlipAgency pr) st'        m b
               -> m (Peer      ps (FlipAgency pr)     stdone m b, c)

    goReceiver (ReceiverDone x)    b         = return (b, x)
    goReceiver (ReceiverEffect a)  b         = a >>= \a' -> goReceiver a' b
    goReceiver  a                 (Effect b) = b >>= \b' -> goReceiver a  b'

    goReceiver (ReceiverAwait _ a) (Yield _ msg b) = goReceiver (a msg) b


    -- Proofs that the remaining cases are impossible
    goReceiver (ReceiverAwait reflA _) (Done  reflB _) =
      absurd $
        exclusionLemmaTheyHaveAgencyAndNobodyHasAgency_1 tokPeerRole reflA reflB

    goReceiver (ReceiverAwait reflA _) (Await reflB _) =
      absurd $
        exclusionLemmaClientAndServerHaveAgency_2 tokPeerRole reflA reflB


-- | Prove that we have a total conversion from pipelined peers to regular
-- peers. This is a sanity property that shows that pipelining did not give
-- us extra expressiveness or to break the protocol state machine.
--
forgetPipelined
  :: forall ps (pr :: PeerRole) (st :: ps) m a.
     Functor m
  => PeerPipelined ps pr st m a
  -> Peer          ps pr st m a
forgetPipelined (PeerPipelined peer) = goSender EmptyQ peer
  where
    goSender :: forall st' n c.
                Queue                n c
             -> PeerSender ps pr st' n c m a
             -> Peer       ps pr st'     m a
    goSender EmptyQ (SenderDone     refl     k) = Done refl k
    goSender q      (SenderEffect            k) = Effect (goSender q <$> k)
    goSender q      (SenderYield    refl m   k) = Yield refl m (goSender q k)
    goSender q      (SenderAwait    refl     k) = Await refl   (goSender q <$> k)
    goSender q      (SenderPipeline refl m r k) = Yield refl m (goReceiver q k r)
    goSender (ConsQ x q) (SenderCollect  _   k) = goSender q (k x)
    -- Here by picking the second continuation in Collect we resolve the
    -- non-determinism by always picking the fully in-order non-pipelined
    -- data flow path.

    goReceiver :: forall stCurrent stNext n c.
                  Queue                        n  c
               -> PeerSender   ps pr stNext (S n) c   m a
               -> PeerReceiver ps pr stCurrent stNext m c
               -> Peer         ps pr stCurrent        m a

    goReceiver q s (ReceiverDone       x) = goSender (enqueue x q) s
    goReceiver q s (ReceiverEffect     k) = Effect (goReceiver q s <$> k)
    goReceiver q s (ReceiverAwait refl k) = Await refl (goReceiver q s . k)

-- | A size indexed queue. This is useful for proofs, including
-- 'connectPipelined' but also as so-called @direct@ functions for running a
-- client and server wrapper directly against each other.
--
data Queue (n :: N) a where
  EmptyQ ::                   Queue  Z    a
  ConsQ  :: a -> Queue n a -> Queue (S n) a

-- | At an element to the end of a 'Queue'. This is not intended to be
-- efficient. It is only for proofs and tests.
--
enqueue :: a -> Queue n a -> Queue (S n) a
enqueue a  EmptyQ     = ConsQ a EmptyQ
enqueue a (ConsQ b q) = ConsQ b (enqueue a q)


-- | A reference specification for interleaving of requests and responses
-- with pipelining, where the environment can choose whether a response is
-- available yet.
--
-- This also supports bounded choice where the maximum number of outstanding
-- in-flight responses is limted.
--
pipelineInterleaving :: Int    -- ^ Bound on outstanding responses
                     -> [Bool] -- ^ Pipelining choices
                     -> [req] -> [resp] -> [Either req resp]
pipelineInterleaving omax cs0 reqs0 resps0 =
    go 0 cs0 (zip [0 :: Int ..] reqs0)
             (zip [0 :: Int ..] resps0)
  where
    go o (c:cs) reqs@((reqNo, req) :reqs')
               resps@((respNo,resp):resps')
      | respNo == reqNo = Left  req   : go (o+1) (c:cs) reqs' resps
      | c && o < omax   = Left  req   : go (o+1)    cs  reqs' resps
      | otherwise       = Right resp  : go (o-1)    cs  reqs  resps'

    go o []     reqs@((reqNo, req) :reqs')
               resps@((respNo,resp):resps')
      | respNo == reqNo = Left  req   : go (o+1) [] reqs' resps
      | otherwise       = Right resp  : go (o-1) [] reqs  resps'

    go _ _ [] resps     = map (Right . snd) resps
    go _ _ (_:_) []     = error "pipelineInterleaving: not enough responses"
