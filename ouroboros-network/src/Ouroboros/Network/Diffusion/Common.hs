{-# LANGUAGE DataKinds #-}
-- Common things between P2P and NonP2P Diffusion modules

module Ouroboros.Network.Diffusion.Common
  ( DiffusionAddress (..)
  , DiffusionApplications (..)
  , DiffusionInitializationTracer (..)
  , DiffusionFailure (..)
  ) where

import Control.Exception (SomeException, Exception)

import           Data.List.NonEmpty (NonEmpty)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)

import           Network.Socket (SockAddr, AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient (LocalAddress)
import           Ouroboros.Network.NodeToClient.Version
                   ( NodeToClientVersion
                   , NodeToClientVersionData
                   )
import           Ouroboros.Network.NodeToNode (RemoteAddress)
import           Ouroboros.Network.NodeToNode.Version
                   ( NodeToNodeVersion
                   , NodeToNodeVersionData
                   , DiffusionMode
                   )
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Snocket (FileDescriptor)


-- TODO: use LocalAddress where appropriate rather than 'path'.
--
data DiffusionInitializationTracer
  = RunServer !(NonEmpty SockAddr)
  | RunLocalServer !LocalAddress
  | UsingSystemdSocket !FilePath
  -- Rename as 'CreateLocalSocket'
  | CreateSystemdSocketForSnocketPath !FilePath
  | CreatedLocalSocket !FilePath
  | ConfiguringLocalSocket !FilePath !FileDescriptor
  | ListeningLocalSocket !FilePath !FileDescriptor
  | LocalSocketUp  !FilePath !FileDescriptor
  -- Rename as 'CreateServerSocket'
  | CreatingServerSocket !SockAddr
  | ConfiguringServerSocket !SockAddr
  | ListeningServerSocket !SockAddr
  | ServerSocketUp !SockAddr
  -- Rename as 'UnsupportedLocalSocketType'
  | UnsupportedLocalSystemdSocket !SockAddr
  -- Remove (this is impossible case), there's no systemd on Windows
  | UnsupportedReadySocketCase
  | DiffusionErrored SomeException
    deriving Show

-- TODO: add a tracer for these misconfiguration
data DiffusionFailure = UnsupportedLocalSocketType
                      | UnsupportedReadySocket -- Windows only
                      | UnexpectedIPv4Address
                      | UnexpectedIPv6Address
                      | UnexpectedUnixAddress
                      | NoSocket
  deriving (Eq, Show)

instance Exception DiffusionFailure

data DiffusionAddress = DiffusionAddress {
    daIPv4Address  :: Maybe (Either Socket.Socket AddrInfo)
    -- ^ an @IPv4@ socket ready to accept connections or an @IPv4@ addresses
  , daIPv6Address  :: Maybe (Either Socket.Socket AddrInfo)
    -- ^ an @IPV4@ socket ready to accept connections or an @IPv6@ addresses
  , daLocalAddress :: Maybe (Either Socket.Socket FilePath)
    -- ^ an @AF_UNIX@ socket ready to accept connections or an @AF_UNIX@
    -- socket path
  }


data DiffusionApplications m =
    DiffusionApplications {

      -- | NodeToNode initiator applications for initiator only mode.
      --
      -- TODO: we should accept one or the other, but not both:
      -- 'daApplicationInitiatorMode', 'daApplicationInitiatorResponderMode'.
      --
      daApplicationInitiatorMode
        :: Versions NodeToNodeVersion
                    NodeToNodeVersionData
                    (OuroborosBundle
                      InitiatorMode RemoteAddress
                      ByteString m () Void)

      -- | NodeToNode initiator & responder applications for bidirectional mode.
      --
    , daApplicationInitiatorResponderMode
        :: Versions NodeToNodeVersion
                    NodeToNodeVersionData
                    (OuroborosBundle
                      InitiatorResponderMode RemoteAddress
                      ByteString m () ())


    -- | NodeToClient responder application (server role)
    --
    , daLocalResponderApplication
        :: Versions NodeToClientVersion
                    NodeToClientVersionData
                    (OuroborosApplication
                      ResponderMode LocalAddress
                      ByteString m Void ())

    -- | Diffusion mode.
    --
    , daDiffusionMode :: DiffusionMode
    }
