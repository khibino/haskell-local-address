-----------------------------------------------------------------------------
-- |
-- Module      : Network.Socket.LocalAddress
-- Copyright   : 2011 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This package includes small functions to get local interface address.
-- Following method is traditional technique to getSockName without sending packet.
--
-----------------------------------------------------------------------------

module Network.Socket.LocalAddress (
  localSockAddr',
  localSockAddr,
  localAddress,
  localAddressString) where

import Network.Socket (Socket, Family(..),
                       SockAddr(..), SocketType(..), HostAddress,
                       socket, connect,
                       defaultProtocol,
                       getSocketName,
                       sClose,
                       inet_addr, inet_ntoa)

toIO :: a -> IO a
toIO =  return

{- | Get local address and datagram socket corresponding to remote address
     without sending any packet. -}
localSockAddr' :: SockAddr -> IO (SockAddr, Socket)
localSockAddr' remote =
  do sock <- socket (af remote) Datagram defaultProtocol
     connect sock remote
     addr <- getSocketName sock
     toIO (addr, sock)
  where af (SockAddrInet  _ _)     = AF_INET
        af (SockAddrInet6 _ _ _ _) = AF_INET6
        af (SockAddrUnix _)        = AF_UNIX

{- | Get local address corresponding to remote address 
     without sending any packet. -}
localSockAddr :: SockAddr -> IO SockAddr
localSockAddr remote = do (addr, sock) <- localSockAddr' remote
                          sClose sock
                          toIO $ erasePort addr
  where erasePort (SockAddrInet _ addr)      = SockAddrInet 0 addr
        erasePort (SockAddrInet6 _ fi ha si) = SockAddrInet6 0 fi ha si
        erasePort sa@(SockAddrUnix _)        = sa

{- | Get IPv4 local address corresponding to remote IPv4 address 
     without sending any packet. -}
localAddress :: HostAddress -> IO (Maybe HostAddress)
localAddress =  fmap expect4 . localSockAddr . SockAddrInet 0
  where expect4 (SockAddrInet _ addr) = Just addr
        expect4  _                    = Nothing

{- | Get IPv4 local address string corresponding to 
     remote IPv4 address string without sending any packet. -}
localAddressString :: String -> IO (Maybe String)
localAddressString astr =
  do remote <- inet_addr astr
     local <- localAddress remote
     maybe (toIO Nothing) (fmap Just . inet_ntoa) local
