{-|
Module      : Database.MySQL.Unix
Description : Unix socket support for mysql-haskell
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provides unix socket connection.

-}

module Database.MySQL.Unix (
      connect
    , connectDetail
    ) where

import           Control.Exception              (bracketOnError, throwIO, SomeException, catch)
import           Control.Monad
import qualified Data.Binary                    as Binary
import qualified Data.Binary.Put                as Binary
import qualified Data.Connection                as Conn
import           Data.IORef                     (newIORef)
import           Database.MySQL.Connection      hiding (connect, connectDetail)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Packet
import           Database.MySQL.Protocol.Command
import qualified System.IO.Streams.UnixSocket   as UnixSocket
import qualified System.IO.Streams.TCP          as TCP
import qualified Data.Connection                as UnixSocket

--------------------------------------------------------------------------------

-- | Establish a MySQL connection over a Unix socket.
--
connect :: ConnectInfo -> IO MySQLConn
connect c = fmap snd (connectDetail c)

connectDetail :: ConnectInfo -> IO (Greeting, MySQLConn)
connectDetail (ConnectInfo host _ db user pass charset) =
    bracketOnError (connectWithBufferSize host bUFSIZE) UnixSocket.close go
  where
    go c  = do
        let is = UnixSocket.source c
        is' <- decodeInputStream is
        p <- readPacket is'
        greet <- decodeFromPacket p
        let auth = mkAuth db user pass charset greet
        write c $ encodeToPacket 1 auth
        q <- readPacket is'
        if isOK q
        then do
            consumed <- newIORef True
            let waitNotMandatoryOK = catch
                    (void (waitCommandReply is'))           -- server will either reply an OK packet
                    ((\ _ -> return ()) :: SomeException -> IO ())   -- or directy close the connection
                conn = MySQLConn is'
                    (write c)
                    (writeCommand COM_QUIT (write c) >> waitNotMandatoryOK >> UnixSocket.close c)
                    consumed
            return (greet, conn)
        else UnixSocket.close c >> decodeFromPacket q >>= throwIO . ERRException

    connectWithBufferSize h bs = UnixSocket.connectSocket h >>= TCP.socketToConnection bs
    write c a = UnixSocket.send c $ Binary.runPut . Binary.put $ a
