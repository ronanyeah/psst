{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent  (MVar, modifyMVar, modifyMVar_, newMVar,
                                      readMVar, takeMVar)
import           Control.Exception   (finally)
import           Control.Monad       (forM_, forever, mzero)
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import           Data.ByteString     (ByteString)
import           Data.Char           (isPunctuation, isSpace)
import           Data.HashMap.Strict (lookup)
import           Data.Map.Strict     (Map, assocs, delete, empty, insert,
                                      lookup, member)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         (mappend)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding  (encodeUtf8)
import qualified Data.Text.IO        as T
import           GHC.Generics
import           System.Environment  (getEnv)
import           System.RandomString

import qualified Network.WebSockets  as WS

type ServerState = Map Text WS.Connection

data Msg
  = CreateChat
  | BJoin Text
  | Transmit Text
             Text
  deriving (Show)

instance FromJSON Msg where
  parseJSON (Object o) =
    case Data.HashMap.Strict.lookup (T.pack "create") o of
      Just _ -> pure CreateChat
      _ ->
        case Data.HashMap.Strict.lookup (T.pack "chatId") o of
          Just (String str) -> pure $ BJoin str
          _ ->
            case Data.HashMap.Strict.lookup (T.pack "conn") o of
              Just (String conn) ->
                case Data.HashMap.Strict.lookup (T.pack "contents") o of
                  Just (String contents) -> pure $ Transmit conn contents
                  _                      -> fail "bad lookups"
              _ -> fail "bad lookups"
  parseJSON _ = fail "no object"

parsePort :: String -> Int
parsePort = read

main :: IO ()
main = do
  state <- newMVar empty
  port <- parsePort <$> getEnv "PORT"
  WS.runServer "0.0.0.0" port $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  forever $ do
    msg <- WS.receiveData conn
    let res = decodeStrict $ encodeUtf8 msg :: Maybe Msg
    case res of
      Just CreateChat -> do
        ns <- randomString $ StringOpts Base64 12
        modifyMVar_ state $ \s -> do return $ insert ns conn s
        WS.sendTextData conn (T.pack ("{\"chatId\":\"" ++ T.unpack ns ++ "\"}"))
      Just (BJoin str) -> do
        ns <- randomString $ StringOpts Base64 12
        modifyMVar_ state $ \s -> do return $ insert ns conn s
        WS.sendTextData
          conn
          (T.pack
             ("{\"aId\":\"" ++
              T.unpack str ++ "\", \"bId\":\"" ++ T.unpack ns ++ "\"}"))
      Just (Transmit id str) -> do
        clients <- readMVar state
        case Data.Map.Strict.lookup id clients of
          Just c -> do
            WS.sendTextData c str
          Nothing -> do
            fail "cant transmit"
      Nothing -> fail "bad msg"
