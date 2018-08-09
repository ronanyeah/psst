module Main where

import Data.Either
  ( note
  , either
  , Either(..)
  )
import Data.Int
  ( fromString
  )
import Data.List.NonEmpty
  ( NonEmptyList
  )
import Data.Maybe
  ( Maybe(..)
  )
import Data.String
  ( drop
  )
import Effect
  ( Effect
  )
import Effect.Console
  ( log
  )
import Effect.Random
  ( random
  )
import Foreign
  ( ForeignError
  )
import Foreign.Object
  ( Object
  , empty
  , lookup
  , insert
  )
import Node.Encoding (Encoding(..))
import Node.HTTP
  ( Server
  , createServer
  , listen
  , responseAsStream
  , setStatusCode
  , setHeader
  )
import Node.Stream
  ( writeString
  , end
  )
import Node.Process
  ( exit
  , lookupEnv
  )
import Prelude
  ( Unit
  , bind
  , discard
  , pure
  , unit
  , (<>)
  , (>>=)
  , show
  , (>>>)
  , (>=>)
  , (<$>)
  )
import Simple.JSON
  ( readJSON
  , writeJSON
  )

import Effect.Ref as Rf

foreign import registerWsServer ::
  Server ->
  (Msg -> Effect Unit) ->
  Effect Unit

foreign import sendMessage ::
  String ->
  Unit ->
  Effect Unit

foreign import transmit ::
  String ->
  Unit ->
  Unit ->
  Effect Unit

createChats ::
  Effect (Rf.Ref (Object Chat))
createChats = Rf.new empty

main ::
  Effect Unit
main = lookupEnv "PORT" >>= (note "Missing PORT" >=> (fromString >>> note "Invalid PORT")) >>> either logError start

logError ::
  String ->
  Effect Unit
logError str = do
  log str
  exit 1

cb ::
  (Rf.Ref (Object Unit)) ->
  Msg ->
  Effect Unit
cb sockets { message, ws } = do
  ss <- Rf.read sockets
  case decodeTransmit message of
    Left x -> do
      case decodeBJoin message of
        Left _ -> do
          case decodeCreate message of
            Left _ -> do
              log "create not found"
            Right _ -> do
              aId <- randomString
              _ <- Rf.modify (insert aId ws) sockets
              sendMessage (writeBJoin { chatId: aId
                                      }) ws
        Right { chatId } -> do
          case lookup chatId ss of
            Just _ -> do
              bId <- randomString
              _ <- Rf.modify (insert bId ws) sockets
              sendMessage (writeBJoin { chatId: bId
                                      }) ws
            Nothing -> do
              log "chat not found"
    Right { conn, contents } -> do
      case lookup conn ss of
        Just ws_ -> do
          transmit contents ws ws_
        Nothing -> do
          log "conn not found"

start ::
  Int ->
  Effect Unit
start port = do
  chats <- createChats
  sockets <- Rf.new empty
  server <- createServer (\_ res -> do
          let outputStream = responseAsStream res
          setStatusCode res 200
          setHeader res "Content-Type" "application/json"
          _ <- writeString outputStream UTF8 ("{\"hello\": \"world\"}") (pure unit)
          end outputStream (pure unit)
    )
  registerWsServer server (cb sockets)
  listen server { hostname: ""
                , port
                , backlog: Nothing
                } (log ("server listening on port " <> (show port)))

randomString ::
  Effect String
randomString = drop 2 <$> show <$> random

type Chat
  = {a :: String, b :: String}

type Msg
  = {message :: String, ws :: Unit}

type Transmit
  = {conn :: String, contents :: String}

type BJoin
  = {chatId :: String}

type Create
  = {create :: Boolean}

decodeTransmit ::
  String ->
  Either (NonEmptyList ForeignError) Transmit
decodeTransmit = readJSON

decodeBJoin ::
  String ->
  Either (NonEmptyList ForeignError) BJoin
decodeBJoin = readJSON

decodeCreate ::
  String ->
  Either (NonEmptyList ForeignError) Create
decodeCreate = readJSON

writeBJoin ::
  BJoin ->
  String
writeBJoin = writeJSON
