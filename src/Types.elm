module Types exposing (ChatArgs, ConnId(..), CryptoKey, Device(..), Flags, Message(..), Model, Msg(..), ScrollData, ScrollStatus(..), SocketMessage(..), Status(..))

import Browser
import Browser.Dom
import Json.Encode exposing (Value)
import Time exposing (Posix)
import Url exposing (Url)


type alias ScrollData =
    { scrollHeight : Int
    , scrollTop : Int
    , clientHeight : Int
    }


type Device
    = Mobile
    | Desktop


type Msg
    = CreateChat
    | CbWebsocketMessage String
    | InputChange String
    | Send
    | CbEncrypt String
    | CbDecrypt String
    | ExitChat
    | PublicKeyLoaded Value
    | Tick Posix
    | CbScrollToBottom (Result Browser.Dom.Error ())
    | DisplayScrollButton Value
    | ScrollToBottom
    | Share String


type alias Model =
    { status : Status
    , origin : String
    , device : Device
    , time : Posix
    , arrow : Bool
    , scroll : ScrollStatus
    , shareEnabled : Bool
    , copyEnabled : Bool
    , myPublicKey : CryptoKey
    }


type alias Flags =
    { maybeChatId : Maybe String
    , origin : String
    , shareEnabled : Bool
    , copyEnabled : Bool
    , publicKey : CryptoKey
    }


type Message
    = Self String
    | Them String
    | ChatStart
    | ConnEnd


type ConnId
    = ConnId String


type alias CryptoKey =
    { alg : String
    , e : String
    , ext : Bool
    , key_ops : List String
    , kty : String
    , n : String
    }


type Status
    = Start
    | AWaitingForBKey ConnId
    | BWaitingForAKey ConnId
    | InChat ChatArgs
    | ErrorView String


type alias ChatArgs =
    { connId : ConnId
    , lastSeenTyping : Posix
    , messages : List Message
    , lastTypedPing : Posix
    , isLive : Bool
    , input : String
    , partnerPublicKey : Value
    }


type SocketMessage
    = ReceiveMessage String
    | Key CryptoKey
    | KeyAndConn CryptoKey ConnId
    | ChatUnavailable
    | Typing
    | ConnectionDead
    | ChatCreated ConnId
    | ChatMatched { aId : ConnId, bId : ConnId }


type ScrollStatus
    = Static
    | Moving Posix Int
