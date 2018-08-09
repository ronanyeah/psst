module Types exposing (..)

import Dom
import Json.Encode exposing (Value)
import Time exposing (Time)
import Window


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
    | Resize Window.Size
    | Tick Time
    | CbScrollToBottom (Result Dom.Error ())
    | DisplayScrollButton Value
    | ScrollToBottom
    | Share String


type alias Model =
    { status : Status
    , origin : String
    , wsUrl : String
    , device : Device
    , time : Time
    , arrow : Bool
    , scroll : ScrollStatus
    , shareEnabled : Bool
    , copyEnabled : Bool
    , myPublicKey : CryptoKey
    }


type alias Flags =
    { maybeChatId : Maybe String
    , origin : String
    , wsUrl : String
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
    | BJoining ConnId
    | BWaitingForAKey ConnId
    | InChat ChatArgs
    | ErrorView String


type alias ChatArgs =
    { connId : ConnId
    , lastSeenTyping : Time
    , messages : List Message
    , lastTypedPing : Time
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


type ScrollStatus
    = Static
    | Moving Time Int
