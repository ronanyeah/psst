module Types exposing (..)

import Animation
import Dom
import Element
import Json.Encode exposing (Value)
import Time exposing (Time)
import Window


type alias ScrollData =
    { scrollHeight : Int
    , scrollTop : Int
    , clientHeight : Int
    }


type Msg
    = StartMsg
    | CbWebsocketMessage String
    | InputChange String
    | Send
    | CbEncrypt String
    | CbDecrypt String
    | PublicKeyLoaded ()
    | Resize Window.Size
    | Animate Animation.Msg
    | Tick Time
    | CbScrollToBottom (Result Dom.Error ())
    | DisplayScrollButton Value
    | ScrollToBottom
    | Share String


type alias Model =
    { status : Status
    , messages : List Message
    , input : String
    , lastTyped : Time
    , lastTypedPing : Time
    , location : String
    , wsApi : String
    , device : Element.Device
    , keySpin : Animation.State
    , time : Time
    , arrow : Bool
    , scroll : ScrollStatus
    , shareEnabled : Bool
    , copyEnabled : Bool
    }


type alias Flags =
    { maybeRoomId : Maybe String
    , publicKey : PublicKeyRecord
    , origin : String
    , wsUrl : String
    , shareEnabled : Bool
    , copyEnabled : Bool
    }


type alias Message =
    { self : Bool
    , content : String
    }


type ConnId
    = ConnId String


type RoomId
    = RoomId String


type TypingStatus
    = IsTyping Time
    | NotTyping


type alias PublicKeyRecord =
    { alg : String
    , e : String
    , ext : Bool
    , key_ops : List String
    , kty : String
    , n : String
    }


type Status
    = Start PublicKeyRecord
    | WaitingForBKey PublicKeyRecord ConnId RoomId
    | Joining PublicKeyRecord
    | WaitingForAKey ConnId
    | Ready ConnId TypingStatus
    | ErrorView String


type SocketMessages
    = Waiting ConnId RoomId
    | ReceiveMessage String
    | Error String
    | ReceiveAId ConnId
    | Key PublicKeyRecord
    | RoomUnavailable
    | Typing


type ScrollStatus
    = Static
    | Moving Time Int
