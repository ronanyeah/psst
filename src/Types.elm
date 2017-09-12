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
    = Init
    | CbWebsocketMessage String
    | InputChange String
    | Send
    | CbEncrypt String
    | CbDecrypt String
    | Resize Window.Size
    | Animate Animation.Msg
    | Tick Time
    | CbScrollToBottom (Result Dom.Error ())
    | DisplayScrollButton Value
    | ScrollToBottom


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
    }


type alias Message =
    { self : Bool
    , content : String
    }


type PublicKeyString
    = PublicKeyString String


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


type alias TypingAt =
    Time


type Status
    = Start PublicKeyRecord
    | WaitingForBKey PublicKeyRecord ConnId RoomId
    | Joining PublicKeyRecord
    | WaitingForAKey ConnId
    | Ready ConnId PublicKeyString TypingStatus


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
