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
    | ExitChat
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
    , origin : String
    , wsApi : String
    , device : Element.Device
    , keySpin : Animation.State
    , time : Time
    , arrow : Bool
    , scroll : ScrollStatus
    , shareEnabled : Bool
    , copyEnabled : Bool
    , myPublicKey : PublicKeyRecord
    }


type alias Flags =
    { maybeRoomId : Maybe String
    , origin : String
    , wsUrl : String
    , shareEnabled : Bool
    , copyEnabled : Bool
    }


type Message
    = Message MessageType String


type MessageType
    = Self
    | Them
    | System


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
    = Start
    | AWaitingForBKey ConnId RoomId
    | BJoining
    | BWaitingForAKey ConnId
    | InChat ChatArgs
    | ErrorView String


type alias ChatArgs =
    { connId : ConnId
    , typingStatus : TypingStatus
    , messages : List Message
    , lastTyped : Time
    , lastTypedPing : Time
    , isLive : Bool
    , input : String
    }


type SocketMessages
    = Waiting ConnId RoomId
    | ReceiveMessage String
    | Error String
    | ReceiveAId ConnId
    | Key PublicKeyRecord
    | RoomUnavailable
    | Typing
    | ConnectionDead


type ScrollStatus
    = Static
    | Moving Time Int
