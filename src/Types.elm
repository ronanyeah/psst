module Types exposing (..)

import Dom
import Http
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
    | CbCreateChat (Result Http.Error ChatCreate)
    | CbJoinChat (Result Http.Error ChatJoin)
    | CbEncrypt String
    | CbDecrypt String
    | ExitChat
    | PublicKeyLoaded ()
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
    , restUrl : String
    , device : Device
    , time : Time
    , arrow : Bool
    , scroll : ScrollStatus
    , shareEnabled : Bool
    , copyEnabled : Bool
    , myPublicKey : PublicKeyRecord
    }


type alias Flags =
    { maybeChatId : Maybe String
    , origin : String
    , wsUrl : String
    , restUrl : String
    , shareEnabled : Bool
    , copyEnabled : Bool
    }


type Message
    = Self String
    | Them String
    | ChatStart
    | ConnEnd


type ConnId
    = ConnId String


type ChatId
    = ChatId String


type alias PublicKeyRecord =
    { alg : String
    , e : String
    , ext : Bool
    , key_ops : List String
    , kty : String
    , n : String
    }


type alias ChatCreate =
    { connId : ConnId
    , chatId : ChatId
    }


type alias ChatJoin =
    { aId : ConnId
    , chatId : ChatId
    }


type Status
    = Start
    | AWaitingForBKey ConnId ChatId
    | BJoining
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
    }


type SocketMessage
    = ReceiveMessage String
    | Key PublicKeyRecord
    | ChatUnavailable
    | Typing
    | ConnectionDead


type ScrollStatus
    = Static
    | Moving Time Int
