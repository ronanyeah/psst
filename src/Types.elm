module Types exposing (..)

import Animation
import Dom
import Element
import Http
import Json.Encode exposing (Value)
import Time exposing (Time)
import Window


type alias ScrollData =
    { scrollHeight : Int
    , scrollTop : Int
    , clientHeight : Int
    }


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
    | Animate Animation.Msg
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
    , typingStatus : TypingStatus
    , messages : List Message
    , lastTyped : Time
    , lastTypedPing : Time
    , isLive : Bool
    , input : String
    }


type SocketMessages
    = ReceiveMessage String
    | Error String
    | Key PublicKeyRecord
    | ChatUnavailable
    | Typing
    | ConnectionDead


type ScrollStatus
    = Static
    | Moving Time Int
