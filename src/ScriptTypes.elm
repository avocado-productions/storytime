module ScriptTypes exposing (Choice, Key, Scene, Script, Style, Text(..))


type alias Style =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    , under : Bool
    }


type Text
    = Styled Style String
    | Problem String


type alias Key =
    String


type alias Choice =
    { key : Key
    , text : List Text
    }


type alias Scene =
    { key : String
    , contents : List (List Text)
    , options : Maybe (List Choice)
    , continuation : Maybe Key
    }


type alias Script =
    List Scene
