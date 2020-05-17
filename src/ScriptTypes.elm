module ScriptTypes exposing (Choice, Scene, Script, Style, Text(..))


type alias Style =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    , under : Bool
    }


type Text
    = Styled Style String
    | Problem


type alias Choice =
    { key : String
    , text : List Text
    }


type alias Scene =
    { key : String
    , contents : List (List Text)
    , options : List Choice
    }


type alias Script =
    List Scene
