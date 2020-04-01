module ScriptTypes exposing (Choice, Scene, Script)


type alias Choice =
    { key : String
    , text : String
    }


type alias Scene =
    { key : String
    , contents : String
    , options : List Choice
    }


type alias Script =
    List Scene
