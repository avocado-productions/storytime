module ScriptTypes exposing (Choice, Key, Predicate, Scene, Script, Style, Template(..), Text(..), Var)


type alias Style =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    , under : Bool
    }


type Text
    = Styled Style String
    | Problem String
    | InlineConditional { var : Var, ifSet : Maybe (List Text), ifUnset : Maybe (List Text) }


type Template
    = Paragraph (List Text) Template
    | Conditional (List ( Predicate, Template )) (Maybe Template)
    | Choices { options : List Choice, continuation : Maybe Choice }
    | Set Var Template
    | Unset Var Template
    | Toggle Var Template
    | Return


type alias Predicate =
    List ( Bool, Var )


type alias Key =
    String


type alias Var =
    String


type alias Choice =
    { key : Key
    , text : List Text
    , vanishing : Bool
    , break : Bool
    }


type alias Scene =
    { key : String
    , set : List Var
    , contents : Template
    }


type alias Script =
    List Scene
