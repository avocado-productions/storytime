module ScriptTypes exposing
    ( Choice
    , Key
    , Predicate
    , Scene
    , Script
    , Style
    , Template(..)
    , Text(..)
    , Var
    )


type alias Style =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    , under : Bool
    }


type Text
    = Styled Style String
    | Problem String
    | InlineConditional { markup : List Text, appears : Predicate }


type Template
    = Paragraph (List Text) Template
    | Conditional (List ( Predicate, Template ))
    | Choices (List Choice)
    | Set Var Template
    | Unset Var Template
    | Toggle Var Template
    | Fin


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
    , appears : Predicate
    }


type alias Scene =
    { key : String
    , set : List Var
    , contents : Template
    }


type alias Script =
    List Scene
