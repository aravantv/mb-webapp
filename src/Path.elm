module Path exposing (..)


type GenericField
    = Field String
    | Index Int


type alias SymbolicPath path =
    ( path, List GenericField )


stringOfGenericField : GenericField -> String
stringOfGenericField field =
    case field of
        Field s ->
            s

        Index i ->
            toString i


type alias PathProvider base path =
    { base | concretizer : SymbolicPath path -> path }



{--
    Any resource would typically implement a function of signature
    [commandBuilder : SymbolicPath path -> value -> Cmd msg]
--}
