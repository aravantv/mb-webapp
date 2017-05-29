module ModelJsonParameters exposing (..)


sanitizeName : String -> String
sanitizeName name =
    name ++ "_"


unsanitizeName : String -> String
unsanitizeName =
    String.dropRight 1


typeFieldName : String
typeFieldName =
    "type"


valueFieldName : String
valueFieldName =
    "value"


stringTypeID : String
stringTypeID =
    "string"


intTypeID : String
intTypeID =
    "int"


boolTypeID : String
boolTypeID =
    "bool"
