effect module UUID where { command = MyCmd } exposing (generate)

import Task exposing (..)


type MyCmd msg
    = GenerateGUID (Int -> msg)


generate : (Int -> msg) -> Cmd msg
generate msgFromInt =
    command (GenerateGUID msgFromInt)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f (GenerateGUID intToA) =
    GenerateGUID (\b -> f (intToA b))


init : Task Never Int
init =
    Task.succeed 0


onEffects : Platform.Router msg Never -> List (MyCmd msg) -> Int -> Task Never Int
onEffects router cmdLst currentID =
    case cmdLst of
        [] ->
            Task.succeed currentID

        (GenerateGUID f) :: cmds ->
            Platform.sendToApp router (f currentID)
                |> Task.andThen (\_ -> onEffects router cmds (currentID + 1))


onSelfMsg : Platform.Router msg Never -> Never -> Int -> Task Never Int
onSelfMsg router selfMsg currentID =
    Task.succeed currentID
