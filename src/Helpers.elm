module Helpers exposing (append, matchOne, subtract)

import Regex exposing (Regex)


{-| Takes a `Regex` and a `String` and returns a `Result` if the
string matches the regex only _once_.
-}
matchOne : Regex -> String -> Result String Regex.Match
matchOne regex string =
    case Regex.find Regex.All regex string of
        match :: [] ->
            Ok match

        _ :: _ ->
            Err <| "Couldn't match '" ++ string ++ "' only once."

        [] ->
            Err <| "Couldn't match '" ++ string ++ "'."


{-| Used to make piping numbers easier

    100
        |> subtract 10 -- 90

-}
subtract : number -> number -> number
subtract =
    flip (-)


{-| Used to make piping easier

    "foo"
        |> append "bar" -- "foobar"

-}
append : appendable -> appendable -> appendable
append =
    flip (++)
