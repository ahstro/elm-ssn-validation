module SwedishTests exposing (suite)

import Date exposing (Date)
import Expect
import Result.Extra exposing (isErr, isOk)
import Test exposing (Test, describe, test)
import Validate.SSN.Swedish as SSN


{-| All valid SSN formats.
Doesn't normalize to the same string.
-}
validSSNs : List String
validSSNs =
    "811218+9876" :: validFormats


{-| All valid formats for the same SSN.
Normalizes to "198112189876".
-}
validFormats : List String
validFormats =
    [ "8112189876"
    , "811218-9876"
    , "198112189876"
    , "19811218-9876"
    , "19811218+9876"
    ]


{-| Some invalid inputs
-}
invalidInputs : List String
invalidInputs =
    [ "98112189876"
    , "9811218-9876"
    , "9811218+9876"
    , "19811218"
    , "19811218-"
    , "19811218+"
    , "811218"
    , "811218-"
    , "811218+"
    , "asdf+"
    , "asdf"
    , "1"
    , "0123456789"
    , ""
    ]


january4th2018 : Date
january4th2018 =
    Date.fromTime 1515083518766


suite : Test
suite =
    describe "Validate.SSN.Swedish"
        [ describe "validate"
            [ test "Returns Ok for valid SSNs" <|
                \_ ->
                    Expect.equal
                        (List.map (isOk << SSN.validate) validSSNs)
                        (List.map (always True) validSSNs)
            , test "Returns Err for invalid SSNs" <|
                \_ ->
                    Expect.equal
                        (List.map (isErr << SSN.validate) invalidInputs)
                        (List.map (always True) invalidInputs)
            ]
        , describe "normalize"
            [ test "Normalizes all valid formats correctly" <|
                \_ ->
                    Expect.equal
                        (List.map (SSN.normalize january4th2018) validFormats)
                        (List.map (always (Ok "198112189876")) validFormats)
            , test "Fails to normalize invalid inputs" <|
                \_ ->
                    Expect.equal
                        (List.map (isErr << SSN.normalize january4th2018) invalidInputs)
                        (List.map (always True) invalidInputs)
            ]
        ]
