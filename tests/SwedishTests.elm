module SwedishTests exposing (suite)

import Expect
import Result.Extra exposing (isErr, isOk)
import Test exposing (Test, describe, test)
import Validate.SSN.Swedish as SSN


validSSNs : List String
validSSNs =
    [ "8112189876"
    , "811218-9876"
    , "811218+9876"
    , "198112189876"
    , "19811218-9876"
    , "19811218+9876"
    ]


invalidSSNs : List String
invalidSSNs =
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
                        (List.map (isErr << SSN.validate) invalidSSNs)
                        (List.map (always True) invalidSSNs)
            ]
        ]
