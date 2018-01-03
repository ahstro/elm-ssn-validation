module Validate.SSN.Swedish exposing (isValid, validate)

{-| This library allows you to validate Swedish social security
numbers ("personnummer").

It supports the following formats:

  - `"8112189876"`
  - `"811218-9876"`
  - `"811218+9876"`
  - `"198112189876"`
  - `"19811218-9876"`
  - `"19811218+9876"`


# Validation

@docs validate, isValid

-}

import Luhn
import Regex exposing (Regex, regex)


{-| Accepts a string and returns a `Result String String` indicating
whether the string was a valid Swedish SSN ("personnummer").

    validate "811218-9876" == Ok "811218-9876"
    validate "811218" == Err "Invalid Swedish SSN"

-}
validate : String -> Result String String
validate ssn =
    let
        match =
            case Regex.find Regex.All ssnRegex ssn of
                validSSN :: [] ->
                    Ok validSSN

                _ ->
                    Err "Invalid Swedish SSN"
    in
    match
        |> Result.map .submatches
        |> Result.andThen normalize
        |> Result.andThen Luhn.validate
        |> Result.map (\_ -> ssn)


{-| Same as `validate` but returns a `Bool` instead, for easy use in
`if`-expressions.

    isValid "811218-9876" == True
    isValid "811218" == False

-}
isValid : String -> Bool
isValid ssn =
    case validate ssn of
        Ok _ ->
            True

        Err _ ->
            False


{-| Takes the `.submatches` of a `ssnRegex` match and returns a either
an `Ok` with a normalized string (with the "YYMMDDXXXX" format) or an
`Err`.
-}
normalize : List (Maybe String) -> Result String String
normalize ssn =
    case ssn of
        (Just year) :: (Just month) :: (Just day) :: (Just controlDigits) :: [] ->
            Ok <| String.right 2 year ++ month ++ day ++ controlDigits

        _ ->
            Err "Invalid input to normalize"


{-| Regex for valid inputs.

Matches the following formats:

  - `"8112189876"`
  - `"811218-9876"`
  - `"811218+9876"`
  - `"198112189876"`
  - `"19811218-9876"`
  - `"19811218+9876"`

The regex is made up of seven parts

  - `^`

    Denotes the start of the line

  - `((?:\d{2})?\d{2})`

    The year part. Matches two optional digits, e.g. "19", and two
    obligatory digits, e.g. "81".

  - `(\d{2})`

    The month part. Matches two obligatory digits, e.g. "12".

  - `(\d{2})`

    The day part. Matches two obligatory digits, e.g. "18".

  - `[-+]?`

    The separator part. Matches one optional separator, e.g. "-".

  - `(\d{4})`

    The control-digits part. Matches four obligatory digits, e.g. "9876".

  - `$`

    Denotes the start of the line

-}
ssnRegex : Regex
ssnRegex =
    regex "^((?:\\d{2})?\\d{2})(\\d{2})(\\d{2})[-+]?(\\d{4})$"
