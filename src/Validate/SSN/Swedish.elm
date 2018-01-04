module Validate.SSN.Swedish exposing (isValid, normalize, validate)

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

@docs validate, isValid, normalize

-}

import Date exposing (Date)
import Helpers
import Luhn
import Regex exposing (Regex, regex)


{-| Accepts a string and returns a `Result String String` indicating
whether the string was a valid Swedish SSN ("personnummer").

    validate "811218-9876" == Ok "811218-9876"
    validate "811218" == Err "Invalid Swedish SSN"

-}
validate : String -> Result String String
validate ssn =
    ssn
        |> validateAndNormalize Nothing
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


{-| Accepts a date and a string and returns a returns a either an `Ok`
with a normalized string (with the "YYYYMMDDXXXX" format) or an `Err`.

The supplied date is used when calculating the full year for 10-digit
SSNs and should therefore probably be the current date.

    january4th2018 =
        Date.fromTime 1515083518766

    normalize january4th2018 "811218-9876" == Ok "198112189876"
    normalize january4th2018 "811218+9876" == Ok "188112189876"
    normalize january4th2018 "811218" == Err "Invalid Swedish SSN"

-}
normalize : Date -> String -> Result String String
normalize date ssn =
    validateAndNormalize (Just date) ssn


{-| Does the heavy lifting behind both `normalize` and `validate`,
this just has a more literal name, but is not exposed to the user.
-}
validateAndNormalize : Maybe Date -> String -> Result String String
validateAndNormalize date ssn =
    ssn
        |> Helpers.matchOne ssnRegex
        |> Result.mapError (\_ -> "Invalid Swedish SSN")
        |> Result.map .submatches
        |> Result.andThen (assembleNormalized date)
        |> Result.andThen luhnValidate


{-| Assemble the matched groups of a valid `ssnRegex` into a
normalized ssn string.

The separator is used to indicate whether a person is on or over their
100th year (with a "+") or not (with a "-").

-}
assembleNormalized : Maybe Date -> List (Maybe String) -> Result String String
assembleNormalized date match =
    case match of
        (Just year) :: (Just month) :: (Just day) :: separator :: (Just controlDigits) :: [] ->
            let
                year_ =
                    if String.length year == 4 then
                        year
                    else
                        calculateFullYear date separator year
            in
            Ok <| year_ ++ month ++ day ++ controlDigits

        _ ->
            Err "Invalid input to normalize"


{-| Calculate a full - four digit - year using a relative date, a
separator, and a two digit year.
-}
calculateFullYear : Maybe Date -> Maybe String -> String -> String
calculateFullYear date separator year =
    let
        isOn100thYear =
            separator
                |> Maybe.withDefault "-"
                |> (==) "+"

        yearInt =
            year
                |> String.toInt
                |> Result.withDefault 0

        subtrahend =
            if isOn100thYear then
                100 + yearInt
            else
                yearInt
    in
    case date of
        Just date ->
            date
                |> Date.year
                |> Helpers.subtract subtrahend
                |> toString
                |> String.left 2
                |> Helpers.append year

        Nothing ->
            -- If Date is Nothing, this is only used for validation,
            -- so actual year doesn't matter.
            "19" ++ year


{-| Check if a normalized SSN is Luhn valid.
-}
luhnValidate : String -> Result String String
luhnValidate normalizedSSN =
    normalizedSSN
        |> String.dropLeft 2
        |> Luhn.validate
        |> Result.map (\_ -> normalizedSSN)


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

  - `([-+])?`

    The separator part. Matches one optional separator, e.g. "-".

  - `(\d{4})`

    The control-digits part. Matches four obligatory digits, e.g. "9876".

  - `$`

    Denotes the start of the line

-}
ssnRegex : Regex
ssnRegex =
    regex "^((?:\\d{2})?\\d{2})(\\d{2})(\\d{2})([-+])?(\\d{4})$"
