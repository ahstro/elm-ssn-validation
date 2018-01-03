# elm-ssn-validation

Validate (currently only Swedish) social security numbers (personnummer).

## Installation

```sh
elm-package install ahstro/elm-ssn-validation
```

## Usage

```elm
import Validation.SSN.Swedish as SSN


case SSN.validate "811218-9876" of
    Ok ssn ->
        ssn ++ " is valid" -- "811218-9876 is valid"
    Err error ->
        error


if SSN.isValid "811218-9876" then
  "Yay"
else
  "Nay"

```

More examples are available in the [/tests](https://github.com/ahstro/elm-ssn-validation/tree/master/tests) folder.
