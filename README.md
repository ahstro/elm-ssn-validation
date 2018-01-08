# elm-ssn-validation

Validate social security numbers.
If the type of SSN you'd like to validate isn't available, submit an
[issue](https://github.com/ahstro/elm-ssn-validation/issues/new) or,
even better, a [pull request](https://github.com/ahstro/elm-ssn-validation/compare).


## Available SSNs

| Language                        | Validation         | Normalization      |
| ------------------------------- | :----------------: | :----------------: |
| :sweden: Swedish (personnummer) | :white_check_mark: | :white_check_mark: |

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
