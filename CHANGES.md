## Release v0.17.0

- broke dependency on expect_test_helpers_core
- added rounding dir parameter
- added support for bankers rounding
- added new human readable sexp format representation

## Release v0.16.0

- Add new functions and constants to `Bigdecimal`:
  * `Bigdecimal.one` represents the value one in the `Bigdecimal` type
  * `div`: computes the division of two `Bigdecimal` values with optional `decimals_precision` parameter (default 15)
  * `sqrt`: computes the square root of a `Bigdecimal` value with optional `decimals_precision` parameter (default 15)
  * `**`: computes the power of a `Bigdecimal` value raised to a non-negative integer exponent
  * `scale_int`: scales a `Bigdecimal` value by an integer factor
  * `is_integral`: returns true if and only if the input value is an integer
  * `round_to_power_of_ten`: rounds the given value to the nearest power of ten with optional rounding direction parameter
  * `to_bigint_exact`: converts the given value to an exact `Bigint.t` if it is an integral value; returns `None` otherwise
  * `to_bigint_exact_exn`: converts the given value to an exact `Bigint.t` if it is an integral value; raises an exception otherwise
  * `log10_int_exact`: calculates the log in base 10; returns `Option.None` if the result is not representable as an integer
