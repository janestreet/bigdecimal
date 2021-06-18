(** A high-precision representation of decimal numbers as [mantissa * 10^exponent], where
    the mantissa is internally a [Bigint.t] and the exponent is an [int]. *)

open Core

type t [@@deriving sexp, bin_io]

val zero : t

include Comparable.S with type t := t
include Hashable.S with type t := t

val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val scale_by : t -> power_of_ten:int -> t
val mantissa : t -> Bigint.t
val exponent : t -> int
val of_int : int -> t
val of_bigint : Bigint.t -> t

(** Lossless conversion to [Bignum.t]. Please note, however, that [Bignum.to_string] may
    lose precision. *)
val to_bignum : t -> Bignum.t

(** [of_string] and [to_string_no_sn] are precise and round-trip. *)
val of_string : string -> t

(** Converts to a string without using scientific notation (e.g., no e's show up in the
    middle, as in 1.3e12) *)
val to_string_no_sn : t -> string

(** Like [to_string_no_sn] but adds separators to group digits in the integral part into
    triplets, e.g. [1,234,567.890123]. [sep] is comma by default.
*)
val to_string_no_sn_grouping : ?sep:char -> t -> string

(** Default rounding direction is [`Nearest]. *)
val round : ?dir:[ `Down | `Up | `Nearest | `Zero ] -> t -> t

(** Default rounding direction is [`Nearest]. *)
val round_to_bigint : ?dir:[ `Down | `Up | `Nearest | `Zero ] -> t -> Bigint.t

(** Returns [t] as an exact integer, if [t] is integral and fits within [int]; None
    otherwise. *)
val to_int : t -> int option

(** An exception-throwing version of [to_int]. *)
val to_int_exn : t -> int

(** {2 Floating-point conversions}

    [to_float] and [of_float] round-trip when starting with a float.
*)

(** [to_float] is lossy, since not all decimals can be represented as floats. The result
    is the floating point number that is closest to the provided decimal. *)
val to_float : t -> float

val abs : t -> t
val neg : t -> t
val sign : t -> Sign.t
val is_zero : t -> bool

(** Produces a decimal representation that, when converted back via [to_float], produces
    the original floating point number. It doesn't, however, pick the decimal that is
    exactly equal to the float, even though this exists.

    Instead, it aims to minimize the length of the generated decimal, subject to the
    roundtrip property described above.  See [Float.to_string] for details on the
    semantics of the value chosen.

    An error is returned in the case that the float is not representable as a decimal,
    e.g., NaN and infinity.
*)
val of_float_short : float -> t Or_error.t

(** An exception-throwing version of [of_float_short]  *)
val of_float_short_exn : float -> t

(** Produces a decimal representation that is exactly equal to the provided bignum, or
    an error if the bignum is not exactly representable as a decimal: e.g., infinity
    or [1/3]. *)
val of_bignum : Bignum.t -> t Or_error.t

(** An exception-throwing version of [of_bignum]. *)
val of_bignum_exn : Bignum.t -> t

module Stable : sig
  module V2 : Stable_without_comparator with type t = t
  module V3 : Stable_without_comparator with type t = t
end
