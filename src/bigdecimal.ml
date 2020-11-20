open Core_kernel
open Int.Replace_polymorphic_compare
module Z = Zarith.Z

let z_ten = Bigint.of_int 10 |> Bigint.to_zarith_bigint

let pow_10_z =
  (* When performing [Bignum.t -> Bigdecimal.t] conversion, we need to compute the value
     [10**(log2 bignum)].  Meanwhile, [log2 (max finite float)] is approximately 1024, so
     this seems like a reasonable guess for the upper bound for computations where
     performance may matter.  Add 17% tip, and you end up with 1200.  On the other hand,
     1200 words sounds like a sane enough amount of memory for a library to preallocate
     statically.  If this table fills up, it will take 0.3 MB, which is also not crazy for
     something actually being used. *)
  let max_memoized_pow = 1200 in
  let tbl = Array.create ~len:(max_memoized_pow + 1) None in
  let pow_10_z n = Z.pow z_ten n in
  fun n ->
    if n > max_memoized_pow
    then pow_10_z n
    else (
      match tbl.(n) with
      | Some x -> x
      | None ->
        let x = pow_10_z n in
        tbl.(n) <- Some (pow_10_z n);
        x)
;;

let pow_10 n = pow_10_z n |> Bigint.of_zarith_bigint

module T : sig
  (** This represents the decimal: mantissa * 10 ^ exponent. An invariant of the type is
      that the mantissa is either zero or an integer not divisible by 10. Also, it's
      guaranteed that any two distinct decimal numbers will have distinct representations,
      which requires in addition that zero is always expressed with an exponent of
      zero. *)
  type t = private
    { mantissa : Bigint.t
    ; exponent : int
    }
  [@@deriving fields, hash, compare]

  val zero : t
  val scale_by : t -> power_of_ten:int -> t
  val create : mantissa:Bigint.t -> exponent:int -> t

  val scaling_to_least_common_exponent
    :  t
    -> t
    -> f:(lce:int -> mantissa_a:Bigint.t -> mantissa_b:Bigint.t -> 'a)
    -> 'a

  module Stable : sig
    module V2 : Stable_without_comparator with type t = t
    module V3 : Stable_without_comparator with type t = t
  end
end = struct
  (* Invariant: [mantissa] is either zero or an integer not divisible by 10. *)
  type t =
    { mantissa : Bigint.t
    ; exponent : int
    }
  [@@deriving fields, hash]

  (* derived compare would be incorrect here *)

  let scaling_to_least_common_exponent a b ~f =
    let lce = min a.exponent b.exponent in
    let scale_mantissa { mantissa; exponent } =
      Bigint.(mantissa * pow_10 (Int.( - ) exponent lce))
    in
    f ~lce ~mantissa_a:(scale_mantissa a) ~mantissa_b:(scale_mantissa b)
  ;;

  let is_zero t = Bigint.(t.mantissa = zero)

  let compare x y =
    if x.exponent = y.exponent
    then Bigint.compare x.mantissa y.mantissa
    else if is_zero x
    then Bigint.(compare zero y.mantissa)
    else if is_zero y
    then Bigint.(compare x.mantissa zero)
    else (
      let x_sign = Bigint.sign x.mantissa in
      let y_sign = Bigint.sign y.mantissa in
      if Sign.( <> ) x_sign y_sign
      then Sign.compare x_sign y_sign
      else
        scaling_to_least_common_exponent x y ~f:(fun ~lce:_ ~mantissa_a ~mantissa_b ->
          Bigint.compare mantissa_a mantissa_b))
  ;;

  module Stable = struct
    module V2 = struct
      type nonrec t = t =
        { mantissa : Bigint.Stable.V1.t
        ; exponent : int
        }
      [@@deriving sexp]

      (* derived compare would be incorrect here *)
      let compare = compare

      (** [Bigint] does extra allocation in its binary serialization. Do a simpler version
          of what Bignum does and [bin_io] the mantissa as an int, if it fits in an int,
          and otherwise as a Bigint. *)
      module Bin_rep = struct
        module Mantissa = struct
          type t =
            | Int of int
            | Big of Bigint.Stable.V1.t
          [@@deriving bin_io]
        end

        type t =
          { mantissa : Mantissa.t
          ; exponent : int
          }
        [@@deriving bin_io]
      end

      include Binable.Of_binable_without_uuid [@alert "-legacy"]
          (Bin_rep)
          (struct
            type nonrec t = t

            let to_binable { mantissa; exponent } =
              let mantissa =
                let n = Bigint.to_zarith_bigint mantissa in
                if Z.fits_int n
                then Bin_rep.Mantissa.Int (Z.to_int n)
                else Bin_rep.Mantissa.Big mantissa
              in
              { Bin_rep.mantissa; exponent }
            ;;

            let of_binable { Bin_rep.mantissa; exponent } =
              let mantissa =
                match mantissa with
                | Int n -> Bigint.of_int n
                | Big n -> n
              in
              { mantissa; exponent }
            ;;
          end)
    end

    module V3 = struct
      type nonrec t = t =
        { mantissa : Bigint.Stable.V2.t
        ; exponent : int
        }
      [@@deriving bin_io, sexp]

      (* derived compare would be incorrect here *)
      let compare = compare
    end

    let%expect_test "test bin-io digest" =
      let open Expect_test_helpers_core in
      print_and_check_stable_type [%here] (module V2) [];
      [%expect {| (bin_shape_digest 63dd1de06f1a4e923a03de49c676df55) |}];
      print_and_check_stable_type [%here] (module V3) [];
      [%expect {| (bin_shape_digest 4382b358d87f1333d0277d5af9cfa383) |}]
    ;;
  end

  let zero = { mantissa = Bigint.zero; exponent = 0 }

  let canonicalize ~mantissa ~exponent =
    let mantissa = Bigint.to_zarith_bigint mantissa in
    if Z.(equal mantissa zero)
    then zero
    else (
      (* [go ~mantissa ~exponent n] returns [(mantissa, exponent)] such that mantissa is
         not divisible by [10**n], but it may be divisible by [10**k] for some [k < n]. *)
      let rec go ~mantissa ~exponent n =
        let pow_10_z_n = pow_10_z n in
        let div, remainder = Z.div_rem mantissa pow_10_z_n in
        if not Z.(equal remainder zero)
        then mantissa, exponent
        else (
          let r =
            (* Things would still work if instead we did

               {[ let r = go ~mantissa ~exponent (n * 2) ]}

               But since we already went through the hassle of computing [div], why not
               proceed with smaller numbers, saving some work? *)
            go ~mantissa:div ~exponent:(exponent + n) (n * 2)
          in
          (* At this point the highest power of 10 by which mantissa may be divisible is
             [n * 2 - 1].  So it is sufficient to test again whether it is still divisible
             by [10**n] to bring that number down to [n - 1] or less.  *)
          let mantissa, exponent = r in
          let div, remainder = Z.div_rem mantissa pow_10_z_n in
          if Z.(equal remainder zero) then div, exponent + n else r)
      in
      let mantissa, exponent = go ~mantissa ~exponent 1 in
      { mantissa = Bigint.of_zarith_bigint mantissa; exponent })
  ;;

  let scale_by t ~power_of_ten =
    if Bigint.(t.mantissa = zero)
    then t
    else { mantissa = t.mantissa; exponent = t.exponent + power_of_ten }
  ;;

  let create = canonicalize
end

include T
include Stable.V3

let abs { mantissa; exponent } = create ~mantissa:(Bigint.abs mantissa) ~exponent
let neg { mantissa; exponent } = create ~mantissa:(Bigint.neg mantissa) ~exponent
let sign { mantissa; exponent = _ } = Bigint.sign mantissa
let is_zero t = Bigint.(t.mantissa = zero)

let with_mantissas_scaled_to_least_exponent ~f =
  scaling_to_least_common_exponent ~f:(fun ~lce ~mantissa_a ~mantissa_b ->
    create ~mantissa:(f mantissa_a mantissa_b) ~exponent:lce)
;;

module Infix = struct
  let ( * ) x y =
    create
      ~mantissa:(Bigint.( * ) x.mantissa y.mantissa)
      ~exponent:(x.exponent + y.exponent)
  ;;

  let ( + ) x y =
    if x.exponent = y.exponent
    then create ~mantissa:(Bigint.( + ) x.mantissa y.mantissa) ~exponent:x.exponent
    else if is_zero x
    then y
    else if is_zero y
    then x
    else with_mantissas_scaled_to_least_exponent ~f:Bigint.( + ) x y
  ;;

  let ( - ) x y =
    if x.exponent = y.exponent
    then create ~mantissa:(Bigint.( - ) x.mantissa y.mantissa) ~exponent:x.exponent
    else if is_zero x
    then neg y
    else if is_zero y
    then x
    else with_mantissas_scaled_to_least_exponent ~f:Bigint.( - ) x y
  ;;
end

let of_bigint n = create ~mantissa:n ~exponent:0
let of_int n = create ~mantissa:(Bigint.of_int n) ~exponent:0

let to_bignum { mantissa; exponent } =
  let pow_10 n =
    let factor = pow_10 (Int.abs n) |> Bignum.of_bigint in
    if n < 0 then Bignum.(one / factor) else factor
  in
  let factor = pow_10 exponent in
  let mantissa = Bignum.of_bigint mantissa in
  Bignum.(mantissa * factor)
;;

(* Determines the number of digits after the period, returning None if the part after the
   decimal isn't a simple integer, e.g., doesn't match the pattern {v [_1-9]* v}.  This is
   specifically to catch the case where the part after the decimal starts with a ['-'].
*)
let num_decimal_digits_and_mantissa s =
  let not_underscore = function
    | '_' -> false
    | _ -> true
  in
  match String.index s '.' with
  | None -> Some (0, s)
  | Some dot ->
    let decimal_part = String.subo s ~pos:(dot + 1) |> String.filter ~f:not_underscore in
    (* This rejects strings like "123.-123" *)
    if not (String.for_all decimal_part ~f:Char.is_digit)
    then None
    else (
      let num_decimal_digits = String.length decimal_part in
      let int_part = String.subo s ~len:dot in
      Some (num_decimal_digits, int_part ^ decimal_part))
;;

let of_string_base10 s = Bigint.of_zarith_bigint (Z.of_string_base 10 s)

(* [of_string_without_exponent] accepts the following formats.
   - (-|+)?[0-9][0-9_]*.[0-9_]*
   - (-|+)?.[0-9][0-9_]*
*)
let of_string_without_exponent s =
  let unparseable () =
    raise_s [%message "Can't be parsed as Bigdecimal" ~_:(s : string)]
  in
  (* Explicitly disallow strings without any digits as zarith currently accepts [""] and
     ["-"] as zero. That is: [Bigint.(of_string "" = zero) && Bigint.(of_string "-" =
     zero)]. *)
  if not (String.exists s ~f:Char.is_digit) then unparseable ();
  match num_decimal_digits_and_mantissa s with
  | None -> unparseable ()
  | Some (num_decimal_digits, mantissa) ->
    let mantissa =
      try of_string_base10 mantissa with
      | _ -> unparseable ()
    in
    create ~mantissa ~exponent:(Int.neg num_decimal_digits)
;;

let of_string s =
  match
    String.rfindi s ~f:(fun _ c ->
      match c with
      | 'e' | 'E' -> true
      | _ -> false)
  with
  | None -> of_string_without_exponent s
  | Some e_pos ->
    let significand = String.sub s ~pos:0 ~len:e_pos in
    let outer_exponent = String.subo s ~pos:(e_pos + 1) |> Int.of_string in
    let { mantissa; exponent } = of_string_without_exponent significand in
    create ~mantissa ~exponent:(Int.( + ) exponent outer_exponent)
;;

let to_string_no_sn ({ mantissa; exponent } as t) =
  if [%compare.equal: t] t zero
  then "0"
  else (
    let is_neg, mantissa =
      if Bigint.is_negative mantissa then true, Bigint.neg mantissa else false, mantissa
    in
    let mantissa_string = Bigint.to_string mantissa in
    let mantissa_string_length = String.length mantissa_string in
    let decimal_location = Int.( + ) mantissa_string_length exponent in
    let sign = if is_neg then "-" else "" in
    match Ordering.of_int decimal_location with
    | Equal ->
      (* 0.12345 *)
      sign ^ "0." ^ mantissa_string
    | Greater ->
      if exponent < 0
      then (
        (* decimal point inside the mantissa string, e.g. 123.45 *)
        let int_part, decimal_part =
          ( String.slice mantissa_string 0 decimal_location
          , String.slice mantissa_string decimal_location mantissa_string_length )
        in
        sign ^ int_part ^ "." ^ decimal_part)
      else (
        (* right-pad with [exponent] zeroes, e.g. 1234500 *)
        let rpad = String.make exponent '0' in
        sign ^ mantissa_string ^ rpad)
    | Less ->
      (* zeros between decimal and mantissa_string, e.g. 0.0012345 *)
      let num_zeros = -1 * decimal_location in
      let lpad = String.make num_zeros '0' in
      sign ^ "0." ^ lpad ^ mantissa_string)
;;

let to_string_no_sn_grouping ?(sep = ',') t =
  let str = to_string_no_sn t in
  (* now add separators to make it readable *)
  let end_of_int_part =
    Option.value (String.index str '.') ~default:(String.length str)
  in
  let int_digits_and_seps =
    let rec go acc i = function
      | 0 -> acc |> String.of_char_list
      | n ->
        let digit = str.[n - 1] in
        let acc =
          if i % 3 = 0 && i > 0 && Char.( <> ) digit '-'
          then digit :: sep :: acc
          else digit :: acc
        in
        go acc (i + 1) (n - 1)
    in
    go [] 0 end_of_int_part
  in
  int_digits_and_seps ^ String.subo str ~pos:end_of_int_part
;;

let round_to_bigint_internal ~dir t =
  (* Only fails for zero denominator, which can't happen in this case. *)
  Bignum.round_as_bigint_exn ?dir (to_bignum t)
;;

let round_to_bigint ?dir t = round_to_bigint_internal ~dir t
let round ?dir t = create ~mantissa:(round_to_bigint_internal ~dir t) ~exponent:0

let to_int_exn t =
  if is_zero t
  then 0
  else if Int.is_negative t.exponent
  then failwithf !"to_int_exn not integral: %{#no_sn}" t ()
  else (
    (* Use [Bigint.( * )] since [Int.( * )] doesn't raise on overflow *)
    try Bigint.( * ) t.mantissa (pow_10 t.exponent) |> Bigint.to_int_exn with
    | _ -> failwithf !"to_int_exn overflow: %{#no_sn}" t ())
;;

let to_int t =
  try Some (to_int_exn t) with
  | _ -> None
;;

let to_float t = to_string_no_sn t |> Float.of_string
let of_float_short_exn x = Float.to_string x |> of_string
let of_float_short x = Or_error.try_with (fun () -> of_float_short_exn x)

let power_of_ten_which_is_a_multiple_of x =
  (* This function returns [Some (z, 10**z) ] iff
     {[
       2**k * 5**n = x
     ]}
     Otherwise, it returns None.

     In such case when there is a power of 10 which is in fact a multiple of x, we have:
     {[
       2**(k + n) <= x
     ]}
     {[
       k + n <= floor (log2 x)
     ]}
     {[
       x = 2**k * 5**n | 10**(k + n) | 10**(floor(log2 x))
     ]}
     (where "a|b" means "a divides b")
  *)
  let exponent_candidate = Bigint.to_zarith_bigint x |> Zarith.Z.log2 in
  let ten_to_exponent_candidate = pow_10 exponent_candidate in
  if Bigint.(rem ten_to_exponent_candidate x = zero)
  then Some (exponent_candidate, ten_to_exponent_candidate)
  else None
;;

let of_bignum_exn =
  let unrepresentable ~bignum =
    raise_s [%message "Not representable as bigdecimal" ~_:(bignum : Bignum.t)]
  in
  fun bignum ->
    if Bignum.is_zero bignum
    then zero
    else (
      if Bignum.(is_zero (den bignum)) then unrepresentable ~bignum;
      let num = Bignum.num_as_bigint bignum in
      let den = Bignum.den_as_bigint bignum in
      match power_of_ten_which_is_a_multiple_of den with
      | None -> unrepresentable ~bignum
      | Some (exponent, ten_to_exponent) ->
        let mantissa = Bigint.(num * ten_to_exponent / den) in
        create ~mantissa ~exponent:(-exponent))
;;

let of_bignum x = Or_error.try_with (fun () -> of_bignum_exn x)

include Infix

include Sexpable.Of_stringable (struct
    type nonrec t = t

    let of_string = of_string
    let to_string = to_string_no_sn
  end)

include Comparable.Make (struct
    type nonrec t = t [@@deriving sexp]

    let compare = compare
  end)

include Hashable.Make (struct
    type nonrec t = t [@@deriving hash, sexp, compare]
  end)
