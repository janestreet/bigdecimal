open Core
open Int.Replace_polymorphic_compare
open Stable_witness.Export
module Z = Zarith.Z

let pow_10_z = Bignum.For_bigdecimal.pow_10
let pow_10 n = pow_10_z n |> Bigint.of_zarith_bigint

let pow_10_bignum n =
  if n >= 0
  then pow_10 n |> Bignum.of_bigint
  else (
    let denom = pow_10 (abs n) |> Bignum.of_bigint in
    Bignum.(one / denom))
;;

module T : sig @@ portable
  (** This represents the decimal: mantissa * 10 ^ exponent. An invariant of the type is
      that the mantissa is either zero or an integer not divisible by 10. Also, it's
      guaranteed that any two distinct decimal numbers will have distinct representations,
      which requires in addition that zero is always expressed with an exponent of zero. *)
  type t = private
    { mantissa : Bigint.t
    ; exponent : int
    }
  [@@deriving hash, compare ~localize, typerep]

  val zero : t
  val scale_by : t -> power_of_ten:int -> t
  val create : mantissa:Bigint.t -> exponent:int -> t
  val mantissa : local_ t -> Bigint.t
  val exponent : local_ t -> int

  val scaling_to_least_common_exponent
    :  t @ local
    -> t @ local
    -> f:(lce:int -> mantissa_a:Bigint.t -> mantissa_b:Bigint.t -> 'a)
    -> 'a

  module Stable : sig
    module V2 : sig
      type nonrec t = t [@@deriving bin_io, compare, equal, hash, stable_witness]

      module Structural_sexp : sig
        type nonrec t = t [@@deriving sexp]
      end
    end

    module V3 : sig
      type nonrec t = t [@@deriving bin_io, compare, equal, hash, stable_witness]

      module Structural_sexp : sig
        type nonrec t = t [@@deriving sexp]
      end
    end

    module V4 : sig
      type nonrec t = t [@@deriving bin_io, compare, equal, hash, stable_witness]

      module Structural_sexp : sig
        type nonrec t = t [@@deriving sexp]
      end
    end
  end
end = struct
  (* Invariant: [mantissa] is either zero or an integer not divisible by 10. *)
  type t =
    { mantissa : Bigint.t
    ; exponent : int
    }
  [@@deriving hash, typerep]

  let mantissa (local_ t) : Bigint.t = t.mantissa
  let exponent (local_ t) : int = t.exponent

  (* derived compare would be incorrect here *)

  let scaling_to_least_common_exponent a b ~f =
    let lce = min a.exponent b.exponent in
    let scale_mantissa { mantissa; exponent } =
      Bigint.(mantissa * pow_10 (Int.( - ) exponent lce))
    in
    f ~lce ~mantissa_a:(scale_mantissa a) ~mantissa_b:(scale_mantissa b)
  ;;

  let is_zero t = Bigint.(t.mantissa = zero)

  let%template compare (x @ m) (y @ m) =
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
  [@@mode m = (global, local)]
  ;;

  let equal = [%compare.equal: t]

  module Stable = struct
    module V2 = struct
      type nonrec t = t =
        { mantissa : Bigint.Stable.V1.t
        ; exponent : int
        }
      [@@deriving stable_witness]

      (* derived compare would be incorrect here *)
      let compare = compare
      let equal = equal
      let hash = hash
      let hash_fold_t = hash_fold_t

      module Structural_sexp = struct
        type nonrec t = t =
          { mantissa : Bigint.Stable.V1.t
          ; exponent : int
          }
        [@@deriving sexp]
      end

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

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"] [@modality portable]
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
      [@@deriving bin_io, stable_witness]

      (* derived compare would be incorrect here *)
      let compare = compare
      let equal = equal
      let hash = hash
      let hash_fold_t = hash_fold_t

      module Structural_sexp = struct
        type nonrec t = t =
          { mantissa : Bigint.Stable.V2.t
          ; exponent : int
          }
        [@@deriving sexp]
      end
    end

    (* The only difference between V3 and V4 is [Sexpable.S], and we add that later in the
       file. *)
    module V4 = V3
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

               {[
                 let r = go ~mantissa ~exponent (n * 2)
               ]}

               But since we already went through the hassle of computing [div], why not
               proceed with smaller numbers, saving some work? *)
            go ~mantissa:div ~exponent:(exponent + n) (n * 2)
          in
          (* At this point the highest power of 10 by which mantissa may be divisible is
             [n * 2 - 1]. So it is sufficient to test again whether it is still divisible
             by [10**n] to bring that number down to [n - 1] or less. *)
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
include Stable.V4

let one = create ~mantissa:Bigint.one ~exponent:0
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
  let factor = pow_10_bignum exponent in
  let mantissa = Bignum.of_bigint mantissa in
  Bignum.(mantissa * factor)
;;

(* Determines the number of digits after the period, returning None if the part after the
   decimal isn't a simple integer, e.g., doesn't match the pattern
   {v [_1-9]* v}
   . This is specifically to catch the case where the part after the decimal starts with a
   ['-'].
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

(*=[of_string_without_exponent] accepts the following formats.
   - (-|+)?[0-9][0-9_]*.[0-9_]*
   - (-|+)?.[0-9][0-9_]*
*)
let of_string_without_exponent s =
  let unparseable () =
    raise_s [%message "Can't be parsed as Bigdecimal" ~_:(s : string)]
  in
  (* Explicitly disallow strings without any digits as zarith currently accepts [""] and
     ["-"] as zero. That is:
     [Bigint.(of_string "" = zero) && Bigint.(of_string "-" = zero)]. *)
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
  if ([%compare.equal: t] [@mode local]) t zero
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
  then failwith ("to_int_exn not integral: " ^ to_string_no_sn t) [@nontail]
  else (
    (* Use [Bigint.( * )] since [Int.( * )] doesn't raise on overflow *)
    try Bigint.( * ) t.mantissa (pow_10 t.exponent) |> Bigint.to_int_exn with
    | _ -> failwith ("to_int_exn overflow: " ^ to_string_no_sn t) [@nontail])
;;

let to_int t =
  try Some (to_int_exn t) with
  | _ -> None
;;

let to_float t = to_string_no_sn t |> Float.of_string
let of_float_short_exn x = Float.to_string x |> of_string
let of_float_short x = Or_error.try_with (fun () -> of_float_short_exn x)

let power_of_ten_which_is_a_multiple_of x =
  (*=This function returns [Some (z, 10**z) ] iff
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

let div ?(decimals_precision = 15) ?rounding_dir a b =
  let rounding_dir =
    (* We do this so that the mli doesn't need to have the [> `Nearest] restriction added
       to it. *)
    Option.value
      (rounding_dir :> [ `Zero | `Down | `Up | `Nearest | `Bankers ] option)
      ~default:`Nearest
  in
  (* If a = m * 10^p and b = n * 10^q, then

     a/b = u * 10^r, where

     r = p - q, and u = m / n.

     We compute m/n using Bignum.round_decimal to [d] digits, where [d] =
     [decimals_precision + r]. The reason is that the result is [u] shifted left by [r]
     decimals, so to keep [decimals_precision] decimals after the decimal point, we
     compute [m/n] to [decimals_precision + r] places. If [r < 0] then
     [d < decimals_precision]: we compute [m/n] to fewer digits because we're going to
     shift-right by [abs(r)] afterwards. *)
  let result_exponent = a.exponent - b.exponent in
  let result_mantissa =
    let digits = decimals_precision + result_exponent in
    Bignum.( / ) (Bignum.of_bigint a.mantissa) (Bignum.of_bigint b.mantissa)
    |> Bignum.round_decimal ~dir:rounding_dir ~digits
    |> of_bignum_exn
  in
  scale_by result_mantissa ~power_of_ten:result_exponent
;;

let scale_int t n =
  create ~mantissa:(Bigint.( * ) t.mantissa (Bigint.of_int n)) ~exponent:t.exponent
;;

let round_to_power_of_ten ?dir t ~power_of_ten =
  if t.exponent >= power_of_ten
  then t
  else (
    let mantissa =
      let pow10 = pow_10_bignum (power_of_ten - t.exponent) in
      let num = Bignum.of_bigint t.mantissa in
      Bignum.( / ) num pow10 |> Bignum.round_as_bigint_exn ?dir
    in
    create ~mantissa ~exponent:power_of_ten)
;;

let log10_int_exact { mantissa; exponent } =
  (* [mantissa] is either zero or an integer not divisible by 10. *)
  if Bigint.equal mantissa Bigint.one then Some exponent else None
;;

let[@cold] raise__sqrt_of_negative_number t =
  raise_s [%message "Bigdecimal.sqrt got negative argument" (t : Structural_sexp.t)]
;;

let two = of_int 2
let is_even n = Int.(n % 2 = 0)

let sqrt ?(decimals_precision = 15) t =
  if Bigint.is_negative t.mantissa then raise__sqrt_of_negative_number t;
  if is_zero t
  then zero
  else if Bigint.(t.mantissa = one) && is_even t.exponent
  then
    (* if t = 10^(2*k), then sqrt(t) = 10^k *)
    create ~mantissa:t.mantissa ~exponent:(Int.( / ) t.exponent 2)
  else (
    (*=Babylonian method for computing sqrt
       (https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method)

       To compute sqrt(a) to [d] decimal digits of precision:

       x_0 = approximate_sqrt(a)

       and repeat:
       x_(n+1) = (x_n + (a / x_n)) / 2

       until |x_(n+1) - x_n| < 10^-d

       In order for the result to be accurate to [d] decimals, the division needs to be
       accurate to [d + 1] decimals (addition is exact). *)
    let precision = create ~mantissa:Bigint.one ~exponent:(Int.neg decimals_precision) in
    let[@inline] ( / ) a b = div ~decimals_precision:(decimals_precision + 1) a b in
    let x0 = ref zero in
    let x1 = ref (Float.sqrt (to_float t) |> of_float_short_exn) in
    let open Infix in
    let too_far () =
      let diff = abs (!x0 - !x1) in
      compare diff precision >= 0
    in
    while too_far () do
      x0 := !x1;
      x1 := ((t / !x0) + !x0) / two
    done;
    round_to_power_of_ten ~dir:`Nearest !x1 ~power_of_ten:(Int.neg decimals_precision))
;;

let ( ** ) t pow =
  (* Bigint.( ** ) raises a reasonable-looking exception if the power is negative *)
  create
    ~mantissa:(Bigint.( ** ) t.mantissa (Bigint.of_int pow))
    ~exponent:(Int.( * ) t.exponent pow)
;;

let of_bignum x = Or_error.try_with (fun () -> of_bignum_exn x)
let is_integral t = t.exponent >= 0

let to_bigint_exact_exn t =
  if not (is_integral t)
  then raise_s [%message "to_bigint_exact_exn: not an integer" (t : Structural_sexp.t)];
  Bigint.( * ) t.mantissa (Bigint.( ** ) (Bigint.of_int 10) (Bigint.of_int t.exponent))
;;

let to_bigint_exact t = Option.try_with (fun () -> to_bigint_exact_exn t)

include Infix

module String_sexp = Sexpable.Of_stringable [@modality portable] (struct
    type nonrec t = t

    let of_string = of_string
    let to_string = to_string_no_sn
  end)

include String_sexp

include Comparable.Make [@modality portable] (struct
    type nonrec t = t [@@deriving sexp]

    let compare = compare
  end)

include Hashable.Make [@modality portable] (struct
    type nonrec t = t [@@deriving hash, sexp, compare]
  end)

module Stable = struct
  module V2 = struct
    include Stable.V2
    include Stable.V2.Structural_sexp
  end

  module V3 = struct
    include Stable.V3
    include Stable.V3.Structural_sexp
  end

  module V4 = struct
    include Stable.V4
    include String_sexp
  end
end
