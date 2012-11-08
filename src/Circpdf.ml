open Circstat.Base

let pdf_VM mu kappa t=
  (exp (kappa *. cos (t -. mu))) /. (pi2 *. (Gsl.Sf.bessel_I0 kappa))

(* Premature optimization 
let mk_memo_pdf_VM ?(mu = 0.) ?(kappa = 0.) =
  let coef = (1. /. (pi2 *. (Gsl.Sf.bessel_I0 kappa))) in
  fun t -> coef *. (exp (kappa *. cos(t -. mu)))
*)

let reference_cdf_VM = ref (None : vec option)

module Cdf : sig
  type t = (float *  float * float)
  val equal : t -> t -> bool
  val hash : t -> int
end = struct
  type t = {mu:float ; kappa: float; precision: float; xs:vec ; data:vec}
  let equal a b = 
    (abs_float(a.mu - b.mu) < 0.000001) & 
      (abs_float(a.kappa - b.kappa) < 0.000001) &
      (abs_float (a.precision - b.precision) < 0.000001)
  let hash a =
    Hashtbl.hash (a.mu, a.kappa, a.precision)
end

module  Cdf_VM = struct
  type key_t = {kappa: float ; precesion:float}
  type t = {key:key_t; xs: vec; data:vec}
  let create kappa precision =
    let x = circspace ~start:(0. -. pi) ~stop:pi (pi2 /. precision) in
    phase_vector_map 
  let equal a b = (a.key = b.key)
  let hash t = Hashtbl.hash t.key
end

module AbstCdf = (Cdf : Hashtbl.HashedType)

module C_table = Weak.Make (Cdf : Hashtbl.HashedType)

module Ctable = Weak.Make (Cdf)


module C_table = Hashtbl.Make (Cdf)


module Cdf = struct
  type t = (float * float * float)
  let equal = (=)
  let hash t = Hashtbl.hash t
end

module type M = (Cdf : Hashdable.HashedType)

module Cdf_VM_hash = Weak.Make (Cdf : Hashtbl.HashedType)

let cdf_VM ?(precision = 0.01) mu kappa 

(* Throw-away timing function *)
let time n f a =
  let t0 = ref (Unix.gettimeofday () ) in
  for n = 1 to n do
    f a
  done;
  (Unix.gettimeofday () -. !t0) /. (float_of_int) n















