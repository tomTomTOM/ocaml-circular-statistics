open Circstat.Base

type distribution = 
    VonMises of (float * float)
  | WrappedNormal of (float * float)
  | WrappedCauchy of (float * float)
  | Cardioid of (float * float)
  | CircUniform
  | LinNormal of (float * float)
  | LinUniform of (float * float)

let inv_pi2 = 1. /. pi2
let inv_sqrt_pi2 = 1. /. sqrt(pi2)

let pdf dist t =
  match dist with
      VonMises (mu, kappa) -> vm_pdf mu kappa t
    | CircUniform -> inv_pi2
    | WrappedNormal (mu, rho) -> failwith "Wrapped normal distribution not implemented"
    | WrappedCauchy (mu, rho) -> wrapped_cauchy_pdf mu rho t
    | Cardioid (mu, rho) -> cardioid_pdf mu rho t
    | LinUniform (bg, en) -> lin_uniform_pdf bg en t


let vm_pdf mu kappa t=
  (exp (kappa *. cos (t -. mu))) /. (pi2 *. (Gsl.Sf.bessel_I0 kappa))

let cardioid_pdf mu rho t =
  if rho < 0. || rho > 0.5 then failwith "Cardioid rho parameter must be in [0,1/2]" 
  else
    inv_pi2 *. (1. +. 2. *. rho *. cos (t @- mu))

let wrapped_cauchy_pdf mu rho t =
  if rho < 0. || rho > 1. then failwith "Wrapped cauchy rho must be in [0, 1]"
  else
    inv_pi2 *. (1. -. rho *. rho) /. 
      (1. +. rho *. rho -. (2. *. rho *. (cos(t @- mu))))

let lin_uniform_pdf bg en t = if t >= bg && t <= en then 1. /. (en -. bg) else 0.

let lin_normal_pdf mu sigma t =
  1. *. inv_sqrt_pi2 /. sigma *. 
    exp ( (-. 1.) *. ((t -. mu) ** 2.) /. (sqrt( 2. *. sigma *. sigma)) )


(*

(* TODO: Get cdf's for these distributions, fit the distributions
  to data (or should this be in another module?) *)

(* What about conditional RV's? *)

let reference_cdf_VM = ref (None : vec option)

module Cdf = struct
  type t = { dist:distribution ;
             theta: float array ;
             pdf: (float -> float);
             cdf: float array }
  let equal a b = 
    a.distribution = b.distribution && 
    a.params = b.params
  let hash a =
    Hashtbl.hash (a.distribution, a.params)
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

module Ctable = Weak.Make (Cdf)


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




  *)










