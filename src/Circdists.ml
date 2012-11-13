{-# LANGUAGE ScopedTypeVariables #-}
import Circstat.Base -- for this to work, you need to rename Circbase.ml to Circstat/Base.hs

inv_pi2 = 1 / pi2
inv_sqrt_pi2 = 1 / (sqrt pi2)

--vm_pdf mu kappa t=
--  (exp (kappa * cos (t - mu))) / (pi2 * (Gsl.Sf.bessel_I0 kappa))

cardioid_pdf mu rho t =
  if rho < 0 || rho > 0.5 then error "Cardioid rho parameter must be in [0,1/2]" 
  else
    inv_pi2 * (1 + 2 * rho * cos (t @- mu)) 

{- Note that there's more idiomatic ways of writing this; this is just a straight translation. -}

wrapped_cauchy_pdf mu rho t =
  if rho < 0 || rho > 1 then error "Wrapped cauchy rho must be in [0, 1]"
  else
    inv_pi2 * (1 - rho * rho) /
      (1 + rho * rho - (2 * rho * (cos(t @- mu))))

lin_uniform_pdf bg en t = if t >= bg && t <= en then 1 / (en - bg) else 0

lin_normal_pdf mu sigma_sq t =
  let sigma = sqrt sigma_sq in
  1 * inv_sqrt_pi2 / sigma *
    exp ( (-1) * ((t - mu) ** 2) / (sqrt( 2 * sigma * sigma)) )

data DistParam =
    Constant Float
  | Dependent (Float -> Float)

data Distribution = 
    VonMises (DistParam, DistParam)
  | WrappedNormal (DistParam, DistParam)
  | WrappedCauchy (DistParam, DistParam)
  | Cardioid (DistParam, DistParam)
  | CircUniform
  | LinNormal (DistParam, DistParam)
  | LinUniform (DistParam, DistParam)

{- P(Y|X=x) = N( u=(mx+b), ssq) -}
{- P(Y=y, X=x) = P(Y|X=x)*P(X=x) -}
{- P(Z=z, Y=y, X=x) = P(Z=z | Y=y, X=x)*P(Y=y, X=x) -}

rec eval_pdf (dists :: [Distribution]) (xs :: [Float]) =
  case (zip dists xs) of
    ((dv_dist,dv):tl) ->
      let get_param Constant c = c
          get_param Dependent f = 
                (case xs of 
                    (hd:iv:tl) -> f iv 
                    (hd:[]) -> error ("Requested deeper parameter from" ^
                      " deepest distribution.")
                )
          p_dv_given_iv =
            case dv_dist of
              VonMises (mu, kappa) -> 
                vm_pdf (get_param mu) (get_param kappa) dv
              CircUniform -> inv_pi2
              WrappedNormal (mu, rho) -> 
                error "Wrapped normal distribution not implemented"
              WrappedCauchy (mu, rho) -> 
                wrapped_cauchy_pdf (get_param mu) (get_param rho) dv
              Cardioid (mu, rho) -> 
                cardioid_pdf (get_param mu) (get_param rho) dv
              LinUniform (bg, en) -> 
                lin_uniform_pdf (get_param bg) (get_param en) dv
              LinNormal (mu, sigma_sq) ->
                lin_normal_pdf (get_param mu) (get_param sigma_sq) dv
          p_iv = eval_pdf (tail dists) (tail xs) in 
          p_dv_given_iv * p_iv
        end
    _ -> 1


listify xl = map (\x -> [x]) xl
add_1_to_all x l = map (\li -> x :li) l
add_all_to_all xl l = map (\xli -> add_1_to_all xli l) xl

x_grid (xs :: [[Float]]) = 
  let xs_list = add_all_to_all (xs !! 0) (listify (xs !! 1)) 
  in xs_list

map2d f ll =
  map (\l -> map f l) ll

array_of_list2d ll =
  Array.fromList (map (\l -> Array.of_list l) ll)

norm = LinNormal (Constant 2, Constant 10)
corr_norm = LinNormal (Constant 1, Dependent (\x -> 5 - (abs (x - 2))))
-- x = Array.to_list (Gsl.Vector.to_array (circspace 500))
xs = x_grid [ x, x ]
pdf = map2d (eval_pdf [corr_norm,norm]) xs

{-

cmap x = Graphics.rgb 
  0 (int_of_float (x * 2055.)) (int_of_float (x * 2055.))

let () =
Graphics.open_graph " 500x500"; 

let im = Graphics.make_image (array_of_list2d (map2d cmap pdf)) in

Graphics.draw_image im 0 0



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










-}