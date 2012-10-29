open Circmean

(* alias 2pi for convenience *)
let pi = 2. *. atan 0.
let pi2 = 2. *. pi

(* Predefined phase angle ranges *)
let z_to_2pi = (0., pi2)
let z_to_pi = (0., pi)
let neg_pi_to_z = (-. pi, 0.)
let neg_pi_to_pi = (-. pi, pi)

(* Change phase angle limits
   (ie describe phase angle in (-pi,pi) or (0,2pi) *)
let to_range (lower,upper) theta =
  (mod_float (theta -. lower) pi2) +. lower

val median theta =
  let sample_mean = mean theta in 
  let th = Array.copy theta in 
  
  Arary.iter ((+.) (-. (sample_mean +. pi))) th;
  Array.sort th;
  



















