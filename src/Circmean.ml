let weighted_mean theta_arr weights =
  if (Array.length theta_arr <> Array.length weights) then
    failwith "weighted_mean called with theta_arr and weights different lenghts"
  else
    let (x,y) = (ref 0., ref 0.) in 
    Array.iteri (fun i t -> 
      x := !x +. (weights.(i) *. cos t);
      y := !y +. (weights.(i) *. sin t)) theta_arr;
      Complex.arg {Complex.re = !x; Complex.im = !y}

let mean theta_arr = 
(* slower *)
(*  let weights = Array.make (Array.length theta_arr) 1. in 
  weighted_mean theta_arr weights *)
  
(* faster, but kind of a code copy-paste *)
  let (r,i) = (ref 0., ref 0.) in 
  Array.iter ( fun t ->
    r := !r +. cos t;
    i := !i +. sin t ) theta_arr;
  Complex.arg {Complex.re = !r; Complex.im = !i}




















