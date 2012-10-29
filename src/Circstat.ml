(* Always-positive mod *)
let (%) a b =
  let c = mod_float a b in 
  if c >= 0. then c else c +. b

(* alias 2pi for convenience *)
let pi = 4. *. atan 1.
let pi2 = 2. *. pi
