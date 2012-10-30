(* Always-positive mod *)
let (%) a b =
  let c = mod_float a b in 
  if c >= 0. then c else c +. b

(* alias 2pi for convenience *)
let pi = 4. *. atan 1.
let pi2 = 2. *. pi

(* Circular comparisons
   a < b when linear b is greater
   than linear a, until it becomes
   greater by pi.  When b is ahead
   of a by pi or more, then a is 
   greater *)
let (@<) th_a th_b =
  let th_diff = (th_b -. th_a) % pi2 in
  (th_diff > 0.) & (th_diff < pi)

let (@=) th_a th_b =
  (th_a = th_b) ||
    (th_a -. th_b) % pi2 = 0.

let (@<=) th_a th_b =
  (th_a @< th_b) || 
    (th_a @= th_b)

let (@>) th_a th_b =
  let th_diff = (th_b -. th_a) % pi2 in
  (th_diff > pi) & (th_diff < pi2)

let (@>=) th_a th_b =
  (th_a @> th_b) ||
    (th_a @= th_b)



let comparison_test_cases = [
(0., 0., (@=), true);
(1., 1. +. ( 6. *. pi), (@=), true);
(1., 2., (@<=), true);
(1., 2., (@<=), true);
(2., 0.1, (@<=), false);
(0.1, 4., (@<), false);
(5., 0.1, (@>), false);
(3., 5., (@<=), true);
(3., 6.05, (@<=), true);
]

let comparison_test () = 
  let res =
  List.fold_left (&)
    true
    (List.map (fun (a,b,c,r) -> (c a b = r) = true) comparison_test_cases)
  in
  if res then () else
    failwith "Failed circular comparison unit test."




















