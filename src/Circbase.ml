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

let (@-) th_a th_b =
  (((th_a -. th_b) +. pi) % pi2) -. pi

let (@+) th_a th_b =
  (((th_a +. th_b) +. pi) % pi2) -. pi

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

let circspace
    ?(offset = 0.) 
    ?(halfshift = false) 
    ?(start = 0.) ?(stop = pi2) n =
  let spacing = ((stop @- start) % pi2) /. (float_of_int n) in
  let total_offset = 
    if halfshift then 
      offset +. (spacing /. 2.) else
      offset
  in
  Array.init n (fun i -> 
    (((spacing *. (float_of_int i)) +. start) +. total_offset) % pi2)


(* Change phase angle limits
   (ie describe phase angle in (-pi,pi) or (0,2pi) *)
let to_range (lower,upper) theta =
  (mod_float (theta -. lower) pi2) +. lower

let closest_of_this_and_neighbors arr ind th =
  let dist a b = abs_float( a @- b) in
  let closest_ind = ref ind in
  
  if (ind > 0) & 
    ((dist arr.(ind-1) th) <  (dist arr.(!closest_ind) th))
  then closest_ind := (ind-1) else ();

  if (ind < (Array.length arr -1) ) &
    ((dist arr.(ind+1) th) < (dist arr.(!closest_ind) th))
  then closest_ind := (ind+1);
  
  !closest_ind

let ccompare a b =
  if a @= b then 0 else
    if a @< b then -1 else 1
    
let binary_closest_ind arr th =
  let touchup_ind i = closest_of_this_and_neighbors arr i th in
  let mid_ind a b = (b-a)/2 + a in
  let rec aux min max =
    let test_ind = mid_ind min max in 
    let d = ccompare th arr.(test_ind) in 
    match d with
        0 -> touchup_ind test_ind
      | _ when d < 0 -> if test_ind <> max then aux min test_ind
        else touchup_ind test_ind
      | _ when d > 0 -> if test_ind <> min then aux test_ind max
        else touchup_ind test_ind
      | _ -> failwith "Impossible case"
  in
  aux 0 (Array.length arr - 1)
;;

let closest_ind_above arr th =
  (* Assuming arr is sorted ascending *)
  let closest_ind = binary_closest_ind arr th in
  if arr.(closest_ind) @> th then closest_ind else
      let next_ind = if closest_ind = (Array.length arr - 1) then 0
        else closest_ind + 1
      in
      if arr.(next_ind) @> th then next_ind
      else raise Not_found
      

let closest_ind_above_or_equal arr th =
  let closest_ind = binary_closest_ind arr th in
  if arr.(closest_ind) = th then closest_ind else
    closest_ind_above arr th


let closest_ind_below arr th =
  (* Assuming arr is sorted ascending *)
  let closest_ind = binary_closest_ind arr th in
  if arr.(closest_ind) @< th then closest_ind else
    let  next_ind = if closest_ind = 0 then Array.length arr -1
      else closest_ind - 1
    in
      if arr.(next_ind) @< th then next_ind
      else raise Not_found

let closest_ind_below_or_equal arr th =
  let closest_ind = binary_closest_ind arr th in
  if arr.(closest_ind) = th then closest_ind else
    closest_ind_below arr th


let intermediate_points arr =
  (* Add first element of arr again at the end, because we want
     to find the point between arr(last) and arr(first), too *)
  let arr_pad =  Array.concat [arr; [| arr.(1) |] ] in

  (* We're following the circle counterclockwise, so
     all incremental steps have to be in [0,pi2], hence
     the mod by pi2 (-. and @- often give negative diffs) *)

  let dist_to_next arr i = (arr.(i+1) -. arr.(i)) % pi2 in
  Array.init (Array.length arr) 
    (fun i -> arr.(i) @+ ((dist_to_next arr_pad i) /. 2.))


let n_in_ind_range first last total =
  if last >= first then (last - first + 1) else
    (* (total- first) + (last + 1) =    *)
    (total - first + last + 1)

let n_in_theta_range arr th0 th1 =
  try
    let ind0 = closest_ind_above_or_equal arr th0
    and ind1 = closest_ind_below_or_equal arr th1 in
    Printf.printf "ind0: %d  int1: %d\n" ind0 ind1;
    n_in_ind_range ind0 ind1 (Array.length arr)
  with
      Not_found -> Printf.printf "Open interval: reporting ALL points\n"; 
        Array.length arr
    | _ -> failwith "Impossible case"
        


let signi x = if x < 0 then -1 else 1
let signf x = if x < 0. then -1. else 1.

let test_a = circspace ~start:(pi *. 3. /. 2.) ~stop: (pi /. 2.) 100
let test_b = circspace ~start:0. ~stop:pi 10001




















