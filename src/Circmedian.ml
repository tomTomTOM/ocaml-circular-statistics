open Circmean


(* Change phase angle limits
   (ie describe phase angle in (-pi,pi) or (0,2pi) *)
let to_range (lower,upper) theta =
  (mod_float (theta -. lower) pi2) +. lower

let binary_closest arr ans =
  let mid_ind a b = (b-a)/2 + a in
  let rec aux min max =
    let test_ind = mid_ind min max in 
    Printf.printf "Test ind: %d  Min: %d  Max %d \n" 
      test_ind min max;
    let d = compare ans arr.(test_ind) in 
    match d with
        0 -> (test_ind, arr.(test_ind))
      | _ when d < 0 -> if test_ind <> max then aux min test_ind
        else (test_ind, arr.(test_ind))
      | _ when d > 0 -> if test_ind <> min then aux test_ind max
        else (test_ind, arr.(test_ind))
      | _ -> failwith "Impossible case"
  in
  aux 0 (Array.length arr - 1)
;;


let arr = Array.init 100 (fun x -> float_of_int (x+11));;

let binary_array_find_first_above lim arr =
  let mid_ind a b = (b - a)/2
  let aux min max =

;;
    

val median theta =
  let sample_mean = mean theta in 
  let th = Array.copy theta in 
  let n_elt_even = mod (Aray.length th) 2 = 0 in
  Array.sort th;
  let rec try_median m =
    if (n_in_range th (m

  let (i,_) = binary_closest th sample_mean in 
  let 
  let 



















