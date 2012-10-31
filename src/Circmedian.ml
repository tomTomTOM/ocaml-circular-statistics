open Circbase
open Circinterval
open Circmean


let arr = Array.init 99 (fun x -> float_of_int (x+11));;
let a = circspace ~start: (pi *. 3. /. 2.) ~stop: (pi /. 2.) 100;;


let median arr =

  let sample_mean = mean arr in 
  let th = Array.copy arr in 
  Array.fast_sort ccompare th;

  let samp_points = if ((Array.length th) mod 2 = 1) then th 
    else (intermediate_points th) in

  let forward_int (ind: int) = mk_interval 
   (samp_points.(ind)) Closed
   ((samp_points.(ind)) @+ pi) Closed
  and backward_int (ind: int) = mk_interval
    ((samp_points.(ind)) @- pi) Closed
    (samp_points.(ind)) Closed in
  let div_imbalance (ind: int) = (n_in_interval arr (forward_int ind)) -
    (n_in_interval arr (backward_int ind)) in
 
  let rec aux test_ind n_tries =
    let di = div_imbalance test_ind in

    Printf.printf "Try: %d  ind:%d  fwd:%s  bwd:%s  di:%d\n" n_tries test_ind (string_of_interval (forward_int test_ind))
      (string_of_interval (backward_int test_ind)) di;

    match di with 
        0 -> samp_points.(test_ind)
      | _ ->  let step = if (n_tries < 100) then (di+(2*(signi di)))/3  else 1  in
              aux ((test_ind+step) mod (Array.length samp_points)) (n_tries + 1)
  in

  aux (binary_closest_ind samp_points sample_mean) 0 
      
;;
        
let c = 1


















