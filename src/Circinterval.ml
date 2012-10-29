type limit_style = Open | Closed
type circ_limit = { lim: float;
                    style: limit_style }

(* Interval interior is always the space traced
   from t0 to t1, counterclockwise *)
type circ_interval = {t0 : circ_limit; t1 : circ_limit}

let mk_interval a_v a_s b_v b_s =
  { t0= {lim=a_v; style=a_s};
    t1= {lim=b_v; style=b_s} }

let rev_interval interval = {t0 = interval.t1; t1= interval.t0}

let in_interval interval th = 
  let t0_cmp = match interval.t0.style with Open -> (>=) | Closed -> (>)
  and t1_cmp = match interval.t1.style with Open -> (<=) | Closed -> (<)
  in
  let t0_ok = t0_cmp (th % pi2) (interval.t0.lim % pi2)  
  and t1_ok = t1_cmp ((th -. interval.t0.lim) % pi2) 
       ( (interval.t1.lim -. interval.t0.lim) % pi2) in
  (t0_ok & t1_ok) || (interval.t1.lim -. interval.t0.lim) = pi2 

  

(* Predefined phase angle ranges *)
let z_to_2pi = mk_interval 0. Open pi2 Open
let z_to_pi = mk_interval 0. Open pi Open
let neg_pi_to_z = mk_interval (-. pi) Open 0. Open
let neg_pi_to_pi = mk_interval (-. pi) Open pi Open
