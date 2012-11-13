-- Unfortunately, I can't build packages right now on my local machine, 
--   so I had to make a couple final changes without the aid of the 
--   typechecker. Should be close to working.

-- Of course, first you have to rename this file with a .hs extension
import Text.Printf (printf)
import Data.Fixed (mod')
-- this is where all the Vector and Matrix libraries are. You'll have to run "cabal install hmatrix"
import Data.Packed 

-- Always-positive mod
(%) a b = -- note you can also write this as "a % b = "
  let c = mod' a b in 
  if c >= 0 then c else c + b

-- alias 2pi for convenience
--pi = 4 * (atan 1) -- ...but pi is already defined in Haskell
pi2 = 2 * pi

--module V = Gsl.Vector
--module M = Gsl.Matrix

type PhaseVector = Data.Packed.Vector
type Vec = Data.Packed.Vector
type Mat = Data.Packed.Matrix

{- Circular comparisons
   a < b when linear b is greater
   than linear a, until it becomes
   greater by pi.  When b is ahead
   of a by pi or more, then a is 
   greater -}
(@<) th_a th_b =
  let th_diff = (th_b - th_a) % pi2 in
  (th_diff > 0) && (th_diff < pi)

(@=) th_a th_b =
  (th_a == th_b) ||
    ((th_a - th_b) % pi2 == 0)

(@<=) th_a th_b =
  (th_a @< th_b) || 
    (th_a @= th_b)

(@>) th_a th_b =
  let th_diff = (th_b - th_a) % pi2 in
  (th_diff > pi) && (th_diff < pi2)

(@>=) th_a th_b =
  (th_a @> th_b) ||
    (th_a @= th_b)

(@-) th_a th_b =
  (((th_a - th_b) + pi) % pi2) - pi

(@+) th_a th_b =
  (((th_a + th_b) + pi) % pi2) - pi

comparison_test_cases = [
  (0, 0, (@=), True),
  (1, 1 + ( 6 * pi), (@=), True),
  (1, 2, (@<=), True),
  (1, 2, (@<=), True),
  (2, 0.1, (@<=), False),
  (0.1, 4, (@<), False),
  (5, 0.1, (@>), False),
  (3, 5, (@<=), True),
  (3, 6.05, (@<=), True)
  ]

-- "foldl (&&) true" can better be written as "foldl1 (&&)"
comparison_test () = 
  let res = foldl (&&) True (map (\(a,b,c,r) -> c a b == r) comparison_test_cases) -- in any language, "a == True" can just be written as "a"
  in if res then () else
     error "Failed circular comparison unit test."

{-
phase_vector_set_by_fi f (arr :: PhaseVector) = 
  for n = 0 to (V.length arr - 1) do
    arr.{n} <- f n
  done -- this whole function would be replaced by Vector.map


phase_vector_init n f =
  let arr = V.Create n in
  phase_vector_set_by_fi f arr;
  arr
  
phase_vector_iter f arr =
  for n = 0 to (V.length arr -1 ) do
    f arr.{n}
  done

phase_vector_iteri f arr =
  for n = 0 to (V.length arr -1 ) do
    f n arr.{n}
  done
  -}
phase_vector_print arr = do
  phase_vector_iter (\x -> printf "%f " x) arr -- replaced with a map
  putStrLn ""
    {-
-- Built this way first, then the following way, to test speed
phase_vector_fold_left_a f i arr =
  let rec aux f subarr accum =
    if null subarr then accum else --V.is_
     aux f (V.subvector subarr 1 (V.length subarr - 1)) (f accum subarr.{0})
  in
    aux f arr i

-- This one was a bit faster
phase_vector_fold_left f i arr =
  let a = ref i in
  for n = 0 to (V.length arr - 1) do
    a := f !a arr.{n}
  done;
  !a

phase_vector_map f arr =
  let a = V.create (V.length arr) in
  for n = 0 to (V.length arr) - 1 do
    a.{n} <- (f arr.{n})
  done;
  a

phase_vector_mapi f arr =
  let a = V.create (V.length arr) in 
  for n = 0 to (V.length arr) - 1 do
    a.{n} <- (f n arr.{n})
  done;
  a

-- throwaway profile function 
timer n f arg =
  let t0 = ref (Unix.gettimeofday ()) in
  for i = 1 to n do
    f arg
  done;
  ((Unix.gettimeofday ()) - !t0) / (float_of_int n)
-}
circspace offset halfshift addfinal start stop n = -- Haskell doesn't have default arguments. I rarely miss em
{-
    ?(offset = 0) 
    ?(halfshift = False) 
    ?(addfinal = False)
    ?(start = 0) ?(stop = pi2) n = -}
  let aux_n = if not addfinal then n else (n-1)
      spacing = (stop - start) / (fromIntegral aux_n)
      total_offset = 
        if halfshift then 
        offset + (spacing / 2) else
        offset
  in
  phase_vector_init n (\i -> 
    (((spacing * (fromIntegral i)) + start) + total_offset) % pi2)



-- Unused? 
{- Change phase angle limits
   (ie describe phase angle in (-pi,pi) or (0,2pi) -}
to_range (lower,upper) theta =
  (mod' (theta - lower) pi2) + lower
{-
closest_of_this_and_neighbors arr ind th =
  let dist a b = abs_float( a @- b) in
  let closest_ind = ref ind in
  
  if (ind > 0) & 
    (dist arr.{ind-1} th) <  (dist arr.{!closest_ind} th)
  then closest_ind := (ind-1) else ();

  if (ind < (V.length arr -1) ) &
    (dist arr.{ind+1} th) < (dist arr.{!closest_ind} th)
  then closest_ind := (ind+1);
  
  !closest_ind
-}
ccompare a b = -- this could be written much more easily with Haskell typeclasses
  if a @= b then 0 else
    if a @< b then -1 else 1
{-    
binary_closest_ind (arr: phase_vector) th =
  let touchup_ind i = closest_of_this_and_neighbors arr i th in
  let mid_ind a b = (b-a)/2 + a in
  let rec aux min max =
    let test_ind = mid_ind min max
        d = ccompare th arr.{test_ind} 
    in case d of
        0 -> touchup_ind test_ind
      | _ when d < 0 -> if test_ind <> max then aux min test_ind
        else touchup_ind test_ind
      | _ when d > 0 -> if test_ind <> min then aux test_ind max
        else touchup_ind test_ind
      | _ -> failwith "Impossible case"
  in
  aux 0 (V.length arr - 1)
;;

closest_ind_above arr th =
  -- Assuming arr is sorted ascending 
  let closest_ind = binary_closest_ind arr th in
  if arr.{closest_ind} @> th then closest_ind else
      let next_ind = if closest_ind == (length arr - 1) then 0 -- V.
        else closest_ind + 1
      in
      if arr.{next_ind} @> th then next_ind
      else raise Not_found
      

closest_ind_above_or_equal arr th =
  let closest_ind = binary_closest_ind arr th in
  if arr.{closest_ind} = th then closest_ind else
    closest_ind_above arr th


closest_ind_below arr th =
  -- Assuming arr is sorted ascending 
  let closest_ind = binary_closest_ind arr th in
  if arr.{closest_ind} @< th then closest_ind else
    let  next_ind = if closest_ind = 0 then V.length arr -1
      else closest_ind - 1
    in
      if arr.{next_ind} @< th then next_ind
      else raise Not_found

closest_ind_below_or_equal arr th =
  let closest_ind = binary_closest_ind arr th in
  if arr.{closest_ind} = th then closest_ind else
    closest_ind_below arr th


intermediate_points arr =
  {- Add first element of arr again at the end, because we want
     to find the point between arr(last) and arr(first), too -}
  let arr_pad =  V.create (V.length arr + 1) in
  arr_pad.{V.length arr_pad - 1} <- arr_pad.{0};
  {- We're following the circle counterclockwise, so
     all incremental steps have to be in [0,pi2], hence
     the mod by pi2 (-. and @- often give negative diffs) -}
  let dist_to_next i = (arr_pad.{i+1} -. arr_pad.{i}) % pi2 in
  phase_vector_mapi (\i x -> x + (dist_to_next i) / 2) arr



test_a = circspace ~start:(pi * 3. /. 2.) ~stop: (pi /. 2.) 100
test_b = circspace ~start:0. ~stop:pi 10001

-}


















