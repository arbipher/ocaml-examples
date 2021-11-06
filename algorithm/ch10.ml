open Base
open Lib

let thin_by : ('a -> 'a -> bool) -> 'a list -> 'a list =
  fun (@<=) ts ->
  let bump x = function
    | [] -> [x]
    | y::ys -> 
      if x @<= y then x::ys
      else if y @<= x then y::ys
      else x::y::ys
  in
  foldr bump [] ts

let pair_cmp (a,b) (c,d) = a >= c && b <= d

let%expect_test _ = 
  pr_pair_list @@ thin_by pair_cmp [(1,2);(4,3);(2,3);(5,4);(3,1)];
  [%expect{| [1, 2]; [4, 3]; [5, 4]; [3, 1] |}]

(* 
ys <- thin_by @<= xs
denotes ys is a subsequence of xs, and ∀x.∃y.y @<= x

# 1st law: idempotent
thin_by @<= = (thin_by @<=) @. (thin_by @<=)

# 2nd law: thin introduction
min_with cost = min_with cost @. thin_by @<=
provided by x @<= y => cost x <= cost y

# 3rd law: thin elimination
wrap @. (min_with cost) <- thin_by @<=
provided by cost x <= cost y => x <= y

# 4th law: distribution law
(thin_by @<=) @. concat = (thin_by @<=) @. concant_map (thin_by @<=)
       1                        2                             3

sw: step 2 is for checking the cross list cases e.g.
[ ... ; [ ... ; xn] ; [y0 ; ,,,] ; ... ]

# 5th law: thin-map law
(map f) @. (thin_by @<=) <- (thin_by @<=) @. (map f)
provided by x <= y -> f x <= f y

(thin_by @<=) @. (map f) <- (map f) @. (thin_by @<=)
provided by f x <= f y -> x <= y

(map f) @. (thin_by @<=) == (thin_by @<=) @. (map f)
provided by x <= y <=> f x <= f y

map f @. (thin_by @<=) @. filter p = (thins_by @<=) @. map f @. filter p
provided by p x && p y => (x <= y <=> f x <= f y)

# 6th law: thin-filter law
(thin_by @<=) @. filter p = filter p @. (thin_by @<=)
provided by (x <= y && p y) => p x
*)

