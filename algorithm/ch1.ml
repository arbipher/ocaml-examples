open Base
open Lib

(* intuition 1: 
  insert x at beginning, 
  and inserting x on ys with an extra header y *)
(* intuition 2: 
  inserting x on ys as if there is no y,
  map y as header;
  the only exception is x::y::ys *)
let rec inserts : 'a -> 'a list -> 'a list list = fun x yl ->
  match yl with
  | [] -> [[x]]
  | y::ys -> (x::y::ys) :: (List.map (inserts x ys) ~f:((@:) y))

let%test _ = 
  Poly.equal (inserts 1 []) [[1]]

let%expect_test _ =
  prll_sp @@ inserts 1 [2;3];
  [%expect{| [1 2 3]; [2 1 3]; [2 3 1] |}]

let%test _ = 
  Poly.equal (inserts 1 [2;3]) [[1;2;3];[2;1;3];[2;3;1]]

(* val concat_map : 'a t -> f:('a -> 'b t) -> 'b t *)

let step x xss = List.concat_map xss ~f:(inserts x)

let perms1 y = List.fold_right y ~f:step ~init:[[]]

(* intuition : 
  perms [x1; x2; x3] =
  concat_map (
    concat_map (
      concat_map [] ~f:(inserts x3)
      ~f:(inserts x2)))
    ~f:(inserts x1))
  *)
let perms1 y = List.fold_right y ~f:(fun x xss -> List.concat_map xss ~f:(inserts x)) ~init:[[]]

(* let%expect_test _ =
  prll @@ perms1 [1;2;3];
  [%expect{| |}] *)

let%test _ = 
  Poly.equal (perms1 [1;2;3]) [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]]

let rec picks : 'a list -> ('a * 'a list) list = function
  | [] -> []
  | x::xs -> (x, xs) :: (List.bind (picks xs) ~f:(fun (y, ys) -> [y, (x::ys)]))

(* intuition:
  either pick x, with rest xs,
  or picks from xs
    for each y, ys picked,
    keep y and extend ys as x::ys
 *)
let rec picks : 'a list -> ('a * 'a list) list = 
  let open List.Monad_infix in
  function
  | [] -> []
  | x::xs -> (x, xs) :: ((picks xs) >>= (fun (y, ys) -> [y, (x::ys)]))

let%expect_test _ =
  prpll_sp @@ picks [1;2;3];
  [%expect{| [1, [2 3]]; [2, [1 3]]; [3, [1 2]] |}]

(* intuition:
  subperms:
    perms ys, adding header x
  perms:
    emunarate each picks
*)

let rec subperms (x, ys) = 
  List.map (perms2 ys) ~f:((@:) x)
and perms2 = function
  | [] -> [[]]
  | xs -> List.concat_map (picks xs) ~f:subperms

(* notes:
perms1, recursive, Insertion sort, divide-and-conquer
perms2, inductive, Selection sort, greedy and thinning algorithms
 *)

let rec until : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a = 
  fun p f x ->
    if p x then
      x
    else
      until p f (f x)

let while_ p = until (fun x -> not @@ p x)

let duplicate x = [x; x]

let inc1 x = x + 1

let inc2 x = x + 2


(* (map f) . (map g) = map (f . g) *)
let%test _ =
  let f = inc1 in 
  let g = inc2 in
  let lhs = (fun x -> List.map x ~f) @. (fun x -> List.map x ~f:g) in
  let rhs = (fun x -> List.map x ~f:(f @. g) ) in
  let x = [1;2;3] in
  Poly.equal (lhs x) (rhs x)

(* concatMap f . map g = concatMap (f . g) *)
let%test _ =
  let f = duplicate in
  let g = inc1 in
  let lhs = (fun x -> List.concat_map x ~f) @. (fun y -> List.map y ~f:g) in
  let rhs = (fun x -> List.concat_map x ~f:(f @. g)) in
  let x = [1;2;3] in
  Poly.equal (lhs x) (rhs x)

(* foldr f e . map g = foldr (f . g) e *)
let%test _ =
  let f = (+) in
  let g = inc1 in
  let lhs e = (fun x -> List.fold_right x ~f ~init:e) @. (fun y -> List.map y ~f:g) in
  let rhs e = (fun x -> List.fold_right x ~f:(f @. g) ~init:e) in
  let x = [1;2;3] in
  let init = 0 in
  Poly.equal (lhs init x) (rhs init x)

(* foldr f e . concat = foldr f e . ???? *)

(* 
Fusion Law
  h (foldr f e xs) = foldr g (h e) xs
for all finite lists xs provided
  h (f x y) = g x (h y)
called _fusion condition_
 *)

(* intuision
group isomorphism
h (f x e) = g x (h e)
h (f x1 (f x2 e)) = g x1 (g x2 (h e))
vaguely, 

h e --- g_x --> h (f x e)
 |                |
 e  --- f_x --> (f x e)
 *)

(* notes:
f . concat = concat . map f
concat . map = concatMap
map f . map g = map (f . g)
 *)

let sum xs = List.sum (module Int) xs ~f:id

let collapse : int list list -> int list =
  let rec help xs xss = 
    if sum xs > 0 || null xss then xs
    else help (xs ++ head xss) (tail xss)
    in
  fun xss -> help [] xss

let test_collapase f =
  Poly.equal (f [[1];[-3];[2;4]]) [1]
  && Poly.equal (f [[-2;1];[-3];[2;4]]) [-2;1;-3;2;4]
  && Poly.equal (f [[-2;1];[3];[2;4]]) [-2;1;3]

let%test _ = test_collapase collapse

let labelsum xss = zip (map sum xss) xss
let collapse =
  let cat (s,xs) (t,ys) = (s + t, xs ++ ys) in
  let rec help (s, xs) xss = 
    if s > 0 || null xss then
      xs
    else
      help (cat (s,xs) (head xss)) (tail xss) 
    in
  fun xss -> help (0, []) (labelsum xss)

let%test _ = test_collapase collapse

let collapse =
  let rec help (s,f) xss = 
    if s > 0 || null xss then
      f
    else
      let (t,xs) = head xss in
      help ((s+t), f @. ( (++) xs)) (tail xss)
    in
  fun xss -> (help (0,id)) (labelsum xss) []

let%test _ = test_collapase collapse