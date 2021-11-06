open Base
open Lib

let%test _ =
  (foldr1 (+) [1;2;3]) = 6

let%test _ =
  (foldl1 (+) [1;2;3]) = 6

let partition3 y = foldr (fun x (us,vs,ws) ->
    if x < y then x::us, vs, ws
    else if x = y then us, x::vs, ws
    else (* x > y *) us, vs, x::ws
  ) ([],[],[])

let rec group : nat -> 'a list -> 'a list list =
  fun n -> function
    | [] -> []
    | xs ->
      let ys,zs = List.split_n xs n in
      ys :: group n zs

let middle xs = xs @!! ((length xs + 1) / 2 - 1)

let sort = Ch6_2.Qsort.sort

let medians = map (middle @. sort) @. (group 5)

let%test _ =
  Poly.equal (medians @@ nrange 1 12) [3;8;11]

let rec select : nat -> 'a list -> 'a = 
  fun k xs ->
  let us,vs,ws = partition3 (pivot xs) xs in
  let m,n = length us, length vs in
  if k <= m then select k us
  else if k <= m + n then vs @!! (k-m-1)
  else (* k > m+n *)  select (k-m-n) ws
and median xs = 
  select ((length xs + 1) / 2) xs
and pivot = function
  | [x] -> x
  | xs -> median (medians xs)

(* sec 6.4 *)

let rec search_from k = function
  | [] -> k
  | x::xs -> 
    if k = x then
      search_from (k+1) xs
    else
      k

let select xs = search_from 0 (sort xs)