open Base
open Lib

let (@<) = Char.(<)
let (@<=) = Char.(<=)
let (@=) = Char.(=)
let (@>) = Char.(>)
let (@>=) = Char.(>=)

let up : 'a list -> bool =
  fun xs ->
  and_ (zip_with (@<) (init xs) (tail xs))

let subseqs : 'a list -> 'a list list =
  foldr (fun x xss ->
      xss ++ map ((@:) x) xss
    ) [[]]

let subseqs = 
  let extend x xs = [xs; x::xs] in
  foldr (concat_map @. extend) [[]]

(* longest increasing subsequence *)
let lus : 'a list -> 'a list = 
  max_with length
  @. filter up
  @. subseqs

let ok x ys = null ys || x @< head ys

let lus =
  let step x xss = xss ++ map ((@:) x) (filter (ok x) xss) in
  max_with length
  @. foldr step [[]]

let (&<=) xs ys =
  match xs,ys with
  | [],[] -> true
  | x::xs,y::ys ->
    x @>= y && length xs >= length ys
  | [], _ 
  | _, [] -> false

let lus =
  let tstep x xss =
    let yss = map ((@:) x) (filter (ok x) xss) in
    let cmp xs ys = length xs <= length ys in
    thin_by (&<=) (merge_by cmp [xss;yss])
  in
  last
  @. foldr tstep [[]]
  @. String.to_list

let lus =
  let rec search x xs = function
    | [] -> [x::xs]
    | ys::yss ->
      if head ys @> x then
        ys :: search x ys yss
      else
        (x::xs)::yss
  in
  let tstep x = function
    | []::xss -> [] :: search x [] xss
    | _ -> failwith "tstep"
  in
  last
  @. foldr tstep [[]]
  @. String.to_list

(* let lcs : 'a list -> 'a list -> 'a list = 
   fun xs ys ->
   max_with length (intersect (subseqs xs) (subseqs ys)) *)

let rec sub xs ys =
  match xs,ys with
  | _, [] -> true
  | [], _ -> false
  | x::xs, y::ys ->
    if x @= y then
      sub xs ys
    else
      sub xs (y::ys)

let lcs xs (* ys *) = 
  max_with length
  @.filter (sub xs)
  @. subseqs
(* ys *)

let longer x y = 
  if length x >= length y then x else y

let rec lcs xs ys = 
  match xs,ys with
  | [],_ -> []
  | _,[] -> []
  | x::xs,y::ys ->
    if x @= y then
      x:: lcs xs ys
    else
      longer (lcs (x::xs) ys) (lcs xs (y::ys))

let short_than : nat -> 'a list -> bool =
  fun b xs -> length xs <= b
let short = short_than

let segments : 'a list -> 'a list list =
  concat_map inits @. tails

let mss : nat -> int list -> int list =
  fun b ->
  max_with sum
  @. filter (short b)
  @. segments

let rec mss b = 
  max_with sum
  @. map (msp b)
  @. tails
and msp b =
  max_with sum
  @. filter (short b)
  @. inits

let%test _ =
  Poly.equal (msp 4 [-2;4;4;-5;8;-2;3;1]) [-2;4;4]

let%test _ =
  Poly.equal (msp 6 [-2;4;4;-5;8;-2;3;1]) [-2;4;4;-5;8]

(* Scan Lemma
   map (foldr op e) @. tails = scanr op e
*)

