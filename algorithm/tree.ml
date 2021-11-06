open Base
open Lib

type 'a tree = 
  | Null
  | Node of nat * 'a tree * 'a * 'a tree

let _syntax_highlight_fix _ = ();;

let height = function
  | Null -> 0
  | Node(h, _, _, _) -> h

let node : 'a tree -> 'a -> 'a tree -> 'a tree =
  fun l x r ->
  let h = 1 + max (height l) (height r) in
  Node(h,l,x,r)

let rotr = function
  | Node(_,Node(_,ll,y,rl),x,r) -> node ll y (node rl x r)
  | _ -> failwith "rotr"

let rotl = function
  | Node(_,l,y,Node(_,rl,z,rr)) -> node (node l y rl) z rr
  | _ -> failwith "rotl"

let bias = function
  | Null -> failwith "bias"
  | Node(_,l,_,r) -> height l - height r

let rotateR t1 x t2 = 
  if bias t1 >= 0 then
    rotr (node t1 x t2)
  else
    rotr (node (rotl t1) x t2)

let rotateL t1 x t2 = 
  if bias t2 <= 0 then
    rotl (node t1 x t2)
  else
    rotl (node t1 x (rotr t2))

let balance t1 x t2 =
  let h1 = height t1 in
  let h2 = height t2 in
  if (abs (h1 - h2)) <= 1 then node t1 x t2
  else if h1 = h2 + 2 then rotateR t1 x t2
  else (* h2 = h1 + 2 *)   rotateL t1 x t2

let rec insert x = function
  | Null -> node Null x Null
  | Node(_,l,y,r) when x < y  -> balance (insert x l) y r
  | Node(h,l,y,r) when x = y  -> Node(h,l,y,r)
  | Node(_,l,y,r) (* x > y *) -> balance l y (insert x r)
