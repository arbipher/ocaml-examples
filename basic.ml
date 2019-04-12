(* step 1: basic recursive type *)

type 'a tree = 
  | Leaf
  | Binary of 'a * 'a
  | Ternary of 'a * 'a * 'a

(* let fmap (f : 'a  -> 'b) : 'a tree -> 'b tree = function  *)
let fmap f = function 
  | Leaf -> Leaf
  | Binary (t1, t2) -> Binary (f t1, f t2)
  | Ternary (t1, t2, t3) -> Ternary (f t1, f t2, f t3)

(* let rec foldr alg : 'a tree -> 'a) e : 'a = *)
let rec foldr alg e =
  alg (fmap (foldr alg) e)

let string_alg = function
  | Leaf -> "L"
  | Binary (t1, t2) -> Printf.sprintf "Binary (%s, %s)" t1 t2
  | Ternary (t1, t2, t3) -> Printf.sprintf "Ternary (%s, %s, %s)" t1 t2 t3

(* let t : 'a tree as 'a =   *)
let t =  
  Ternary(Leaf, Leaf, Binary (Leaf, Leaf))

let _ = foldr string_alg t