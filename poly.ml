(* step 2: poylymorphic recursive type *)

type ('a, 'b) tree = 
  | Leaf
  | Binary of 'b * 'a * 'a
  | Ternary of 'b * 'a * 'a * 'a

let fmap f = function 
  | Leaf -> Leaf
  | Binary (c, t1, t2) -> Binary (c, f t1, f t2)
  | Ternary (c, t1, t2, t3) -> Ternary (c, f t1, f t2, f t3)

let rec foldr alg e =
  alg (fmap (foldr alg) e)

let string_alg = function
  | Leaf -> "L"
  | Binary (e, t1, t2) -> Printf.sprintf "Binary [%d] (%s, %s)" e t1 t2
  | Ternary (e, t1, t2, t3) -> Printf.sprintf "Ternary [%d] (%s, %s, %s)" e t1 t2 t3

let t =  
  Ternary(5, Leaf, Leaf, Binary (7, Leaf, Leaf))

let _ = foldr string_alg t