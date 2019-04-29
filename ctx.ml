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

let rec make_space d = if d > 0 then " " ^ make_space (d - 1) else ""
let string_alg e ctx = 
  let padding, ctx' = make_space ctx, ctx + 2 in match e with 
  | Leaf -> Printf.sprintf "%sL\n" padding
  | Binary (t1, t2) -> Printf.sprintf "%sBinary (\n%s, \n%s)" padding (t1 ctx') (t2 ctx')
  | Ternary (t1, t2, t3) -> Printf.sprintf "%sTernary (\n%s, \n%s, \n%s)" padding (t1 ctx') (t2 ctx') (t3 ctx')

(* let t : 'a tree as 'a =   *)
let t =  
  Ternary(Leaf, Leaf, Binary (Leaf, Leaf))
;;
Printf.printf "%s" @@ foldr string_alg t 0