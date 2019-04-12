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
  let d, state = ctx 
  in let padding, d' = make_space d, d + 2 
  in let id::state' = state 
  in match e with 
  | Leaf -> (Printf.sprintf "%sL %d\n" padding id, state')
  | Binary (t1, t2) -> 
    let r1, state1 = t1 (d', state')
    in let r2, state2 = t2 (d', state1)
    in (Printf.sprintf "%sBinary (\n%s, \n%s)" padding r1 r2, state2)
  | Ternary (t1, t2, t3) -> 
    let r1, state1 = t1 (d', state')
    in let r2, state2 = t2 (d', state1)
    in let r3, state3 = t3 (d', state2)
    in (Printf.sprintf "%sTernary (\n%s, \n%s, \n%s)" padding r1 r2 r3, state3)

(* let t : 'a tree as 'a =   *)
let t =  
  Ternary(Leaf, Leaf, Binary (Leaf, Leaf))
let str, rs = foldr string_alg t (0, [2; 3; 5; 7; 11; 13; 17; 19])
;;
Printf.printf "%s" str
;;
rs