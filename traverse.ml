type 'a tree = 
  | Leaf
  | Binary of 'a * 'a
  | Ternary of 'a * 'a * 'a

(* let fmap (f : 'a -> 'b -> 'c * 'b) : 'a tree -> ('c * 'b) tree = function  *)
let sequence f ctx = function 
  | Leaf -> ctx, Leaf
  | Binary (t1, t2) -> 
    let r1, ctx1 = f t1 ctx
    in let r2, ctx2 = f t2 ctx1
    in ctx2, Binary (r1, r2)
  | Ternary (t1, t2, t3) -> 
    let r1, ctx1 = f t1 ctx
    in let r2, ctx2 = f t2 ctx1
    in let r3, ctx3 = f t3 ctx2
    in ctx3, Ternary (r1, r2, r3)

(* let rec foldr alg : 'a tree -> 'a) e : 'a = *)
let rec traverse alg ctx e =
  alg (sequence (traverse alg ctx) e)

let string_alg e ctx = 
  match e with 
  | Leaf -> (Printf.sprintf "Leaf %d" ctx, ctx + 1)
  | Binary (r1, r2) -> 
    (Printf.sprintf "Binary %d (%s, %s)\n" ctx r1 r2 , ctx + 1)
  | Ternary (r1, r2, r3) -> 
    (Printf.sprintf "Ternary %d (%s, %s, %s)\n" ctx r1 r2 r3, ctx + 1)

let rl = string_alg Leaf 0;;
let rb = string_alg (Binary("Leaf", "Leaf")) 0;;

let rsl = sequence string_alg 0 Leaf;;
let rsb = sequence string_alg 0 (Binary(Leaf, Leaf));;
let nctx, nr = rsb in string_alg nr nctx;;

let rsbb = sequence string_alg 0 (Binary(Binary(Leaf, Leaf), Leaf));;
let nctx, nr = rsb in string_alg nr nctx;;

(* 
[O] sequence traverse
[O] traverse alg
[X] sequence alg

let _ = sequence string_alg 0;; 
*)


(* let t : 'a tree as 'a =   *)
let t =  
  Ternary(Leaf, Leaf, Binary (Leaf, Leaf))
let str, rs = traverse string_alg t
;;
Printf.printf "%s" str
;;
rs