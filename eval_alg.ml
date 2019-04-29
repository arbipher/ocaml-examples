type 'a tree = 
  | Var of string
  | Int of int
  | Function of string * 'a
  | Appl of 'a * 'a

(* let fmap (f : 'a  -> 'b) : 'a tree -> 'b tree = function  *)
let fmap f = function 
  | Var (id) -> Var (id)
  | Int (i) -> Int (i)
  | Function (id, e) -> Function (id, f e)
  | Appl (e1, e2) -> Appl (f e1, f e2)

(* let rec foldr alg : 'a tree -> 'a) e : 'a = *)
let rec foldr alg e =
  alg (fmap (foldr alg) e)

let rec make_space d = if d > 0 then " " ^ make_space (d - 1) else ""
let string_alg e env = 
  match e with 
  | Var(x) -> (Printf.sprintf "%s" x, env)
  | Int(i) -> (Printf.sprintf "%d" i, env)
  | Function (id, e) -> 
    let r, _ = e @@ id::env
    in (Printf.sprintf "(Fun %s -> %s)" id r, env)
  | Appl (e1, e2) -> 
    let r1, _ = e1 env
    in let r2, _ = e2 env
    in (Printf.sprintf "%s (%s)" r1 r2, env)

(* let t : 'a tree as 'a =   *)
let t = 
  Appl(
    Function("x", Var("x")), 
    Int(3))
let str, rs = foldr string_alg t []
;;
Printf.printf "%s" str
;;
rs
;;

let rec eval_alg e env = 
  match e with 
  | Var(x) -> List.assoc x env, env
  | Int(i) -> Int(i), env
  | Function (id, e) -> 
    Function (id, e), env
  | Appl (e1, e2) -> 
    let r1, _ = e1 env
    in let r2, _ = e2 env
    in match r1 with
    | Function (id, fe) -> (eval_alg fe ((id, r2)::env))
    | _ -> failwith "error"