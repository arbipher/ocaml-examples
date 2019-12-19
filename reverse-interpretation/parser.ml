#use "main.ml";;

open Toy
open Angstrom

let (let*) = (>>=)

let ws = skip_while (function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false)

let lchar c =
  ws *> char c

let schar c =
  ws *> char c <* ws

let lstring s =
  ws *> string s

let parens p = lchar '(' *> p <* lchar ')'
let add = char '+' *> return (+)
(* let sub = char '-' *> return (-)
let mul = char '*' *> return ( * )
let div = char '/' *> return (/) *)

let is_digit =
  function '0' .. '9' -> true | _ -> false

let is_idchar0 =
  function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let is_idchar =
  function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false

let integer =
  take_while1 is_digit >>| (fun x -> Int (int_of_string x))

let id = 
  ws *> 
  lift2 (^)
    (satisfy is_idchar0 >>| Char.escaped)
    (take_while is_idchar)

let var =
  id >>= (fun x -> return (Var x))

let int_plus = 
  let* es = (sep_by1 (schar '+') integer) in
  let e1, er = List.hd es, List.tl es in
  return (List.fold_left (fun acc e -> Plus (acc, e)) e1 er)

let expr : exp t = 
  fix (fun expr ->
    let fun_ = 
      lstring "fun" *>
      let* x = id in
      lstring "->" *>
      let* e = expr in
      return (Fun (x, e))
    and plus = 
      let* es = (sep_by1 (schar '+') expr) in
      let e1, er = List.hd es, List.tl es in
      return (List.fold_left (fun acc e -> Plus (acc, e)) e1 er)
    in
    ws *> 
    choice [parens expr; plus; integer; var;])

let program =
  expr <* end_of_input

let eval (s : string) =
  match parse_string program s with
  | Ok v      -> v
  | Error msg -> failwith msg
;;

eval "1";;
eval "x";;
eval "1 x";;