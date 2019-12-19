#use "main.ml";;

open Toy
open Angstrom

let (let*) = (>>=)
let (let+) = (>>|)

let ws = skip_while (function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false)

let lchar c =
  ws *> char c

let schar c =
  ws *> char c <* ws

let is_digit =
  function '0' .. '9' -> true | _ -> false

let integer =
  ws *>
  take_while1 is_digit >>| (fun x -> Int (int_of_string x))

let int_plus = 
  let* es = (sep_by1 (lchar '+') integer) in
  let e1, er = List.hd es, List.tl es in
  return (List.fold_left (fun acc e -> Plus (acc, e)) e1 er)

let expr : exp t = 
  fix (fun expr ->
    let plus = 
      let* es = (sep_by1 (lchar '+') expr) in
      let e1, er = List.hd es, List.tl es in
      return (List.fold_left (fun acc e -> Plus (acc, e)) e1 er)
    in
    ws *>
    choice [integer; plus; ])

let eval (s : string) =
  match parse_string expr s with
  | Ok v      -> v
  | Error msg -> failwith msg
;;

eval "1";;
(* eval "x";; *)
(* eval "1 + 1";; *)