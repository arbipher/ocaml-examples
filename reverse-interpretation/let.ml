(* #require "angstrom";; *)
open Angstrom

let ws = skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)

let is_digit =
  function '0' .. '9' -> true | _ -> false

let is_alphabet =
  function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let integer =
  ws *>
  take_while1 is_digit >>| int_of_string

let id = 
  ws *>
  lift2 (fun p1 p2 -> p1 ^ p2)
    (take_while1 is_alphabet)
    (take_while is_digit)

let term = 
  ws *> 
  take_while1 is_alphabet

let lstring s = 
  ws *> string s

(* let a = 
      let b = 1 in b 
   in 
      let c = 2 in c *)

let chain1_nested e op =
  fix (fun loop ->
      op (loop <|> e)
    )

let llet =
  fix (fun llet ->
      let let_template e = 
        lift3 (fun id e1 e2 -> Printf.sprintf "(let %s = %s in %s)" id e1 e2)
          (lstring "let" *> id <* lstring "=") e (lstring "in" *> e) in

      chain1_nested term let_template
    )

let eval (s : string) =
  match parse_string ~consume:All llet s with
  | Ok v      -> v
  | Error msg -> failwith msg
;;

let _ = eval "let x = a in x";;
let _ = eval "let x = a in let x = a in x";;
let _ = eval "let x = let x = a in x in x";;
let _ = eval "let x = a in let x = a in let x = a in a";;

let const v = fun _ -> v

let not_followed_by p =
  p >>| const false <|> return true >>= function
  | true -> return ()
  | false -> fail "not_followed_by"

let not_followed_by p =
  p *> fail "not_followed_by" <|> return ()

let not p =
  p >>| const false <|> return true >>= function
  | true -> return ()
  | false -> fail "not_followed_by"

(* let not p =
   p *> fail "not p" <|> return () *)

let ss = 
  not (string "aa") *> term

let eval2 (s : string) =
  match parse_string ~consume:All ss s with
  | Ok v      -> v
  | Error msg -> failwith msg