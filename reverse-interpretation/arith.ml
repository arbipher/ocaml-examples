open Angstrom

type exp = 
  | Int of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Mul of exp * exp
  | Div of exp * exp

let ws = skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)

let lchar c =
  ws *> char c
let parens p = lchar '(' *> p <* lchar ')'
let add = lchar '+' *> return (fun e1 e2 -> Plus(e1, e2))
let sub = lchar '-' *> return (fun e1 e2 -> Minus(e1, e2))
let mul = lchar '*' *> return (fun e1 e2 -> Mul(e1, e2))
let div = lchar '/' *> return (fun e1 e2 -> Div(e1, e2))
let integer =
  ws *> 
  take_while1 (function '0' .. '9' -> true | _ -> false) 
  >>| (fun x -> Int (int_of_string x))

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init

let chainl1 elem op =
  let rec foldl acc =
    (lift2 (fun f x -> f acc x) op elem >>= foldl) <|> return acc in
  elem >>= foldl

(* (lift2 (fun f x -> f acc x) op elem >>= foldl) *)
 (* <|> return acc  *)
let chainll (elem : 'a t) (op : 'a -> 'a -> 'a) : 'a t =
  let rec foldl (acc : 'a) : 'a t =
    (elem >>= fun e -> return (op acc e) >>= foldl) <|> return acc
     in
  foldl

let expr : exp t =
  fix (fun expr ->
    let factor = parens expr <|> integer in
    let term   = chainl1 factor (mul <|> div) in
    chainl1 term (add <|> sub))

let eval (str : string) : exp =
  match parse_string ~consume:All expr str with
  | Ok v      -> v
  | Error msg -> failwith msg