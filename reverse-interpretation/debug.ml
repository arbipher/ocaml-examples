#require "angstrom";;
open Angstrom

type tid = string
and exp = 
  | Var of tid

  | Neg of exp
  | Int of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Mul of exp * exp
  | Div of exp * exp

  | Bool of bool
  | And of exp * exp
  | Or of exp * exp
  | Not of exp

  | Fun of tid list * exp
  | App of exp * exp
  | Let of tid * exp * exp

let ws = skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)

let lchar c =
  ws *> char c

let lstring s = 
  ws *> string s

let parens p = lchar '(' *> p <* lchar ')'

let is_digit =
  function '0' .. '9' -> true | _ -> false

let is_alphabet =
  function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let integer =
  ws *>
  take_while1 is_digit >>| (fun x -> Int (int_of_string x))

let var = 
  ws *>
  lift2 (fun p1 p2 -> Var (p1 ^ p2))
    (take_while1 is_alphabet)
    (take_while is_digit)

let id = 
  ws *>
  lift2 (fun p1 p2 -> p1 ^ p2)
    (take_while1 is_alphabet)
    (take_while is_digit)

let chainl1 elem op =
  let rec foldl acc =
    (lift2 (fun f x -> f acc x) op elem >>= foldl) <|> return acc in
  elem >>= foldl

let chainl1 elem op =
  let rec foldl acc =
    (fun f x -> f acc x) <$> op <*> elem >>= foldl <|> return acc in
  elem >>= foldl

let chainl1 elem op =
  let rec foldl acc =
    op <*> return acc <*> elem >>= foldl <|> return acc in
  elem >>= foldl

(* let rec chainr1 e op =
   (op <*> (chainr1 e op)) <|> e *)

let chainr1 e op =
  fix (fun loop ->
      (op <*> loop) <|> e
    )

let chain1_nested e op =
  fix (fun loop ->
      op (loop <|> e)
    )

let rec fold_right_2 es op =
  match es with
  | [] -> failwith "non empy"
  | x :: [] -> x
  | x :: xs -> op x (fold_right_2 xs op)

let rchainl1 elem sep op =
  sep_by1 sep elem >>|
  fun es -> 
  fold_right_2 es op

let mul_r e = rchainl1 e (lchar '*') (fun x y -> Mul(x, y))
let add = lchar '+' *> return (fun e1 e2 -> Plus(e1, e2))
let sub = lchar '-' *> return (fun e1 e2 -> Minus(e1, e2))
let mul = lchar '*' *> return (fun e1 e2 -> Mul(e1, e2))
let div = lchar '/' *> return (fun e1 e2 -> Div(e1, e2))
let btrue = lstring "true" *> return (Bool true)
let bfalse = lstring "false" *> return (Bool false)
let blit = btrue <|> bfalse
let op_and = lstring "&&" *> return (fun e1 e2 -> And(e1, e2))
let op_or = lstring "||" *> return (fun e1 e2 -> Or(e1, e2))
let op_app = return (fun acc e -> App(acc, e))
let op_signed = 
  (lchar '+' *> return (fun e -> e)) <|>
  (lchar '-' *> return (fun e -> Neg e))

let op_not = lstring "not" *> return (fun e -> Not e)

let if_not cond e =
  (cond *> commit *> fail "_") <|> e

let non_keyword_var = 
  var >>= function
  | Var s ->
    if List.mem s ["in"; "and"; "not"; "or"; "let"] then
      fail "_"
    else
      return (Var s)
  | _ -> fail "_"

let expr : exp t = 
  fix (fun expr ->
      let pe = integer <|> non_keyword_var <|> parens expr in
      let app = chainl1 pe op_app in
      let fun_p = 
        lstring "fun" *> 
        lift2 (fun ids e -> Fun (ids, e))
          (many1 id) ((lstring "->") *> expr)
      in
      let lambda = fun_p <|> app in
      let factor = chainr1 lambda op_signed in
      let term = chainl1 factor (mul <|> div) in
      let arith = chainl1 term (add <|> sub) in
      let logic_term = blit <|> arith in
      let not_ = chainr1 logic_term op_not in
      let logic = chainl1 not_ (op_and <|> op_or) in
      let let_template e = 
        lift3 (fun id e1 e2 -> Let (id, e1, e2))
          (lstring "let" *> id <* lstring "=") e (lstring "in" *> e)
      in
      let let_term = chain1_nested logic let_template in
      let_term <|> logic
    ) <* ws 

let eval (s : string) : exp =
  match parse_string ~consume:All expr s with
  | Ok v      -> v
  | Error msg -> failwith msg
;;

eval "(1)";;
eval "- 1";;
eval "- - 1";;
eval "(1 * 2)";;

eval "-3 * -1";;
eval "-3 * (-1)";;
eval "-(1+1)";;
eval "- - - 1";;
eval "- + - 1";;

eval "1";;
eval "x";;
eval "x + 1";;
eval "1 + x";;
eval "x + x";;
eval "1 + 2 + x";;

eval "f x + g x";;
eval "f x + g x * p y";;

eval "true && x";;
eval "x && false";;
eval "x && x";;

eval "true";;
eval "true && false";;
eval "true && false || true";;
eval "(true && false || true)";;
eval "(true && false || (true))";;

eval "fun x y -> 1 + 1";;
eval "fun x y -> true";;
eval "fun x y -> fun x y -> 1";;
eval "fun x y -> (fun x y -> 1)";;

eval "fun x -> 1";;
eval "fun x -> a a";;
eval "fun x -> a (fun x -> a)";;
eval "fun x -> fun x -> a";;
eval "fun x -> fun x -> a a";;
eval "fun x -> a (fun x -> a)";;

eval "a";;
eval "a a";;
eval "a b c";;

eval "- 1";;
eval "-- 1";;
eval "- a";;
eval "- a a";;
eval "- - - a a";;

eval "- (fun x -> 1) 1";;
(* eval "- (fun x -> 1) -1";; *)

eval "let x = 1 in 1";;
eval "let x = 1 in true";;
eval "let x = true in 1";;
eval "let x = true in true";;
eval "let x = a a in a a";;

eval "let x = (let x = 1 in 1) in 1";;
eval "let x = 1 in (let x = 1 in 1)";;
eval "let x = let x = 1 in 1 in 1";;
eval "let x = 1 in let x = 1 in 1";;
eval "let x = let x = 1 in 1 in let x = 1 in 1";;
eval "let x = 1 in let y = 1 in x + y";;
eval "let x = 1 in let y = let x = 1 in let x = 1 in 1 in x + y";;