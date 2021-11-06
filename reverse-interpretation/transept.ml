type operation =
  | Add
  | Minus
  | Mult
  | Div

type expr =
  | Number of float
  | BinOp of operation * expr * expr

module Utils = Transept.Utils
module CharParser = Transept.Extension.Parser.For_char_list
module Literals = Transept.Extension.Literals.Make (CharParser)

let operator = 
    let open Utils in
    let open CharParser in
    (atom '+' <$> constant Add)   <|>
    (atom '-' <$> constant Minus) <|>
    (atom '*' <$> constant Mult)  <|>
    (atom '/' <$> constant Div)

let expr = 
    (* sexpr ::= float | '(' expr ')' *)
    let rec sexpr () =
      let open Literals in
      let open CharParser in
      float <$> (fun f -> Number f) <|> (atom '(' &> do_lazy expr <& atom ')')
    
    (* expr ::= sexpr (operator expr)? *)
    and expr () =
      let open CharParser in
      do_lazy sexpr <&> opt (operator <&> do_lazy expr) <$> function
      | e1, None -> e1
      | e1, Some (op, e2) -> BinOp (op, e1, e2)
    
    in expr

let parse s =
    let open Utils in
    let open CharParser in
    let result = parse (expr ()) @@ Stream.build @@ chars_of_string s in
    Response.fold result
      (fun (_, a, c) -> Some a, c)
      (fun (_, c) -> None, c)
    (* match  with
    | Ok v -> v
    | Error _ -> failwith "error" *)
