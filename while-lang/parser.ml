open Base
open Angstrom

open Ast

let chainl1 elem op =
  let rec foldl acc =
    op <*> return acc <*> elem >>= foldl <|> return acc in
  elem >>= foldl

let ws_pred = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let ws = skip_while ws_pred

let lchar c = ws *> char c

let lstring s = ws *> string s

let parens p = lchar '(' *> p <* lchar ')'

let comment = 
  lstring "(*" *> 
  many_till any_char (string "*)") *> 
  return ()

let comments = 
  skip_many ((satisfy ws_pred *> return ()) <|> comment)

let ignore_comments p =
  comments *> p <* comments

let is_digit =
  function '0' .. '9' -> true | _ -> false

let is_alphabet =
  function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let id_ = 
  ws *>
  lift2 (fun p1 p2 -> p1 ^ p2)
    (take_while1 is_alphabet)
    (take_while is_digit)

let non_keyword_id =
  id_ >>= fun s ->
  if List.mem ["do"; "while"; "*)"] s ~equal:String.equal then
    fail "_"
  else
    return s

let const v = 
  fun _ -> v

let not_string str =
  string str >>| const false <|> return true >>= function
  | true -> return ()
  | false -> fail "not_string"

(* 
let a in let b in c

 *)
let not_followed_by p =
  p >>| const false <|> return true >>= function
  | true -> return ()
  | false -> fail "not_followed_by"
let var = non_keyword_id >>| (fun x -> Var(x))
let expr =
  fix (fun e ->
      let cons = 
        lstring "cons" *>
        lift2 (fun e1 e2 -> Cons(e1, e2)) e e 
      and head = 
        lstring "hd" *> e
        >>| (fun e -> Hd(e))
      and tail = 
        lstring "tl" *> e
        >>| (fun e -> Tl(e))
      and eq =
        lstring "=?" *>
        lift2 (fun e1 e2 -> Eq(e1, e2)) e e 
      and atom = 
        lstring "nil"  *> return Atom
      in
      ignore_comments @@ choice [parens e; cons; head; tail; eq; atom; var]
    )

let cmd =
  fix (fun c ->
      let assign =
        lift2 (fun v e -> Assign {v = v; e = e}) 
          non_keyword_id 
          ((lstring ":=") *>
           expr) 
      in
      let seq = 
        let op_seq =
          (lstring ";") *> return
            (fun acc c -> Seq (acc, c))
        in
        chainl1 assign op_seq
      and while_ =
        lift2 (fun e c -> While {cond = e; body = c})
          (lstring "while" *>
           expr)
          (lstring "do" *>
           c)
      in
      ignore_comments @@ choice [while_; seq]
    )

let program =
  ignore_comments @@
  lift3 (fun x c y -> Program {id_in = x; cmd = c; id_out = y})
    (lstring "read" *> non_keyword_id <* lchar ';')
    (cmd <* lchar ';')
    (lstring "write" *> non_keyword_id)

let eval_exp (s : string) : exp =
  match parse_string ~consume:All expr s with
  | Ok v      -> v
  | Error msg -> failwith msg

let eval_cmd (s : string) : cmd =
  match parse_string ~consume:All cmd s with
  | Ok v      -> v
  | Error msg -> failwith msg

let eval_program (s : string) : program =
  match parse_string ~consume:All program s with
  | Ok v      -> v
  | Error msg -> failwith msg

let eval = eval_program

(* let%test_unit "addition" = [%test_result: int] (1 + 2) ~expect:4 *)

open Base.Poly

let%test "_" = (eval_exp "cons a a") = Cons(Var "a", Var "a")
let%test "_" = (eval_exp "cons (a) a") = Cons(Var "a", Var "a")
let%test "_" = (eval_exp "cons a (a)") = Cons(Var "a", Var "a")
let%test "_" = (eval_exp "(cons a a)") = Cons(Var "a", Var "a")
let%test "_" = (eval_exp "(cons ((a)) (a))") = Cons(Var "a", Var "a")
let%test "_" = (eval_exp "hd a") = Hd (Var "a")
let%test "_" = (eval_exp "tl a") = Tl (Var "a")
let%test "_" = (eval_exp "=? a nil") = Eq(Var "a", Atom)
let%test "_" = (eval_exp "=? (a) (nil)") = Eq(Var "a", Atom)
let%test "_" = (eval_exp "nil") = Atom

let%test "_" = (eval_exp "(* 1 *) nil") = Atom
let%test "_" = (eval_exp "nil (* 2 *)") = Atom

let%test "_" = (eval_exp "x") = Var "x"
let%test "_" = (eval_cmd "x := x") = Assign {v = "x"; e = Var "x"}
let%test "_" = (eval_cmd "x := x ; y := y") = 
               Seq(Assign {v = "x"; e = Var "x"}, Assign {v = "y"; e = Var "y"})
let%test "_" = (eval_cmd "x := x ; y := y; z := z") = 
               Seq(Seq(Assign {v = "x"; e = Var "x"}, Assign {v = "y"; e = Var "y"}), Assign {v = "z"; e = Var "z"})
let%test "_" = (eval_cmd "x := hd x ; y := tl y; z := nil") = 
               Seq(Seq(Assign {v = "x"; e = Hd (Var "x")}, Assign {v = "y"; e = Tl (Var "y")}), Assign {v = "z"; e = Atom})
let%test "_" = (eval_cmd "while a do x := x") = While {cond = Var "a"; body = Assign{v = "x"; e = Var "x"}}
let%test "_" = (eval_cmd "while a do while a do x := x") = While {cond = Var "a"; body = While {cond = Var "a"; body = Assign{v = "x"; e = Var "x"}}}

let%test "_" = (eval_program "read x; x := x; write x") = 
               Program {id_in = "x"; id_out = "x"; cmd = Assign {v = "x"; e = Var "x"}}

let%test "_" = (eval_program "(* 1 *)read x; x := x; write x") = 
               Program {id_in = "x"; id_out = "x"; cmd = Assign {v = "x"; e = Var "x"}}
let%test "_" = (eval_program "(* 1 *) read x; (* 1 *)x := (* 1 *)x; write x(* 1 *)") = 
               Program {id_in = "x"; id_out = "x"; cmd = Assign {v = "x"; e = Var "x"}}


let%test_unit "_" = ignore (eval_program 
                              "(* Jones, N. D. (1997). Computability and Complexity: From a Programming Perspective.  *)
(* p.34  *)

read X;
  Y := cons nil X;
write Y
" : program )