open Base
open Ast

let parse s =
  let lexbuf = Lexing.from_string (s^";;") in
  Parser.main Lexer.token lexbuf

class and_monoid = object
  method zero : bool = false
  method plus b1 b2 : bool = 
    b1 && b2
end

let is_closed (e : expr) = 
  let v = object (self)
    inherit [_] reduce as super
    inherit and_monoid
    method! visit_Int _env _ = true
    method! visit_Bool _env _ = true
    method! visit_Var env x =
      List.mem env x ~equal:equal_ident
    method! visit_Function env x e =
      super#visit_expr (x::env) e
    method! visit_Let env x e1 e2 =
      self#plus (super#visit_expr env e1) (super#visit_expr (x::env) e2)
    method! visit_LetRec env x1 x2 e1 e2 =
      self#plus (super#visit_expr (x1::x2::env) e1) (super#visit_expr (x1::env) e2)
  end in
  v#visit_expr [] e

class log_monoid = object
  method zero : string = ""
  method plus b1 b2 : string = b1 ^ b2
end

let log_literal e = 
  let v = object (_self)
    inherit [_] reduce as _super
    inherit log_monoid
    method! visit_Int _env i = (Int.to_string i) ^ "; "
    method! visit_Bool _env b = (Bool.to_string b)  ^ "; "
  end in
  v#visit_expr () e

(* Error: invalid package type: parametrized types are not supported *)
(* let eval (type a) e (module M : Monad.S with type 'a t = a) : a =
   match e with
   | Int i -> M.return (Int i)
   | _ -> failwith "." *)

module Logger = struct
  type 'a t = 'a * string
  let bind (a, log_a) ~f:f =
    let (b, log_b) = f a in
    (b, log_a ^ log_b)

  let return a = a, ""

  let map = `Define_using_bind
end

module LogEval = struct 
  module M = Monad.Make(Logger)
  open M
  (* open Let_syntax *)

  let rec eval e : expr Logger.t = 
    match e with
    | Int i -> (Int i, (Int.to_string i) ^ "; ")
    | Bool b -> (Bool b, (Bool.to_string b) ^ "; ")
    | Let (_id, e1, e2) -> (eval e1) >>= (fun _ -> (eval e2))
    | r -> M.return r

  let run (m : 'a Logger.t) =
    let (_, s) = m in s 
end