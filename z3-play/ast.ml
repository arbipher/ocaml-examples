open Core

module Var = struct
  module T = 
  struct type t = Var of string [@@deriving sexp, compare, variants] end
  include T
  include Comparable.Make(T)

  let to_string e = e |> sexp_of_t |> Sexp.to_string
end

module Ast = struct
  module T = struct
    type t = expr

    and expr = Expr of clause list

    and clause = { 
      x : Var.t;
      body : body
    }

    and body = 
      | Value of value
      | Var of Var.t
      | Int_input
      | Appl of Var.t * Var.t
      | Cond of Var.t * expr * expr
      | Plus of Var.t * Var.t
      | Not of Var.t

    and value =
      | Int of int
      | Bool of bool
      | Fun of fun_record

    and fun_record = {
      para : Var.t;
      fbody : expr;
    }
    [@@deriving sexp, compare, fields]
  end
  include T
  include Comparable.Make(T)

  let clause s body =
    { 
      x = (Var.Var s);
      body = body;
    }

  let vi i = Value (Int i)
  let vb b = Value (Bool b)
  let vf f = Value (Fun f)
  let leti id i = clause id (vi i)
  let letb id b = clause id (vb b)
  let letf f x fcs = clause f (vf {
      para = Var.Var x;
      fbody = Expr fcs;
    })
  let letin id = clause id Int_input
  let letp x y z = clause x (Plus (Var.Var y, Var.Var z))
  let app x y = Appl ((Var.Var x), (Var.Var y))
  let letv id e = clause id e

end

let e = Ast.Expr [{x = Var "x"; body = Ast.Int_input}]

let () = 
  let m1 = Map.empty (module Var) in
  let m2 = Map.singleton (module Var) (Var "x") 1 in

  let m3 = Map.empty (module Ast) in
  let m4 = Map.singleton (module Ast) e 1 in
  ()

let%expect_test _ =
  e 
  |> Ast.sexp_of_t 
  |> Sexp.to_string 
  |> print_endline;
  [%expect{| (Expr(((x(Var x))(body Int_input)))) |}]