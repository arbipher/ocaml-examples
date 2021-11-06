open Core

module Exp = struct
  module T = struct
    type t = 
      | Int of int 
      | Bool of bool
      | Var of Ast.Var.t
      | Any
      | Fun
    [@@deriving sexp, compare, variants]
  end
  include T
  include Comparable.Make(T)
end

module TypeEqZ3 = struct
  open Z3

  let ctx = mk_context []
  let solver  = Solver.mk_solver ctx None
  let intS = Arithmetic.Integer.mk_sort ctx
  let boolS = Boolean.mk_sort ctx
  let intC = Datatype.mk_constructor_s ctx "Int"
      (Symbol.mk_string ctx "is-Int") [Symbol.mk_string ctx "i"] [Some intS] [1]
  let boolC = Datatype.mk_constructor_s ctx "Bool"
      (Symbol.mk_string ctx "is-Bool") [Symbol.mk_string ctx "b"] [Some boolS] [2]
  let funC = Datatype.mk_constructor_s ctx "Fun"
      (Symbol.mk_string ctx "is-Fun") [] [] []
  let valS = Datatype.mk_sort_s ctx "IntOrBoolOrFun"
      [intC; boolC; funC]
  let intD = Datatype.Constructor.get_constructor_decl intC
  let boolD = Datatype.Constructor.get_constructor_decl boolC
  let funD = Datatype.Constructor.get_constructor_decl funC

  let e1 = Expr.mk_const_s ctx "r1" valS
  let e2 = Expr.mk_const_s ctx "r2" valS
  let intv1 = FuncDecl.apply intD [Arithmetic.Integer.mk_numeral_i ctx 11]
  let intb1 = FuncDecl.apply boolD [Boolean.mk_val ctx true]
  let f1 = Boolean.mk_eq ctx e1 intv1
  let f2 = Boolean.mk_eq ctx e2 intb1


  (* let intC = Datatype.mk_constructor_s ctx "IntV"
      (Symbol.mk_string ctx "is-IntV") [Symbol.mk_string ctx "i"] [Some intSort] [1]

     let nodeConstructor = Datatype.mk_constructor_s ctx "node"
      (Symbol.mk_string ctx "is-node") 
      [Symbol.mk_string ctx "left"; Symbol.mk_string ctx "node"; Symbol.mk_string ctx "right"]
      [None; Some intSort; None] [0; 0; 0]

     let intD = Datatype.Constructor.get_constructor_decl intC

     let boolS = Boolean.mk_sort ctx
     (* let fun_s ctx = Sort.mk_uninterpreted_s ctx "fun-sort" *)

     let boolC = Datatype.mk_constructor_s ctx "Bool"
      (Symbol.mk_string ctx "is-Bool") [] [] []

     let boolD = Datatype.Constructor.get_constructor_decl boolC
     (* let funD ctx = Datatype.Constructor.get_constructor_decl (funC ctx) *)

     let value_s ctx = Datatype.mk_sort_s ctx "IntOrBoolOrFun"
      [intC; boolC]

     let var ctx name = 
     Z3.Expr.mk_const_s ctx name (value_s ctx)
     let int_ ctx i = 
     FuncDecl.apply intD [Expr.mk_numeral_int ctx i intSort]
     (* Arithmetic.Integer.mk_numeral_i ctx i *)
     let bool_ ctx b =
     FuncDecl.apply boolD [Boolean.mk_val ctx b]
     (* let fun_ ctx = 
     FuncDecl.apply (funD ctx) []     *)

     let of_expr ctx = function
     | Exp.Int i -> int_ ctx i
     | Exp.Bool b -> bool_ ctx b
     | Exp.Var x -> var ctx (Ast.Var.to_string x)
     | Exp.Fun -> failwith "sss"
     | Exp.Any -> var ctx ("???")

     let of_assign e1 e2 =
     let ze1 = of_expr ctx e1 in
     let ze2 = of_expr ctx e2 in
     Boolean.mk_eq ctx ze1 ze2 *)

  (* let solve_all ctx es =
     let solver = Solver.mk_solver ctx None in
     Solver.add solver es;
     print_endline @@ Solver.to_string solver;
     match Solver.check solver [] with
     | Solver.SATISFIABLE ->
      begin
        match Solver.get_model solver with
        | None -> print_endline "none"
        | Some model -> print_endline @@ Model.to_string model
      end
     | Solver.UNSATISFIABLE ->
      print_endline "UNSAT"
     | Solver.UNKNOWN ->
      failwith @@ Printf.sprintf "Unknown result in solve: %s"
        (Solver.get_reason_unknown solver) *)

  let () =
    Solver.add solver [f1; f2];
    print_endline @@ Solver.to_string solver;
    match Solver.check solver [] with
    | Solver.SATISFIABLE ->
      begin
        match Solver.get_model solver with
        | None -> print_endline "none"
        | Some model -> print_endline @@ Model.to_string model
      end
    | Solver.UNSATISFIABLE ->
      print_endline "UNSAT"
    | Solver.UNKNOWN ->
      failwith @@ Printf.sprintf "Unknown result in solve: %s"
        (Solver.get_reason_unknown solver)
end

open TypeEqZ3

(* let () =
   let _ze = of_assign (Exp.Var (Ast.Var.Var "x")) (Bool true) in
   () *)
(* solve_all ctx [ze] *)
(* 

  let z3e_of_te ctx te =
    let name =
      te 
      |> Typing.Type.sexp_of_t
      |> Sexp.to_string
    in
    Arithmetic.Integer.mk_const_s ctx name

  let z3eq_of_eq ctx ((lhs, rhs) : Typing.TypeEq.t) =
    let z3el = z3e_of_te ctx lhs in
    let z3er = z3e_of_te ctx rhs in
    let z3r = Boolean.mk_eq ctx z3el z3er in
    z3r
 *)

let%expect_test _ =
  print_endline "";
  [%expect{| |}]