open Base
open Ast
open Ast_helper

type aint = Pos | Zero | Neg

and aexpr =
  (* int *)
  | AInt of aint | APlus of aexpr * aexpr 
  | AMinus of aexpr * aexpr | AEqual of aexpr * aexpr

  (* bool *)
  | ABool of bool | AAnd of aexpr * aexpr| AOr of aexpr * aexpr | ANot of aexpr
  | AIf of aexpr * aexpr * aexpr

  (* lambda *)
  | AVar of ident | ALet of callsite * ident * aexpr * aexpr
  | AFunction of ident * aexpr | AClosure of aexpr * Context.t
  | AAppl of callsite * aexpr * aexpr  
  | ALetRec of callsite * ident * ident * aexpr * aexpr

  (* record *)
  | ARecord of (label * aexpr) list | ASelect of label * aexpr

(* state *)
(* not yet *)
[@@deriving sexp, compare, equal]