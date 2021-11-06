open Base

type label = Lab of string

and ident = Ident of string

and expr = 
  (* int *)
  | Int of int | Plus of expr * expr | 
    Minus of expr * expr | Equal of expr * expr

  (* bool *)
  | Bool of bool | And of expr * expr | Or of expr * expr | Not of expr | If of expr * expr * expr

  (* lambda *)
  | Var of ident | Let of ident * expr * expr | Function of ident * expr | Appl of expr * expr |
  LetRec of ident * ident * expr * expr

  (* record *)
  | Record of (label * expr) list | Select of label * expr

  (* state *)
  | Ref of expr | Set of expr * expr | Get of expr | Cell of int

[@@deriving 
  visitors { variety = "iter" },
  visitors { variety = "reduce" },
  sexp, compare, equal]

(* and closure = {
   (* fn is a unwrapper Function *)
   fn : ident * expr ;
   (* env is an env *)
   env: (ident * expr) list
   } *)
