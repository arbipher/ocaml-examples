open Ast

val eval_cmd : string -> cmd

val eval_exp : string -> exp

val eval_program : string -> program

val eval : string -> program