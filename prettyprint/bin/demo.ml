(*
   | Lambda of string * lambda
   | Var of string
   | Apply of lambda * lambda
*)

open Lambda_ast

let _e0 = Lambda ("x", Var "x")
let e1 = Apply (Lambda ("x", Var "x"), Lambda ("x", Var "x"))
let _ey = Apply (Lambda ("y", Var "y"), Lambda ("x", Var "x"))
let f e = Apply (Lambda ("x", e), Lambda ("x", Var "x"))
let _e2 = f e1
let e3 = Apply (Lambda ("x", Var "x"), Var "x")

open Format

let exp n e =
  Format.set_margin n ;
  List.init (n - 1) (fun _ -> "=")
  |> List.iter (fun s -> Format.fprintf Format.std_formatter "%s" s) ;
  Format.fprintf Format.std_formatter "%d" n ;
  Lambda_default.print_lambda e ;
  Format.fprintf Format.std_formatter "--dft--\n" ;
  Lambda_hov.print_lambda e ;
  Format.fprintf Format.std_formatter "--hov--\n" ;
  Lambda_hv.print_lambda e ;
  Format.fprintf Format.std_formatter "--hv--\n\n"

let () =
  exp 6 e3 ;
  exp 7 e3 ;
  exp 8 e3 ;
  exp 10 e3 ;
  (* Lambda_default.print_lambda e2 ;
     Format.fprintf Format.std_formatter "---\n" ;
     Lambda_hov.print_lambda e2 ;
     Format.fprintf Format.std_formatter "---\n" ;
     Lambda_hv.print_lambda e2 ; *)
  ()
