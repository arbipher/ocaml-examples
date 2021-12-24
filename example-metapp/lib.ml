let magic = 42

let s1 =
  [%meta if Sys.ocaml_version >= "4.08.0" then
     [%e Sys.ocaml_version]
  else
     [%e "0"]]


(* let s2 =
[%meta if Sys.ocaml_version >= "4.08.0" then
    [%e Sys.ocaml_version]
else
    "0"] *)
(* 
Error: This expression has type string but an expression was expected of type
         Ppxlib.expression = Astlib.Ast_412.Parsetree.expression
*)

let%expect_test _ =
  print_endline s1;
  [%expect{| 4.13.1 |}]

(* let%expect_test _ =
  print_endline s2;
  [%expect{| |}] *)