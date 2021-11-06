open Base
open Interpret
open Abstract_ast_helper
open Lazi

let pa s = s |> parse |> of_expr

let test_closed s = s |> parse |> is_closed

let test_literal s = s |> parse |> log_literal

let test_logger s = s |> parse |> LogEval.eval |> LogEval.run

let test_analysis s = s |> parse |> of_expr |> Analysis.eval

let run_analysis step s = s |> parse |> of_expr |> Analysis.run_steps step

let analysis_should_equal ?(step=10) s1 slist = 
  let el1 = run_analysis step s1
  and el2 = List.map ~f:pa slist in
  let s1 = Set.of_list (module AExpr) el1
  and s2 = Set.of_list (module AExpr) el2 in
  Set.equal s1 s2

let print_analysis ?(step=10) s = 
  pe "";
  run_analysis step s
  |> List.sexp_of_t AExpr.sexp_of_t
  |> Sexp.to_string_hum
  |> pe

;;
(* assert (equal_expr (Int 1) (parse "1"));;

   assert (not @@ test_closed "x");;
   assert (test_closed "Fun x -> x");;
   assert (not @@ test_closed "Fun x -> y");;
   assert (test_closed "Let x = 1 In 1");;
   assert (test_closed "Let x = 1 In x");;
   assert (test_closed "Let Rec f x = f 1 In f");;
   assert (test_closed "Let Rec f x = f x In f");;
   assert (not @@ test_closed "Let Rec f x = f 1 In f x");;

   assert (String.equal (test_literal "1") "1; ");;
   assert (String.equal (test_literal "Let x = 1 In x") "1; ");;
   assert (String.equal (test_literal "Let x = 1 In 2") "1; 2; ");;

   assert (String.equal (test_logger "1") "1; ");;
   assert (String.equal (test_logger "Let x = 1 In x") "1; ");;
   assert (String.equal (test_logger "Let x = 1 In 2") "1; 2; ");; *)

let%test _ = analysis_should_equal "1" ["1"]
let%test _ = analysis_should_equal "0" ["0"]
let%test _ = analysis_should_equal "1+1" ["1"]
let%test _ = analysis_should_equal "0+1" ["1"]
let%test _ = analysis_should_equal "1+0" ["1"]
let%test _ = analysis_should_equal "0+0" ["0"]

let%test _ = analysis_should_equal "True And False" ["False"]
let%test _ = analysis_should_equal "True Or False" ["True"]
let%test _ = analysis_should_equal "True And (1=1)" ["True"; "False"]
let%test _ = analysis_should_equal "(1=1) Or True" ["True"]

let%test _ = analysis_should_equal "True = True" ["True"]
let%test _ = analysis_should_equal "True = False" ["False"]
let%test _ = analysis_should_equal "0 = 0" ["True"]
let%test _ = analysis_should_equal "1 = 1" ["True"; "False"]
let%test _ = analysis_should_equal "(0 = 0) = True" ["True"]
let%test _ = analysis_should_equal "(0 = 1) = True" ["False"]

let%test _ = analysis_should_equal "(1 = 1) = True" ["True"; "False"] 

let%test _ = analysis_should_equal "(Fun x -> 1) 1" ["1"]
let%test _ = analysis_should_equal "(Fun x -> (0 = 0)) 1" ["True"]
let%test _ = analysis_should_equal "(Fun x -> (1 = 1)) 1" ["True"; "False"]

let%test _ = analysis_should_equal "(Fun x -> x) 1" ["1"]
let%test _ = analysis_should_equal "(Fun x -> x) 0" ["0"]
let%test _ = analysis_should_equal "(Fun x -> x) (1 = 0)" ["False"]
let%test _ = analysis_should_equal "(Fun x -> x) (1 = 1)" ["True"; "False"]

let%test _ = analysis_should_equal "Let x = 1 In 1" ["1"]
let%test _ = analysis_should_equal "Let x = 1 In x" ["1"]
let%test _ = analysis_should_equal "Let x = (0=0) In x" ["True"]
let%test _ = analysis_should_equal "Let x = (1=0) In x" ["False"]
let%test _ = analysis_should_equal "Let x = (1=1) In x" ["True"; "False"]

let%test _ = analysis_should_equal "Let x = 1 In Let x = 0 In x" ["0"]
let%test _ = analysis_should_equal "Let x = 1 In Let x = (0=1) In x" ["False"]
let%test _ = analysis_should_equal "Let x = 1 In Let x = (1=1) In x" ["True"; "False"]
let%test _ = analysis_should_equal "Let x = (1=1) In Let x = 1 In x" ["1"]

let%test _ = analysis_should_equal "Let Rec f x = 1 In 1" ["1"]
let%test _ = analysis_should_equal "Let Rec f x = 1 In f 0" ["1"]
let%test _ = analysis_should_equal "Let Rec f x = (0=0) In f 0" ["True"]
let%test _ = analysis_should_equal "Let Rec f x = (1=1) In f 0" ["True"; "False"]

let%test _ = analysis_should_equal "Let Rec f x = f x In 1" ["1"]
let%test _ = analysis_should_equal "Let Rec f x = f x In f 1" []

let%test _ = analysis_should_equal "{x = 1}.x" ["1"]
let%test _ = analysis_should_equal "{x = (0=0)}.x" ["True"]
let%test _ = analysis_should_equal "{x = (1=1)}.x" ["True"; "False"]
let%test _ = analysis_should_equal "(Fun x -> x.foo) {foo = 1}" ["1"]
let%test _ = analysis_should_equal "(Fun x -> x.foo) {foo = (1=1)}" ["True"; "False"]

let length_src = "
  Let Rec len r = If r.end Then 0 Else 1 + (len r.next) In
  len ";;

let%test _ = analysis_should_equal (length_src ^ "{end=True}") ["0"];;
(* let%test _ = analysis_should_equal (length_src ^ "{end=False; next={end=True}}") ["1"; "0"];; *)

let () = print_analysis (length_src ^ "{end = True}")

let () = print_analysis (length_src ^ "{end = False; next = {end = True}}")

let () = print_analysis "Let Rec f x = If x Then True Else f True In f False"

(* let () = print_analysis "Let x = 1 In Let x = 0 In x"
   let () = print_analysis "(Fun x -> (Fun x -> x) 0) 1" *)

let%expect_test "letrec" =
  pf "%d" (1 + 2);
  [%expect {| 3 |}]

(* let%expect_test _ = 

   [%expect {| |}]
*)

(* let () = print_analysis ~step:1 "Let Rec f x = 1 + 1 In f 1" *)
(* let () = print_analysis ~step:20 "(Fun x -> 1) 1";; *)
