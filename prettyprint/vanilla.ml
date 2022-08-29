(*
utop # Format.pp_print_int;;
- : Format.formatter -> int -> unit = <fun>

utop # Format.print_int;;
- : int -> unit = <fun>

utop # Format.(printf "%d\n" 42);;
42
- : unit = ()
*)

let%expect_test _ =
  Format.(printf "%d\n" 42) ;
  [%expect {| 42 |}] ;
  Format.(printf "%a\n" pp_print_int 42) ;
  [%expect {| 42 |}]

let pp_int_pair formatter (a, b) = Format.fprintf formatter "%d, %d" a b

let pp_pair pp_a pp_b formatter (a, b) =
  Format.fprintf formatter "(%a, %a)" pp_a a pp_b b

let%expect_test _ =
  let p = (4, 2) in
  Format.(printf "%a\n" pp_int_pair p) ;
  [%expect {| 4, 2 |}] ;
  Format.(printf "%a\n" (pp_pair pp_print_int pp_print_int) p) ;
  [%expect {| (4, 2) |}] ;
  ()

let pp_int_pair_neo ppf (a, b) =
  Format.(
    pp_print_int ppf a ;
    pp_print_string ppf ", " ;
    pp_print_int ppf b)

let pp_pair_neo pp_a pp_b ppf (a, b) =
  Format.(
    pp_a ppf a ;
    pp_print_string ppf ", " ;
    pp_b ppf b)

let%expect_test _ =
  let p = (4, 2) in
  Format.(printf "%a\n" pp_int_pair_neo p) ;
  [%expect {| 4, 2 |}] ;
  Format.(printf "%a\n" (pp_pair_neo pp_print_int pp_print_int) p) ;
  [%expect {| 4, 2 |}] ;
  ()

(* https://github.com/ocaml/ocaml/blob/2526e225838b868a3b0109fd9a8da36a8f1e0c29/stdlib/format.ml#L1252
   let rec pp_print_list ?(pp_sep = pp_print_cut) pp_v ppf = function
     | [] -> ()
     | [v] -> pp_v ppf v
     | v :: vs ->
       pp_v ppf v;
       pp_sep ppf ();
       pp_print_list ~pp_sep pp_v ppf vs

   let pp_print_option ?(none = fun _ () -> ()) pp_v ppf = function
     | None -> none ppf ()
     | Some v -> pp_v ppf v *)

(*
   Format.printf "%a" pp_a a === pp_a Format.std_formatter a
*)
let%expect_test _ =
  Format.(printf "%d\n" 42) ;
  [%expect {| 42 |}] ;
  Format.(printf "%a\n" pp_print_int 42) ;
  [%expect {| 42 |}] ;
  Format.(fprintf std_formatter "%d\n" 42) ;
  [%expect {| 42 |}] ;
  Format.(fprintf std_formatter "%a\n" pp_print_int 42) ;
  [%expect {| 42 |}] ;
  Format.(pp_print_int std_formatter 42) ;
  [%expect {| 42 |}] ;
  ()

(*  *)
