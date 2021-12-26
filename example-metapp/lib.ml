let magic = 42

let s1 =
  [%meta if Sys.ocaml_version >= "4.08.0" then
     [%e Sys.ocaml_version]
  else
     [%e "0"]]


let s2 =
 if Sys.ocaml_version >= "4.08.0" then
  [%meta [%e Sys.ocaml_version]]
else
    "0"
;;

let%expect_test _ =
  print_endline s1;
  [%expect{| 4.13.1 |}]

[%%metadef let m1 () = Metapp_preutils.Exp.of_int 1]
(* [%%metadef let m2 () = [%meta [%e 2]]] *)

let sm1 =  [%meta m1 ()]

let%expect_test _ =
  print_int sm1;
  [%expect{| 1 |}];
  print_int [%meta m1 ()];
  [%expect{| 1 |}]


type foo = [%meta if Sys.ocaml_version >= "4.08.0" then
    [%t: int] else
    [%t: string]
]


let s3 =
  [%meta if Sys.ocaml_version >= "4.08.0" then
     [%e Sys.ocaml_version]
    else
       [%e "0"]]

let%expect_test _ =
  [%meta if Sys.ocaml_version >= "4.08.0" then
    [%e (print_int 1)] else
    [%e (print_endline "1")]];
  [%expect{| 1 |}];

;;

[%%metaverbose true]
[%%metapackage fmt]
(* [%%metaflag "-open", "Fmt"] *)
[%%metapackage base]

(* [%%metapackage stdcompat]
[%%metaflag "-open", "Stdcompat"] *)

let s4 =
  [%meta if Sys.ocaml_version >= "4.08.0" then
      (* [%e (Fmt.str "%d" 1)] *)
      [%e string_of_int (Base.Int.abs 1)]
      (* [%e string_of_int (Int.add 0 1)] *)
    else
      [%e "0"]]

let%test _ = String.equal s4 "1"

let a = 
  match (0, 1, "hello", 3) with
  | [%meta
      Ppxlib.Ast_helper.Pat.tuple (List.init 4 (function
      | 2 -> [%p? x]
      | _ -> [%p? _]))] -> x
;;

let%test _ = String.equal a "hello"

(* modules *)
(* [%%metapackage metapp] *)

module T1 = struct
  type t1 = int

  [%%meta
   [%stri type t2 = string]
  ]
end

let s6 : T1.t2 = "foo"

(* module T2 = struct
  
  [%%meta
    [%str 
      type t1 = int
      type t2 = string
    ]
  ]
end

let s7 : T2.t2 = "foo" *)