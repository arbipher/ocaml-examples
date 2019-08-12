type t = A | B of int | C of bool * int | D of { a : int; b : string }
[@@deriving show]

let%expect_test _ =
  print_string @@ show A;
  [%expect{| Lib.A |}]

type foo = Foo [@@deriving show];;

(* Error: Unbound value Magic.pp *)
(* type myt = Sparkling [@@deriving show]

   let%expect_test _ =
   print_string @@ show_myt Sparkling;
   [%expect{| |}] *)

type myt2 = [%import: Magic.t] [@@deriving show]

let%expect_test _ =
  print_string @@ show_myt2 Sparkling;
  [%expect{| Magic.Sparkling |}]

module InnerMagic = struct
  type t = Normal
end
type myt3 = Normal [@@deriving show]
let%expect_test _ =
  print_string @@ show_myt3 Normal;
  [%expect{| Lib.Normal |}]

(* 
(* Error: [%import]: cannot locate module InnerMagic *)
type myt4 = InnerMagic.t [@@deriving show]
type myt5 = [%import: InnerMagic.t] [@@deriving show] 
*)