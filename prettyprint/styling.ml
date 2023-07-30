(* observe
   for `let x = e1 in e2`,
   - `let x = ` is always at the first line
   - `e2` is always at the last line
   - `e1 in` will either be first in the first line or
   - `e1` will be in the indented new line and `in` will be in the non-indented newline. so the structure will be
*)

let%expect_test _ =
  Fmt.int Fmt.stdout @@ Format.pp_get_margin Format.std_formatter () ;
  [%expect {| 78 |}]

module Dummy = struct
  let _x = 1 in
  let _y =
    let y = 2 in
    y
  in
  let z =
    let z =
      let z = 3 in
      z
    in
    z
  in
  z
end

let _ =
  let x = 1 in
  x

let%expect_test _ =
  Fmt.pr "@[<v>let x = 1 in@ x@]" ;
  [%expect {|
    let x = 1 in
    x |}]

let _ =
  let y1 =
    let y2 = 2 in
    y2
  in
  y1

let%expect_test _ =
  Fmt.pr "@[<v>@[<v 2>let y1 =@ @[<v 2>let y2 = 2@] in@ y2@]@ in@ y1@]" ;
  [%expect {|
  let y1 =
    let y2 = 2 in
    y2
  in
  y1 |}]

let _ =
  let z1 =
    let z2 =
      let z3 = 2 in
      z3
    in
    z2
  in
  z1

let%expect_test _ =
  Fmt.pr
    "@[<v>@[<v 2>let z1 =@ @[<v 2>let z2 =@ let z3 = 2 in z3@]@ in@ z2@]@ in@ \
     z1@]" ;
  [%expect
    {|
  let z1 =
    let z2 =
      let z3 = 2 in z3
    in
    z2
  in
  z1 |}]

let _ =
  let y1 =
    let y2 =
      "222222222222222222222222222222222222222222222222222222222222222"
    in
    y2
  in
  y1

let%expect_test _ =
  Fmt.pr
    "@[<v>@[<v 2>let y1 =@ @[<v 2>let y2 = \
     22222222222222222222222222222222222222222222222222222222222222222222222222222222222@] \
     in@ y2@]@ in@ y1@]" ;
  [%expect
    {|
  let y1 =
    let y2 = 22222222222222222222222222222222222222222222222222222222222222222222222222222222222 in
    y2
  in
  y1 |}]

let _ =
  let z1 =
    let z2 =
      let z3 =
        "2222222222222222222222222222222222222222222222222222222222222"
      in
      z3
    in
    z2
  in
  z1

let%expect_test _ =
  Fmt.pr "@[<v>pre body@ in@ post" ;
  [%expect {|
  pre body
  in
  post |}] ;
  Fmt.pr "@[<v>@[<v 2>pre@ body_new_line@]@ in@ post@]" ;
  [%expect {|
  pre
    body_new_line
  in
  post |}] ;
  Fmt.pr
    "@[<v>@[<v 2>pre@ @[<v>@[<v 2>pre@ body_new_line@]@ in@ post@]@]@ in@ \
     post@]" ;
  [%expect {|
  pre
    pre
      body_new_line
    in
    post
  in
  post |}] ;
  Fmt.pr
    "@[<v>@[<v 2>pre@ @[<v>@[<v 2>pre@ @[<v>@[<v 2>pre@ body_new_line@]@ in@ \
     post@]@]@ in@ post@]@]@ in@ post@]" ;
  [%expect
    {|
pre
  pre
    pre
      body_new_line
    in
    post
  in
  post
in
post |}] ;

  Fmt.pr "@[<v>@[<v 2>pre body_same_line@]@ in@ post@]" ;
  [%expect {|
pre body_same_line
in
post |}] ;
  Fmt.pr
    "@[<v>@[<v 2>pre@ @[<v>@[<v 2>pre body_same_line@]@ in@ post@]@]@ in@ \
     post@]" ;
  [%expect {|
  pre
    pre body_same_line
    in
    post
  in
  post |}] ;
  ()

(* observe:
   if you are a nested block element, print a leading `@ ` (because you are surrounded in a outer <v> block)
*)
