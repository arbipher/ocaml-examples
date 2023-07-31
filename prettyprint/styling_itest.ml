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

(* observe:
   if you are a nested block element, print a leading `@ ` (because you are surrounded in a outer <v> block)
*)

let%expect_test _ =
  let _ =
    let x = 1 in
    x
  in

  Fmt.pr "@[<v>let x = 1@ in@ x@]" ;
  [%expect {|
  let x = 1
  in
  x |}] ;

  Fmt.pr "@[<v>@[<v 2>let x = 1@] in@ x@]" ;
  [%expect {|
let x = 1 in
x |}] ;

  Fmt.pr "@[<v>@[<v 2>let x = @ 123456789@]@ in@ x@]" ;
  [%expect {|
  let x =
    123456789
  in
  x |}] ;

  Fmt.pr
    "@[<v>@[<v 2>let x = @ @[<v>@[<v 2>let y =@ 123456789@]@ in@ y@]@]@ in@ x@]" ;
  [%expect {|
  let x =
    let y =
      123456789
    in
    y
  in
  x |}] ;
  Fmt.pr
    "@[<v>@[<v 2>let x = @ @[<v>@[<v 2>let y =@ @[<v>@[<v 2>let z =@ \
     123456789@]@ in@ z@]@]@ in@ y@]@]@ in@ x@]" ;
  [%expect
    {|
let x =
  let y =
    let z =
      123456789
    in
    z
  in
  y
in
x |}] ;

  let _ =
    let x =
      let y = 1 in
      y
    in
    x
  in

  Fmt.pr "@[<v>@[<v 2>let x = @ @[<v>@[<v 2>let y = 1@] in@ y@]@]@ in@ x@]" ;
  [%expect {|
  let x =
    let y = 1 in
    y
  in
  x |}] ;
  ()
