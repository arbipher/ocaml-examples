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
  Fmt.pr "@[<v>@[<v 2>let y1 =@ @[<v>@[<v 2>let y2 = 2@]@ in@ y2@]@]@ in@ y1@]" ;
  (*      o1----------------------------------------------------------------o1 *)
  [%expect {|
    let y1 =
      let y2 = 2
      in
      y2
    in
    y1 |}]

let%expect_test _ =
  Fmt.pr "@.%s\n world!!!" "Hello," ;
  [%expect {|
    Hello,
     world!!! |}]
