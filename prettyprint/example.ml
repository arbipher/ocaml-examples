open Ast

let n1 = Num 1

let _ =
  let x = 1 in
  x

let e1 = e1_in_x n1 "x"

let _ =
  let y =
    let x = 1 in
    x
  in
  y

let e2 = e1_in_x e1 "y"

let _ =
  let z =
    let y =
      let x = 1 in
      x
    in
    y
  in
  z

let e3 = e1_in_x e2 "z"

let _ =
  let x =
    let x = 1 in
    x
  in
  let x = 1 in
  x

let e4 = let_ "x" e1 e1

let _ =
  let x =
    let x = 1 in
    x
  in
  let y =
    let x = 1 in
    x
  in
  y

let e5 = let_ "x" e1 e2

let _ =
  let x =
    let y =
      let x = 1 in
      x
    in
    y
  in
  let y =
    let x = 1 in
    x
  in
  y

let e6 = let_ "x" e2 e2

let _ =
  let x = 1 in
  1

let e7 = e2_in_x "x" n1

let _ =
  let x = 1 in
  let x = 1 in
  1

let e8 = e2_in_x "x" e7

let _ =
  let x =
    let x = 1 in
    let x = 1 in
    1
  in
  x

let e9 = e1_in_x e8 "x"

let _ =
  let x = 1 in
  let x =
    let x = 1 in
    let x = 1 in
    1
  in
  x

let e10 = e2_in_x "x" e9
let all = [ e1; e2; e3; e4; e5; e6; e8; e9; e10 ]
