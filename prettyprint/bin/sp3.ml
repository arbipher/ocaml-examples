type exp = Let of string * exp * exp | Num of int | Var of string

let e1_in_x e x = Let (x, e, Var x)
let e2_in_x x e = Let (x, Num 1, e)
let let_ x e1 e2 = Let (x, e1, e2)

open Format

let is_block = function Let _ -> true | _ -> false

let rec pp_exp ?(top = true) fmter e =
  match e with
  | Let (x, e1, e2) ->
      if not top then fprintf fmter "@ " ;
      fprintf fmter "@[<v>@[<v 2>let %a = %a@] " pp_print_string x
        (pp_exp ~top:false) e1 ;
      if is_block e1 then fprintf fmter "@ " ;
      fprintf fmter "in@ %a@]" (pp_exp ~top:true) e2
  | Num n -> pp_print_int fmter n
  | Var s -> pp_print_string fmter s

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

let () =
  Format.printf "@[%a@]@\n@\n" (pp_exp ~top:true) e1 ;
  Format.printf "@[%a@]@\n@\n" (pp_exp ~top:true) e2 ;
  Format.printf "@[%a@]@\n@\n" (pp_exp ~top:true) e3 ;
  Format.printf "@[%a@]@\n@\n" (pp_exp ~top:true) e4 ;
  Format.printf "@[%a@]@\n@\n" (pp_exp ~top:true) e5 ;
  Format.printf "@[%a@]@\n@\n" (pp_exp ~top:true) e6 ;
  Format.printf "@[%a@]@\n@\n" (pp_exp ~top:true) e8 ;
  Format.printf "@[%a@]@\n@\n" (pp_exp ~top:true) e9 ;
  Format.printf "@[%a@]@\n@\n" (pp_exp ~top:true) e10
