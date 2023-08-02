type exp = Let of string * exp * exp | Num of int | Var of string

let e1_in_x e x = Let (x, e, Var x)
let e2_in_x x e = Let (x, Num 1, e)
let let_ x e1 e2 = Let (x, e1, e2)
let is_block = function Let _ -> true | _ -> false
