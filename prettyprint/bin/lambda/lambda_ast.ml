type lambda =
  | Lambda of string * lambda
  | Var of string
  | Apply of lambda * lambda

open Format

let ident ppf s = fprintf ppf "%s" s
let kwd ppf s = fprintf ppf "%s" s
let nl () = Format.fprintf Format.std_formatter "\n"
