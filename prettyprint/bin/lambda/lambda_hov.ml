(* hov box (packing) *)

open Lambda_ast
open Format

let rec pr_exp0 ppf = function
  | Var s -> fprintf ppf "%a" ident s
  | lam -> fprintf ppf "@[<hov 1>(%a@,)@]" pr_lambda lam

and pr_app ppf e = fprintf ppf "@[<hov 2>%a@]" pr_other_applications e

and pr_other_applications ppf f =
  match f with
  | Apply (f, arg) -> fprintf ppf "%a@ %a" pr_app f pr_exp0 arg
  | f -> pr_exp0 ppf f

and pr_lambda ppf = function
  | Lambda (s, lam) ->
      fprintf ppf "@[<hov 1>%a%a%a@ %a@]" kwd "\\" ident s kwd "." pr_lambda lam
  | e -> pr_app ppf e

let print_lambda e =
  pr_lambda std_formatter e ;
  nl ()

let eprint_lambda = pr_lambda err_formatter
