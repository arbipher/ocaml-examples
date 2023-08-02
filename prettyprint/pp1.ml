open Ast
open Format

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
