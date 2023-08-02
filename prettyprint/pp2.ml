open Ast
open Format

(*
   | Let(Ident(i), e1, e2) ->
       ff fmt "@[<hv>Let %s =@;<1 4>%a@;<1 0>In@;<1 4>%a@]"
         i pp_expr e1 pp_expr e2

    | Let (Ident i, e1, e2) ->
      ff fmt "@[<hv>Let %s =@;<1 4>%a@;<1 0>In@\n%a@]" i pp_expr e1 pp_expr e2
*)
(*
   let rec pp_exp ?(top = true) fmter e =
     let _ = top in
     match e with
     | Let (x, e1, e2) ->
         if not top then fprintf fmter "@\n" ;
         fprintf fmter "@[<hv 2>let %a =@ %a@;<1 0>in@]@\n%a" pp_print_string x
           (pp_exp ~top:false) e1
           (* if is_block e1 then fprintf fmter "@ " ; *)
           (pp_exp ~top:true)
           e2
     | Num n -> pp_print_int fmter n
     | Var s -> pp_print_string fmter s *)

let rec pp_exp ?(top = true) fmter e =
  match e with
  | Let (x, e1, e2) ->
      if not top then fprintf fmter "@\n" ;
      fprintf fmter "@[<hv 2>let %a = %a@ in@] " pp_print_string x
        (pp_exp ~top:false) e1 ;
      (* if is_block e1 then fprintf fmter "@\n" ; *)
      (* if not top then fprintf fmter "@\n" ; *)
      fprintf fmter "@\n%a" (pp_exp ~top:true) e2
  | Num n -> pp_print_int fmter n
  | Var s -> pp_print_string fmter s
