open Leak

(* [(---[(----[(---b)]b)]b)] *)
let boxed_parens cbox ff = cbox Fmt.(any "(" ++ ff ++ cut ++ any ")")
let ci () = Format_helper.dump_string Format.std_formatter

let _p1 cbox =
  let par = boxed_parens cbox in
  let fmt = Fmt.(par (any "---" ++ par (any "----" ++ par (any "---")))) in
  Fmt.pr "[%s]%a" (ci ()) fmt () ;
  Fmt.pr "\n"

let p2 cbox =
  cbox () ;
  Format.print_string "(---" ;
  cbox () ;
  Format.print_string "(----" ;
  Format.print_string (ci ()) ;
  cbox () ;
  Format.print_string "(---" ;
  Format.print_cut () ;
  Format.print_string ")" ;
  Format.print_string (ci ()) ;
  Format.close_box () ;
  Format.print_cut () ;
  Format.print_string ")" ;
  Format.close_box () ;
  Format.print_cut () ;
  Format.print_string ")" ;
  Format.close_box () ;
  ()

let () =
  Format.set_margin 7 ;
  p2 (fun () -> Format.open_hovbox 1) ;
  Format.print_newline () ;
  Format.printf "----\n---\n" ;
  p2 (fun () -> Format.open_box 1) ;
  Format.print_newline () ;
  (* Format.print_string (ci ()) ; *)
  ()
(*
   let () =
     Format.set_margin 8 ;
     p1 (Fmt.box ~indent:1) ;
     (*

   (---
    (----
     (---)))
     *)
     p1 (Fmt.hovbox ~indent:1) ;
     (*

   (---
    (----
     (---
     )
    )
   )
     *)
     () *)
