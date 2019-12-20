open Angstrom

type exp = 
  | Int of int
  | Plus of exp * exp

let (let*) = (>>=)

let ws = skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)

let lchar c =
  ws *> char c

let parens p = lchar '(' *> p <* lchar ')'

let is_digit =
  function '0' .. '9' -> true | _ -> false

let integer =
  ws *>
  (take_while1 is_digit >>| (fun x -> Int (int_of_string x)))
  (* <* end_of_input *)


let int_plus = 
  (* parens @@ *)
  let* es = (sep_by1 (lchar '+') integer) in
  let e1, er = List.hd es, List.tl es in
  (return (List.fold_left (fun acc e -> Plus (acc, e)) e1 er))
(* <* (lchar ';') *)

let expr : exp t = 
  fix (fun expr ->
    let plus = 
      (sep_by1 (lchar '+') expr)
      >>| 
      fun es ->
        let e1, er = List.hd es, List.tl es in
        (List.fold_left (fun acc e -> Plus (acc, e)) e1 er)
    in
    ws *> 
    (* peek_char_fail >>= fun _ -> *)
    (* (integer <|> plus) *)
    (choice [integer; plus ])
    )

let eval (s : string) =
  match parse_string (expr <* end_of_input) s with
  | Ok v      -> v
  | Error msg -> failwith msg

let ps = parse_string int_plus

let p1 = "1";;

eval p1;;
ps p1;;

let p2 = "1 + 1";;

eval p2;;
ps p2;;