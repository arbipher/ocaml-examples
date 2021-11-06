open Angstrom
let expr = 
  fix (fun expr ->
    choice [char '2'; char '1'; char '1' *> (char '2')]
  )

let eval1 s =
  match parse_string ~consume:All expr s with
  | Ok v      -> v
  | Error msg -> failwith msg

let no_fix =
  choice [char '2'; char '1'; char '1' *> (char '2')]

let eval2 s =
  match parse_string ~consume:All expr s with
  | Ok v      -> v
  | Error msg -> failwith msg

;;
eval1 "1";;
(* eval1 "12";; *)

eval2 "1";;
(* eval2 "12";; *)
