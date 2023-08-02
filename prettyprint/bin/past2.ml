open Example

let pp_exp = Pp2.pp_exp ~top:true
let () = List.iter (fun e -> Format.printf "@[%a@]@\n@\n" pp_exp e) Example.all
