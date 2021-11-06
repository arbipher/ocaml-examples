open Base

let map_fields_value fields f =
  List.map fields ~f:(fun (k, v) -> (k, f v))

let make_ref () = ref 0

let make_counter () = 
  let last = make_ref () in 
  fun () -> Int.incr last ; !last

let pe = Stdio.print_endline
let pf = Stdio.printf

let keys_of_assoc lst = List.map lst ~f:fst

let values_of_assoc lst = List.map lst ~f:snd
