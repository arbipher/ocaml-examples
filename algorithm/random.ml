open Base

let rec reorder xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | [x;y] -> [x;y]
  | x::xs -> 
    let acc = pick_end xs [] in
    x :: acc

and pick_end xs acc =
  match xs with
  | [x] -> x :: reorder acc
  | y::ys -> pick_end ys (acc@[y])
  | _ -> failwith "pick_end"

let%test _ =
  Poly.equal (reorder []) []

let%test _ =
  Poly.equal (reorder [1]) [1]

let%test _ =
  Poly.equal (reorder [1;2]) [1;2]

let%test _ =
  Poly.equal (reorder [1;2;3;4]) [1;4;2;3]

let%test _ =
  Poly.equal (reorder [1;2;3;4;5]) [1;5;2;4;3]
