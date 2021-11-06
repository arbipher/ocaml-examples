open Base
open Lib

let search : (nat * nat -> nat) -> nat -> (nat * nat) list =
  fun f t ->
  List.(
    cartesian_product 
      (range 0 t)
      (range 0 t)
    |>
    filter ~f:(fun (x,y) -> t = f (x,y))
  )

let search : (nat * nat -> nat) -> nat -> (nat * nat) list =
  fun f t ->
  List.(
    bind (nrange 0 t) ~f:(fun x -> 
        bind (nrange 0 t) ~f:(fun y -> 
            if t = f (x,y) then
              [x,y]
            else
              []
          ))
  )

let%expect_test _ =
  pr_pair_list @@ search (fun (x,y)-> x+y) 5;
  [%expect{| [0, 5]; [1, 4]; [2, 3]; [3, 2]; [4, 1]; [5, 0] |}]

let search : (nat * nat -> nat) -> nat -> (nat * nat) list =
  fun f t ->
  List.(
    bind (nrange 0 t) ~f:(fun x -> 
        bind (range_downto t 0) ~f:(fun y -> 
            if t = f (x,y) then
              [x,y]
            else
              []
          ))
  )

let%expect_test _ =
  pr_pair_list @@ search (fun (x,y)-> x+y) 5;
  [%expect{| [0, 5]; [1, 4]; [2, 3]; [3, 2]; [4, 1]; [5, 0] |}]

let searchIn (a,b) f t =
  List.(
    (nrange a t) |> bind ~f:(fun x -> 
        (range_downto b 0) |> bind  ~f:(fun y -> 
            if t = f (x,y) then
              [x,y]
            else
              []
          ))
  )

(* intuition:
   starting at (0,t) as (x,y)
   | f (x,y) < t -> x moves right, must increase
   | f (x,y) = t -> x moves right, y moves down, maybe
   | f (x,y) > t -> y moves down, must descease
*)
let search f t =
  let rec searchIn (x,y) = 
    if x > t || y < 0 then []
    else 
      let z = f (x,y) in
      if z < t then searchIn (x+1,y)
      else if z = t then (x,y)::(searchIn (x+1,y-1))
      else (* z > t *) searchIn(x, y-1)
  in
  searchIn (0, t)

let%expect_test _ =
  pr_pair_list @@ search (fun (x,y)-> x+y) 5;
  [%expect{| [0, 5]; [1, 4]; [2, 3]; [3, 2]; [4, 1]; [5, 0] |}]

(* let%expect_test _ =
   pr_pair_list @@ search (fun (x,y)-> x ** 2 + 3 ** y) 20259;
   [%expect.unreachable] *)