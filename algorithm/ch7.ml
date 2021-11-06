open Base
open Lib

let mcc : 'component list -> 'candidate = 
  fun candidates ->
  let cost x = x in
  (min_with cost) candidates

let extend : 'component -> 'candidate -> 'candidate list =
  fun _co c1 -> [c1]

(* let condidates : 'component list -> 'candidate list =
   fun xs -> foldr step [] *)

let rec extend x = function
  | [] -> [[x]]
  | y::ys -> (x::y::ys) :: (map ((@:) y) (extend x ys))

let gstep x = (min_with (fun xs -> head xs)) @. (extend x)

(* let perm = foldr (concat_map @. extend) [[]] *)

let%expect_test _ =
  pr_list @@ gstep 1 [2;3;4];
  [%expect{| [1; 2; 3; 4] |}]

let rec gstep x = function
  | [] -> [x]
  | y::ys ->
    if x <= y then x::y::ys
    else y :: gstep x ys

let insert_sort xs = foldr gstep [] xs

let usds = [100;50;25;10;5;1]

let ukds = [200;100;50;20;10;5;2;1]

let amount ds cs = foldl1 (+) (map (fun (x,y) -> x * y) (zip ds cs))

let rec mktuples ds n =
  match ds with
  | [1] -> [[n]]
  | d::ds ->
    List.bind (nrange 0 (n/d)) ~f:(fun c ->
        List.bind (mktuples ds (n-c*d)) ~f:(fun cs ->
            [c::cs]
          )
      )
  | _ -> failwith "mktuples"

let mkchange : 'denom list -> nat -> nat list = 
  let count = foldl1 (+) in
  fun ds ->
    (min_with count) @. (mktuples ds)

let%expect_test _ =
  prll @@ mktuples [3;2;1] 5;
  [%expect{| [0; 0; 5]; [0; 1; 3]; [0; 2; 1]; [1; 0; 2]; [1; 1; 0] |}]

let rec mkchange ds n = 
  match ds with
  | [1] -> [n]
  | d::ds -> 
    let c = n/d in
    mkchange ds (n-c*d)
  | _ -> failwith "mktuples"
