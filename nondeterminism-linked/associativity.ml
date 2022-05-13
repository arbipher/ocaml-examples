open Base
open Common

(* sum *)

let sum a b =
  List.init (b - a + 1) ~f:(fun i -> i + a) |> List.fold ~f:( + ) ~init:0

let rec sum_rec a b = if a = b then b else a + sum_rec (a + 1) b

let sum_acc a b =
  let rec loop a acc = if a > b then acc else loop (a + 1) (a + acc) in
  loop a 0

let sum_in_2 f n =
  let m = n / 2 in
  let s1 = f 1 m in
  let s2 = f (m + 1) n in
  s1 + s2

let sum_in_4 f n =
  let m1 = n / 4 in
  let m2 = n / 2 in
  let m3 = n * 3 / 4 in
  let s1 = f 1 m1 in
  let s2 = f (m1 + 1) m2 in
  let s3 = f (m2 + 1) m3 in
  let s4 = f (m3 + 1) n in
  s1 + s2 + s3 + s4

(* c/p *)

let touch _ = 1

let list_make_to f n =
  let open In_list in
  let fork s = [ 0 :: s; 1 :: s ] in
  let s0 = [ [] ] in
  let ss = loop n (fun s -> ML.bind s fork) s0 in
  let vv = List.map ~f ss in
  let ans = List.fold ~f:( + ) ~init:0 vv in
  ans
