open Base
open Lib

let search : (nat -> nat) -> nat -> nat list =
  fun f t -> (nrange 0 t) |> List.filter ~f:(fun x -> t = f x)

let search : (nat -> nat) -> nat -> nat list =
  fun f t -> 
  let seek a b = (nrange a b) |> List.filter ~f:(fun x -> t = f x)
  in
  seek 0 t

let search : (nat -> nat) -> nat -> nat list =
  fun f t -> 
  let rec seek a b = 
    let m = (a + b) / 2 in
    if a > b then
      []
    else if t < f m then
      seek a (m-1)
    else if t = f m then
      [m]
    else
      seek (m+1) b
  in
  seek 0 t

let bound : (nat -> nat) -> nat -> int * nat =
  fun f t ->
  if t <= f 0 then
    (-1,0)
  else 
    let b = until (fun b -> t <= f b) (fun x -> 2 * x) 1 in
    (b/2, b)

let rec smallest (a,b) f t = 
  if a+1 = b then b
  else 
    let m = (a+b) / 2 in
    if t <= f m then smallest (a,m) f t
    else smallest (m,b) f t

let search : (nat -> nat) -> nat -> nat list =
  fun f t ->
  let x = smallest (bound f t) f t in
  if f x = t then [x] else []