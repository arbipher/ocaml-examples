(*
open Base
open Lib

type data = int
type candidate = int

let anon = []
let cost _x = 0
let good _x = false
let candidates _xs = []
let extend _x ds = ds
let (@<=) x _y = x
let value x = x

 let best : data list -> candidate =
   min_with cost
   @. filter good
   @. candidates

   let candidates : data list -> candidate list =
   foldr (concat_map @. extend) anon

   let best : data list -> candidate =
   let step d = 
    thin_by (@<=)
    @. concat_map (filter good @. extend d)
   in
   min_with cost
   @. foldr step anon *)

(* let best = 
   let cmp x y = value x <= value y in
   let step d =
    thin_by (@<=)
    @. merge_by cmp
    @. map (filter good @. extend d)
   in
   min_with cost
   @. foldr step anon *)