open Base
open Lib

type name = string
type value = nat
type weight = nat
type item = name * value * weight
type selection = name list * value * weight

let name (n,_,_) = n
let value (_,v,_) = v
let weight (_,_,w) = w

let within : weight -> selection -> bool =
  fun w sn -> weight sn <= w

let add : item -> selection -> selection =
  fun i (ns,v,w) -> (name i :: ns, value i + v, weight i + w)

let selections : item list -> selection list =
  let extend i sn = [sn; add i sn] in
  foldr (concat_map @. extend) [([],0,0)]

let swag : weight -> item list -> selection =
  fun w ->
  max_with value
  @. filter (within w)
  @. selections

let choices : weight -> item list -> selection list =
  fun w ->
  let extend i sn = filter (within w) [sn; add i sn] in
  foldr (concat_map @. extend) [([],0,0)]

let swag w = 
  let extend i sn = filter (within w) [sn; add i sn] in
  let tstep i = 
    thin_by (fun sn1 sn2 -> 
        value sn1 >= value sn2 && weight sn1 <= weight sn2)
    @. concat_map (extend i) 
  in
  max_with value
  @. foldr tstep [([],0,0)]

let swag w = 
  let extend i sn = filter (within w) [sn; add i sn] in
  let cmp sn1 sn2 = weight sn1 <= weight sn2 in
  let tstep i = 
    thin_by (fun sn1 sn2 -> 
        value sn1 >= value sn2 && weight sn1 <= weight sn2)
    @. merge_by cmp
    @. map (extend i) 
  in
  max_with value
  @. foldr tstep [([],0,0)]