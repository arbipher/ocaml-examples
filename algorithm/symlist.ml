open Base
open Lib

module type SYMLIST = sig
  type 'a t
  val fromSL : 'a t -> 'a list
  val consSL : 'a -> 'a t -> 'a t
  val snocSL : 'a -> 'a t -> 'a t
  val headSL : 'a t -> 'a
  val lastSL : 'a t -> 'a
  val tailSL : 'a t -> 'a t
  val initSL : 'a t -> 'a t
  val nullSL : 'a t -> bool
  val singleSL : 'a t -> bool
  val lengthSL : 'a t -> int
  val nilSL : 'a t
end

module SymList : SYMLIST = struct
  type 'a t = 'a list * 'a list

  (* let bottom = failwith "bottom" *)
  let bottom = [],[]

  let nilSL = [],[]

  let fromSL (xs,ys) = xs ++ reverse ys

  let snocSL x (xs,ys) = 
    if null xs then
      ys, [x]
    else
      xs, x::ys

  let lastSL (xs,ys) =
    if null ys then
      head xs
    else
      head ys

  let lastSL (xs,ys) =
    if null ys then
      if null xs then
        failwith "lastSL of empty list"
      else
        head xs
    else
      head ys

  let headSL (xs, ys) =
    if null xs then
      if null ys then
        failwith "headSL of empty list"
      else
        head ys
    else
      head xs

  let tailSL (xs,ys) =
    match (xs,ys) with
    | _ when null xs -> 
      if null ys then
        bottom
      else
        nilSL
    | _ when single xs ->
      let us,vs = List.split_n ys ((length ys) / 2) in
      (reverse vs, us)
    | _ -> (tail xs, ys)

  let initSL (xs,ys) =
    match (xs,ys) with
    | _ when null ys ->
      if null xs then
        bottom
      else
        (init xs, ys)
    | _ when single ys ->
      let us,vs = List.split_n xs ((length xs) / 2) in
      us, (reverse vs)
    | _ -> (xs, init ys)

  let consSL z (xs,ys) =
    (z::xs,ys)

  let nullSL (xs,ys) =
    null xs && null ys

  let singleSL (xs,ys) =
    single (xs++ys)

  let lengthSL (xs,ys) =
    length xs + length ys
end