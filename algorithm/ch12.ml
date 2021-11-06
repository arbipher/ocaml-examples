open Base
open Lib

type 'a segment = 'a list

type 'a partition = 'a segment list

(* concat xss = xs && all (not @. null) xss *)

let rec splits : 'a list -> ('a list * 'a list) list = 
  function
  | [] -> []
  | x::xs ->
    ([x],xs) :: ((splits xs) >>= (fun (ys,zs) -> [(x::ys,zs)]))

let cons : 'a -> 'a partition -> 'a partition = 
  fun x p -> [x]::p

let glue : 'a -> 'a partition -> 'a partition = 
  fun x -> function
    | y::ys -> (x::y)::ys
    | _ -> failwith "glue"

let extendl : 'a -> 'a partition -> 'a partition list =
  fun x -> function
    | [] -> [cons x []]
    | p -> [cons x p; glue x p]

let snoc : 'a -> 'a partition -> 'a partition =
  fun x p -> p ++ [[x]]

let bind : 'a -> 'a partition -> 'a partition =
  fun x p -> init p ++ [last p ++ [x]]

let extendr : 'a -> 'a partition -> 'a partition =
  fun x -> function
    | [] -> [snoc x []]
    | p -> [snoc x p; bind x p]

let parts : 'a list -> 'a partition list =
  foldr (concat_map @. extendl) [[]]

let rec parts : 'a list -> 'a partition list =
  function
  | [] -> [[]]
  | xs -> 
    (splits xs) >>= (fun (ys,zs) ->
        (parts zs) >>= (fun yss ->
            [ys :: yss]
          ))

let parts = foldl (flip (concat_map @. extendr)) [[]]