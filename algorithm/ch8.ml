open Base
open Lib

type 'a tree = 
  | Leaf of 'a
  | Node of 'a tree * 'a tree

let leaf x = Leaf x
let node u v = Node(u, v)

let rec size = function
  | Leaf _ -> 0
  | Node(u,v) -> 1 + size u + size v

let rec height = function
  | Leaf _ -> 0
  | Node(u,v) -> 1 + max (height u) (height v)

let rec fringe = function
  | Leaf x -> [x]
  | Node(u,v) -> fringe u ++ fringe v

let rec pair_with f = function
  | [] -> []
  | [x] -> [x]
  | x::y::xs -> f x y :: pair_with f xs

let rec mktree = function
  | [] -> failwith "mktree"
  | [x] -> Leaf x
  | xs -> 
    let us,vs = List.split_n xs ((length xs) / 2) in
    Node (mktree us, mktree vs)
let mktree xs = unwrap @@ until single (pair_with node) (map leaf xs)

let%test _ =
  let a = [1;2;3] in
  Poly.equal a (fringe (mktree a))

(* leftmost x *)
let rec extend x = function
  | Leaf y -> [Node (Leaf x, Leaf y)]
  | Node (u,v) -> 
    Node(Leaf x, Node(u,v)) 
    :: map (fun u' -> Node (u',v)) (extend x u)

let rec mktrees = function
  | [x] -> [Leaf x]
  | x::xs -> concat_map (extend x) (mktrees xs)
  | [] -> failwith "mktrees"

let mktrees xs = foldrn (concat_map @. extend) (wrap @. leaf) xs

type 'a forest = 'a tree list

let rollup : 'a forest -> 'a tree =
  fun forest ->
  foldl1 node forest

let rec spine = function
  | Leaf x -> [Leaf x]
  | Node (u,v) -> spine u ++ [v]

let%test _ =
  Poly.([Leaf 1] = ([Leaf 1] |> rollup |> spine))

let%test _ =
  let ts = [Leaf 1; Node (Leaf 2, Leaf 3)] in
  Poly.(ts = (ts |> rollup |> spine))

(* let extend x ts = 
   map (fun k ->
      [Leaf x :: (rollup (take k ts)) :: (drop k ts)]
    ) (nrange 1 @@ length ts) 

   let mkforests xs = foldrn (concat_map @. extend) (wrap @. wrap @. leaf) xs
   let mktrees = map (rollup @. mkforests)

   let cost _ = 3

   let mct xs = min_with cost (mktrees xs) *)