open Base
open Lib

(* sort = flatten·mktree *)

let a = [2;3;1;5;4]

module Qsort = struct
  type 'a tree =
    | Null
    | Node of 'a tree * 'a * 'a tree

  let rec flatten : 'a tree -> 'a list = function
    | Null -> []
    | Node(l,x,r) -> flatten l ++ [x] ++ flatten r

  let (@<) x y = x < y
  let rec mktree = function
    | [] -> Null
    | x::xs -> (
        let ys,zs = List.partition_tf xs ~f:(fun t -> t @< x) in
        Node((mktree ys), x, (mktree zs))
      )

  let sort = flatten @. mktree

  let%expect_test _ =
    pr_list @@ sort a;
    [%expect{| [1; 2; 3; 4; 5] |}]

  let rec qsort = function
    | [] -> []
    | x::xs -> (
        let ys,zs = List.partition_tf xs ~f:(fun t -> t @< x) in
        qsort ys ++ [x] ++ qsort zs
      )

  let%expect_test _ =
    pr_list @@ qsort a;
    [%expect{| [1; 2; 3; 4; 5] |}]
end

module Merge_sort = struct
  type 'a tree = 
    | Null
    | Leaf of 'a
    | Node of 'a tree * 'a tree

  let leaf x = Leaf x

  let node x y = Node(x,y)

  let rec flatten = function
    | Null -> []
    | Leaf x -> [x]
    | Node(l,r) -> flatten l ++ flatten r

  let rec pair_with f = function
    | [] -> []
    | [x] -> [x]
    | x::y::xs -> f x y :: pair_with f xs

  let mktree = function
    | [] -> Null
    | xs -> unwrap @@ until single (pair_with node) (map leaf xs)

  let%test _ =
    Poly.equal a (a |> mktree |> flatten)

  let rec merge xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x::xs, y::ys -> 
      if x <= y then 
        x :: merge xs (y::ys)
      else
        y :: merge (x::xs) ys

  let msort = 
    let runs = foldr (fun x -> function
        | [] -> [[x]]
        | (y::ys)::xss ->
          if x <= y then (x::y::ys)::xss
          else [x]::(y::ys)::xss
        | _ -> failwith "runs op"
      ) []
    in
    function
    | [] -> []
    | xs -> unwrap @@ until single (pair_with merge) (runs xs)
end