open Base
open Lib

type vertex = int
type weight = int
type edge = vertex * vertex * weight
type path = edge list
type net = edge list list

let source : edge -> vertex =
  fun (u,_v,_w) -> u

let target : edge -> vertex =
  fun (_u,v,_w) -> v

let weight : edge -> weight =
  fun (_u,_v,w) -> w
let cost = sum @. (map weight)

let linked : edge -> path -> bool =
  fun e1 -> function
    | [] -> true
    | e2::_es -> target e1 = source e2

let rec connected : path -> bool = 
  function
  | [] -> true
  | e::es -> linked e es && connected es

let paths : net -> path list =
  filter connected @. cp

let step es ps = 
  let cons e ps = 
    List.filter ps ~f:(linked e) in
  List.bind es ~f:(fun e -> [cons e ps])
  |> concat

let paths = foldr step [[]]

let (@<=) p1 p2 = 
  source (head p1) = source (head p2)
  && cost p1 <= cost p2

(* mcp for minimum-cost path *)
let mcp : net -> path =
  fun paths ->
  (min_with cost) paths

let mcp =
  let tstep es ps =
    thin_by (@<=) (
      List.bind es ~f:(fun e -> (
            List.bind ps ~f:(fun p -> (
                  if linked e p then
                    [e::p]
                  else
                    []
                ))
          ))
    ) in
  (min_with cost) @. (foldr tstep [[]])

let g1 = [
  [(1,5,2);(1,6,7);(2,6,1);(3,6,4);(3,7,5);(4,7,2);(4,8,3)];
  [(5,9,5);(6,9,3);(6,10,9);(6,11,8);(7,11,2);(8,11,7);(8,12,1)];
  [(9,13,4);(9,14,8);(10,14,2);(10,15,5);(11,15,6);(11,16,3);(12,16,7)]
]

let%expect_test _ =
  [%sexp_of: (int * int * int) list] (mcp g1) |> prs;
  [%expect{| ((4 7 2) (7 11 2) (11 16 3)) |}]

let%test _ =
  Poly.equal (mcp g1) [(4,7,2);(7,11,2);(11,16,3)]