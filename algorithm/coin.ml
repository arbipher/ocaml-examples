open Base
open Lib

type denom = nat
type coin = nat
type residue = nat
type count = nat
type tuple = coin list * residue * count

let ukds = [1;2;5;10;20;50;100;200]

let urds = [1;2;5;15;20;50;100]

(* getter *)
let coins (cs,_,_) = cs
let residue (_,r,_) = r
let count (_,_,k) = k

let extend : denom -> tuple -> tuple list =
  fun d (cs,r,k) ->
  (0 @.. (r/d)) >>= (fun c ->
      [cs++[c],r-c*d,k+c]
    )

let mktuples : nat -> denom list -> tuple list =
  fun n ->
  foldr (concat_map @. extend) [([],n,0)]

(* checked but slow *)
(* let%test _ =
   length (mktuples 256 ukds) = 10640485 *)

let cost : tuple -> (residue * count) =
  fun t -> (residue t, count t)

let cmp_cost : residue * count -> residue * count -> bool =
  fun (_r1,c1) (_r2,c2) -> c1 <= c2

let (@<=) t1 t2 = (residue t1 = residue t2) && (count t1 <= count t2)

let mkchange : nat -> denom list -> coin list =
  fun n ->
  (* tuple -> coin list *)
  coins 
  (* tuple list -> tuple  *)
  @. (min_with_cmp cmp_cost cost) 
  (* tuple list -> tuple list *)
  @. (thin_by (@<=)) 
  (* demon list -> tuple list *)
  @. (mktuples n)

(* the ocaml stype *)
let mkchange n xs =
  xs
  |> mktuples n
  |> thin_by (@<=)
  |> min_with_cmp cmp_cost cost
  |> coins

let merge_by : ('a -> 'a -> bool) -> 'a list list -> 'a list =
  fun _ _ -> []

let tstep d =
  let cmp t1 t2 = residue t1 >= residue t2 in
  thin_by (@<=)
  @. merge_by cmp
  @. map (extend d)

let mkchange : nat -> denom list -> coin list =
  fun n ->
  coins 
  @. min_with_cmp cmp_cost cost
  @. foldr tstep [([],n,0)]