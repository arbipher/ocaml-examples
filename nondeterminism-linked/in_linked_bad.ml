open Common

module Linked_list = struct
  type 'a t = Tail | Cons of 'a * 'a t ref

  let rec length = function Tail -> 0 | Cons (_, s) -> 1 + length !s

  let rec concat (e1 : 'a t) (e2 : 'a t) : 'a t =
    match (e1, e2) with
    | Tail, _ -> e2
    | _, Tail -> e1
    | Cons (v1, r1), Cons (v2, r2) ->
        Cons (v1, ref (Cons (v2, ref (concat !r1 !r2))))

  let rec to_list = function Tail -> [] | Cons (v, e) -> v :: to_list !e
  let of_list s = List.fold_left (fun acc v -> Cons (v, ref acc)) Tail s
  let pp (pp_v : 'v Fmt.t) oc s = (Fmt.Dump.list pp_v) oc (List.rev (to_list s))
end

module Nondeter_in_linked_list : Monad_S with type 'a t = 'a Linked_list.t =
struct
  open Linked_list

  type 'a t = 'a Linked_list.t

  let return v = Cons (v, ref Tail)

  let rec bind (ma : 'a t) (f : 'a -> 'b t) : 'b t =
    match ma with
    | Tail -> Tail
    | Cons (v, es) ->
        let e1 : 'b t = f v in
        let e2 : 'b t = bind !es f in
        Linked_list.concat e1 e2
end

module MLL = struct
  open Linked_list

  let fork v = Cons (v, ref (Cons (v, ref Tail)))
  let s0 = Tail
  let s1 = of_list [ 3; 5 ]
end

let%expect_test _ =
  Fmt.pr "%a" (Linked_list.pp Fmt.int) MLL.s1 ;
  [%expect {| [3; 5] |}]

module MSLL = struct
  open Linked_list
  open Nondeter_in_linked_list

  let fork v = Cons (v, ref (Cons (v, ref Tail)))

  let s0 =
    of_list [ Int_set.singleton 1; Int_set.singleton 2; Int_set.singleton 3 ]

  let inc_fork s =
    let s1 = mutation [ 1; 2; 3 ] s in
    let s2 = mutation [ 4; 5; 6 ] s in
    Cons (s1, ref (Cons (s2, ref Tail)))

  let s1 = loop 1 (fun s -> bind s inc_fork) s0
  let s2 = loop 2 (fun s -> bind s inc_fork) s0
  let s3 = loop 3 (fun s -> bind s inc_fork) s0

  let invariant_length n =
    length @@ loop n (fun s -> bind s inc_fork) s0 = 3 * pow2 n
end

let%expect_test _ =
  Fmt.pr "%a" (Linked_list.pp pp_int_set) MSLL.s0 ;
  [%expect {| [[1]; [2]; [3]] |}] ;
  Fmt.pr "%a" (Linked_list.pp pp_int_set) MSLL.s1 ;
  [%expect
    {|
    [[1; 5; 6; 7]; [2; 6; 7; 8]; [1; 2; 3; 4]; [3; 7; 8; 9]; [2; 3; 4; 5];
     [3; 4; 5; 6]] |}]

let%test _ = MSLL.invariant_length 5
