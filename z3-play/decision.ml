type 'a t =
  (* Branch *)
  | All of 'a * 'a t * 'a t
  | ExactOne of 'a * 'a t * 'a t
  (* Leaf *)
  | Done of 'a
  | Lazy of 'a t lazy_t
[@@deriving show {with_path = false}]

let rec fulfill left right =
  let mid = (left + right) / 2 in
  if left >= right then (
    if mid mod 4 = 0 then
      Lazy (lazy (fulfill (mid*3) (mid*4)))
    else
      Done mid
  )
  else (
    if mid mod 2 = 0 then
      All (mid, fulfill left mid, fulfill (mid+1) right)
    else
      ExactOne (mid, fulfill left mid, fulfill (mid+1) right)
  )

let make_phis t = 
  let phis = ref [] in
  let add_phi phi = 
    phis := phi :: !phis
  in
  let check () = () in
  let rec loop t = 
    match t with
    | Done a -> add_phi a
    | All (p, left, right) -> (
        add_phi p;
        loop left;
        loop right
      )
    | ExactOne (p, left, right) -> (
        add_phi p;
        loop left;
        loop right
      )
    | Lazy _ -> ()
  in
  loop t;
  !phis


let t1 = fulfill 1 1
let t2 = fulfill 1 3
let t3 = fulfill 2 16
let t4 = fulfill 2 4
let t5 = fulfill 2 5

let%expect_test _ =
  print_endline @@ show Fmt.int t1;
  [%expect{| (Done 1) |}];

  print_endline @@ show Fmt.int t2;
  [%expect{|
    (All (2, (ExactOne (1, (Done 1), (Done 2))), (Done 3))) |}];

  print_endline @@ show Fmt.int t3;
  [%expect{|
    (ExactOne (9,
       (ExactOne (5,
          (ExactOne (3, (All (2, (Done 2), (Done 3))),
             (All (4, (Lazy <not evaluated>), (Done 5))))),
          (ExactOne (7, (All (6, (Done 6), (Done 7))),
             (All (8, (Lazy <not evaluated>), (Done 9)))))
          )),
       (ExactOne (13,
          (ExactOne (11, (All (10, (Done 10), (Done 11))),
             (All (12, (Lazy <not evaluated>), (Done 13))))),
          (ExactOne (15, (All (14, (Done 14), (Done 15))), (Lazy <not evaluated>)
             ))
          ))
       )) |}];

  print_endline @@ show Fmt.int t4;
  [%expect{| (ExactOne (3, (All (2, (Done 2), (Done 3))), (Lazy <not evaluated>))) |}];

  print_endline @@ show Fmt.int t5;
  [%expect{|
    (ExactOne (3, (All (2, (Done 2), (Done 3))),
       (All (4, (Lazy <not evaluated>), (Done 5))))) |}];
  ()

let%expect_test _ =
  Fmt.(pr "%a" (Dump.list int) (make_phis t1));
  [%expect{| [1] |}];

  Fmt.(pr "%a" (Dump.list int) (make_phis t2));
  [%expect{| [3; 2; 1; 1; 2] |}];

  Fmt.(pr "%a" (Dump.list int) (make_phis t3));
  [%expect{|
    [15; 14; 14; 15; 13; 12; 11; 10; 10; 11; 13; 9; 8; 7; 6; 6; 7; 5; 4;
     3; 2; 2; 3; 5; 9] |}];

  Fmt.(pr "%a" (Dump.list int) (make_phis t4));
  [%expect{| [3; 2; 2; 3] |}];

  Fmt.(pr "%a" (Dump.list int) (make_phis t5));
  [%expect{| [5; 4; 3; 2; 2; 3] |}];
