open Common

module Nondeter_in_list : Monad_S with type 'a t = 'a list = struct
  type 'a t = 'a list

  let return e = [ e ]
  let bind ma f = List.concat_map f ma
end

(* test *)

module ML = struct
  include Nondeter_in_list

  let fork s = [ s; s ]
end

(* int as element *)

let%test _ =
  let open ML in
  let s0 = [ 1; 2; 3 ] in
  let s1 = bind s0 fork in
  let s2 = bind s1 fork in
  List.length s2 = 12

(* int_set as element *)

module ML_example = struct
  let s0 = [ Int_set.singleton 1; Int_set.singleton 2; Int_set.singleton 3 ]
  let inc_fork s = [ mutation [ 1; 2; 3 ] s; mutation [ 4; 5; 6 ] s ]
  let s1 = loop 1 (fun s -> ML.bind s inc_fork) s0
  let s2 = loop 2 (fun s -> ML.bind s inc_fork) s0
  let s3 = loop 3 (fun s -> ML.bind s inc_fork) s0

  let invariant_length n =
    List.length @@ loop n (fun s -> ML.bind s inc_fork) s0 = 3 * pow2 n
end

let%expect_test _ =
  Fmt.pr "%a" (Fmt.Dump.list pp_int_set) ML_example.s1 ;
  [%expect
    {|
    [[1; 2; 3; 4]; [1; 5; 6; 7]; [2; 3; 4; 5]; [2; 6; 7; 8]; [3; 4; 5; 6];
     [3; 7; 8; 9]]
    
    |}] ;
  Fmt.pr "%a" (Fmt.Dump.list pp_int_set) ML_example.s2 ;
  [%expect
    {|
    [[1; 2; 3; 4; 5; 6; 7]; [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
     [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]; [1; 5; 6; 7; 9; 10; 11; 12; 13];
     [2; 3; 4; 5; 6; 7; 8]; [2; 3; 4; 5; 6; 7; 8; 9; 10; 11];
     [2; 3; 4; 5; 6; 7; 8; 9; 10; 11]; [2; 6; 7; 8; 10; 11; 12; 13; 14];
     [3; 4; 5; 6; 7; 8; 9]; [3; 4; 5; 6; 7; 8; 9; 10; 11; 12];
     [3; 4; 5; 6; 7; 8; 9; 10; 11; 12]; [3; 7; 8; 9; 11; 12; 13; 14; 15]] |}] ;
  Fmt.pr "%a" (Fmt.Dump.list pp_int_set) ML_example.s3 ;
  [%expect
    {|
       [[1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
        [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
        [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
        [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
        [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
        [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
        [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
        [1; 5; 6; 7; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19];
        [2; 3; 4; 5; 6; 7; 8; 9; 10; 11];
        [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14];
        [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14];
        [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17];
        [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14];
        [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17];
        [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17];
        [2; 6; 7; 8; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];
        [3; 4; 5; 6; 7; 8; 9; 10; 11; 12];
        [3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15];
        [3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15];
        [3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18];
        [3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15];
        [3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18];
        [3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18];
        [3; 7; 8; 9; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21]] |}]

(* let%test _ = ML_example.invariant_length 17 *)

(* bench *)
let%bench "nondeter in list" = ignore @@ ML_example.invariant_length 15
