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

  let mutation deltas s =
    Int_set.fold
      (fun e s -> List.fold_left (fun s i -> Int_set.add (e + i) s) s deltas)
      s s

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

(* module Nondeter_in_set' (Ord : Set.OrderedType) :
     Poor_monad_S with type t = Set.Make(Ord).t and type a = Set.Make(Ord).elt =
   struct
     module This_set = Set.Make (Ord)

     type a = This_set.elt
     type t = This_set.t

     let return e = This_set.singleton e

     let bind sa f =
       This_set.fold
         (fun e acc ->
           let sb = f e in
           This_set.union acc sb)
         sa This_set.empty
   end *)

module Nondeter_in_set (Set : Set.S) :
  Poor_monad_S with type t = Set.t and type a = Set.elt = struct
  type a = Set.elt
  type t = Set.t

  let return e : t = Set.singleton e

  let bind sa f : t =
    Set.fold
      (fun e acc ->
        let sb = f e in
        Set.union acc sb)
      sa Set.empty
end

module MS = struct
  include Nondeter_in_set (Int_set)
end

module MS_example_1 = struct
  let add_deltas ds e =
    List.fold_left (fun s d -> Int_set.add (d + e) s) Int_set.empty ds

  let s0 = Int_set.of_list [ 1; 2; 3 ]
  let s1 = MS.bind s0 (add_deltas [ 1; 2 ])
  let s2 = MS.bind s1 (add_deltas [ 1; 2 ])
  let rec loop n f s = if n = 0 then s else loop (n - 1) f (f s)
  let s3 = loop 3 (fun s -> MS.bind s (add_deltas [ 1; 2 ])) s0

  let invariant_length n =
    Int_set.cardinal @@ loop n (fun s -> MS.bind s (add_deltas [ 1; 2 ])) s0
    = n + 3
end

let%expect_test _ =
  Fmt.pr "%a" (Fmt.Dump.seq Fmt.int) (Int_set.to_seq MS_example_1.s2) ;
  [%expect {|
    [3; 4; 5; 6; 7]
    
    |}] ;
  Fmt.pr "%a" (Fmt.Dump.seq Fmt.int) (Int_set.to_seq MS_example_1.s3) ;
  [%expect {| [4; 5; 6; 7; 8; 9] |}]

let%test _ = MS_example_1.invariant_length 2
let%test _ = MS_example_1.invariant_length 20

module MSS = struct
  module NS = Nondeter_in_set (Set_set)
  include NS

  let dump s = Fmt.pr "%a" pp_set_set s
end

module MSS_example = struct
  open MSS

  let s0 =
    Set_set.of_list
      [ Int_set.singleton 1; Int_set.singleton 2; Int_set.singleton 3 ]

  let add_d delta s = Int_set.map (fun e -> e + delta) s

  let mutation deltas s =
    Int_set.fold
      (fun e s -> List.fold_left (fun s i -> Int_set.add (e + i) s) s deltas)
      s s

  let inc_fork s =
    Set_set.(add (mutation [ 1; 2; 3 ] s) empty |> add (mutation [ 4; 5; 6 ] s))

  let s1 = loop 1 (fun s -> MSS.bind s inc_fork) s0
  let s2 = loop 2 (fun s -> MSS.bind s inc_fork) s0
  let s3 = loop 3 (fun s -> MSS.bind s inc_fork) s0

  let invariant_length n =
    Set_set.cardinal @@ loop n (fun s -> MSS.bind s inc_fork) s0 < 3 * pow2 n
end

let%expect_test _ =
  MSS.dump MSS_example.s1 ;
  [%expect
    {|
    [[1; 2; 3; 4]; [1; 5; 6; 7]; [2; 3; 4; 5]; [2; 6; 7; 8]; [3; 4; 5; 6];
     [3; 7; 8; 9]] |}] ;

  MSS.dump MSS_example.s2 ;
  [%expect
    {|
       [[1; 2; 3; 4; 5; 6; 7]; [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
        [1; 5; 6; 7; 9; 10; 11; 12; 13]; [2; 3; 4; 5; 6; 7; 8];
        [2; 3; 4; 5; 6; 7; 8; 9; 10; 11]; [2; 6; 7; 8; 10; 11; 12; 13; 14];
        [3; 4; 5; 6; 7; 8; 9]; [3; 4; 5; 6; 7; 8; 9; 10; 11; 12];
        [3; 7; 8; 9; 11; 12; 13; 14; 15]] |}] ;

  MSS.dump MSS_example.s3 ;
  [%expect
    {|
          [[1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
           [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
           [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
           [1; 5; 6; 7; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19];
           [2; 3; 4; 5; 6; 7; 8; 9; 10; 11];
           [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14];
           [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17];
           [2; 6; 7; 8; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];
           [3; 4; 5; 6; 7; 8; 9; 10; 11; 12];
           [3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15];
           [3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18];
           [3; 7; 8; 9; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21]] |}]

let%test _ = MSS_example.invariant_length 15
(* bench *)
let%bench "nondeter in list" = ignore @@ ML_example.invariant_length 15
let%bench "nondeter in set" = ignore @@ MSS_example.invariant_length 15
