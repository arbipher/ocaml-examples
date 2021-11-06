open Core

module Type = struct
  module T = struct
    type t = 
      | Int 
      | Bool
      | Any
      | Var of Ast.Var.t
      | Arrow of t * t
    [@@deriving sexp, compare, variants]
  end
  include T
  include Comparable.Make(T)

  let id s = Var (Ast.Var.Var s)

  let is_compatible _t1 _t2 =
    true
    (* match t1, t2 with
       | Var _  , Var _
       | Var _  , Type _
       | Type _ , Var _ ->
       true
       | Type t1, Type t2 ->
       Type.equal t1 t2 *)
end

module TypeEq = struct
  module T = struct
    type t = Type.t * Type.t 
    [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)
end

type typeeq_set =
  Set.M(TypeEq).t
[@@deriving sexp]

type id_types_map =
  (Type.t list) Map.M(Ast.Var).t
[@@deriving sexp]

module Typing = struct
  exception TypeInequal of TypeEq.t
  [@@deriving sexp]

  let check_add set (l, r) =
    Set.(add set (l, r))
  (* if TypeEq.is_compatible l r then
     Set.(add set (l, r))
     else
     raise (TypeInequal (l, r)) *)

  let rec closure eq_set =
    (* structural *)
    let set = 
      Set.fold eq_set
        ~init:eq_set
        ~f:(fun acc (l, r) -> 
            match l, r with
            | Type.Arrow (l1, l2), Type.Arrow (r1, r2) ->
              Set.(add (add acc (l1, r1)) (l2, r2))
            | _ -> acc)
    in
    (* symmetric and structural *)
    let set' = 
      Set.fold set
        ~init:set
        ~f:(fun acc (l, r) -> 
            Set.add acc (r, l)) in
    (* transitive *)
    let set'' =
      Set.fold set'
        ~init:set'
        ~f:(fun acc1 (l1, l2) -> 
            Set.fold set'
              ~init:acc1
              ~f:(fun acc2 (r1, r2) ->
                  if Type.equal l2 r1 then
                    Set.add acc2 (l1, r2)
                  else
                    acc2)) in    
    if Set.equal eq_set set'' then
      set''
    else
      closure set''

  let gen_eq e : typeeq_set =
    let open Ast.Ast in
    let rec loop_expr eq_set (Expr cs) =
      List.fold_left cs
        ~init:eq_set
        ~f:(fun acc c -> loop_clause acc c)
    and loop_clause set clause =
      match clause.body with
      | Value (Int _) ->  Core.Set.add set (Type.Var clause.x, Type.Int)
      | Value (Bool _) -> Core.Set.add set (Type.Var clause.x, Type.Bool)
      | Value (Fun fun_def) -> (
          let (Expr fc) = fun_def.fbody in
          let set' = Core.Set.add set (Type.Var clause.x, Type.Arrow (
              Type.Var fun_def.para, 
              Type.Var (List.last_exn fc).x)) in
          loop_expr set' fun_def.fbody
        )
      | Var y ->          Core.Set.add set (Type.Var clause.x, Type.Var y)
      | Int_input ->      Core.Set.add set (Type.Var clause.x, Type.Int)
      | Appl (x1, x2) ->  Core.Set.add set (Type.Var x1, Type.Arrow (Type.Var x2, Type.Var clause.x) )
      | Cond (xc, (Expr cs1 as e1), (Expr cs2 as e2)) -> 
        let set' = Core.Set.(add (add (add set 
                                         (Type.Var xc, Type.Bool))
                                    (Type.Var clause.x, Type.Var (List.last_exn cs1).x))
                               (Type.Var clause.x, Type.Var (List.last_exn cs2).x)) in
        loop_expr (loop_expr set' e1) e2
      | Plus (x1, x2) -> Core.Set.(
          add (add (add set 
                      (Type.Var x1, Type.Int))
                 (Type.Var x2, Type.Int))
            (Type.Var clause.x, Type.Int))
      | Not x -> Core.Set.(add (add set 
                                  (Type.Var clause.x, Type.Bool))
                             (Type.Var x, Type.Bool))
    in
    loop_expr (Core.Set.empty (module TypeEq)) e

  let gen_id_types_map set =
    Set.fold set
      ~init:(Map.empty (module Ast.Var))
      ~f: (fun acc eq ->
          Option.value (
            let open Option.Let_syntax in
            let%bind (key, data) = 
              match eq with
              | Type.Var _, Type.Var _ -> None
              | Type.Var x, v -> Some (x, v)
              | v, Type.Var x -> Some (x, v)
              | _, _ -> None in
            let acc' = match Map.find_multi acc key with
              | [] -> Map.add_multi acc ~key ~data
              | vs -> if List.mem vs data ~equal:Type.equal then
                  acc
                else
                  Map.add_multi acc ~key ~data
            in
            Option.some acc' 
          ) ~default:acc
        ) 

  (* let gen_id_type_map id_types_map =
     Map.map id_types_map
      ~f: (fun v -> 
        if List.length > 1 then
          Type.Any 
      List.hd_exn v)   *)

  let typecheck e =
    let eq_set = gen_eq e in
    try
      let set = closure eq_set in
      let id_types_map = gen_id_types_map set in
      Ok id_types_map
    with TypeInequal (t1, t2) ->
      Error (TypeInequal (t1, t2))

  let to_string = function
    | Ok set ->
      set
      |> sexp_of_typeeq_set
      |> Sexp.to_string_hum
    | Error err ->
      err
      |> Exn.sexp_of_t
      |> Sexp.to_string_hum


  let map_to_string = function
    | Ok set ->
      set
      |> sexp_of_id_types_map
      |> Sexp.to_string_hum
    | Error err ->
      err
      |> Exn.sexp_of_t
      |> Sexp.to_string_hum
end

module Static_typing = struct
end

let p1 = (Type.id "x"), (Type.Int)
let p2 = (Type.id "x"), (Type.Bool)

let s1 = Set.of_list (module TypeEq)
    [p1]

let s2 = Set.of_list (module TypeEq)
    [p1; p2]

let%expect_test _ =
  s1
  |> Typing.closure
  |> sexp_of_typeeq_set
  |> Sexp.to_string
  |> print_endline;
  [%expect{| ((Int Int)(Int(Var(Var x)))((Var(Var x))Int)((Var(Var x))(Var(Var x)))) |}]

module Examples = struct
  open Ast.Ast

  let e1 = Expr [
      leti "x" 1;
    ]

  let e2 = Expr [
      leti "x" 1;
      leti "y" 2;
    ]

  let e3 = Expr [
      leti "u" 1;
      leti "y" 2;
      letp "z" "u" "y";
    ]

  let e4 = Expr [
      letf "f" "fx" [
        leti "fr" 1;  
      ];
      leti "t" 1;
      letv "z" @@ app "f" "t";
    ]

  let e5 = Expr [
      letf "f" "fx" [
        leti "fr" 1;  
      ];
      leti "t" 1;
      letv "z" @@ app "f" "t";
      letb "s" true;
      letv "w" @@ app "f" "s";
    ]
end
open Examples
;;

let pg e =
  e
  |> Typing.gen_eq
  |> [%sexp_of: Set.M(TypeEq).t]
  |> Sexp.to_string_hum
  |> print_endline

let%expect_test _ =
  pg e1;
  [%expect{| (((Var (Var x)) Int)) |}];
  pg e2;
  [%expect{| (((Var (Var x)) Int) ((Var (Var y)) Int)) |}];
  pg e3;
  [%expect{| (((Var (Var u)) Int) ((Var (Var y)) Int) ((Var (Var z)) Int)) |}];
  pg e4;
  [%expect{|
    (((Var (Var f)) (Arrow (Var (Var fx)) (Var (Var fr))))
     ((Var (Var f)) (Arrow (Var (Var t)) (Var (Var z)))) ((Var (Var fr)) Int)
     ((Var (Var t)) Int)) |}];
  pg e5;
  [%expect{|
    (((Var (Var f)) (Arrow (Var (Var fx)) (Var (Var fr))))
     ((Var (Var f)) (Arrow (Var (Var s)) (Var (Var w))))
     ((Var (Var f)) (Arrow (Var (Var t)) (Var (Var z)))) ((Var (Var fr)) Int)
     ((Var (Var s)) Bool) ((Var (Var t)) Int)) |}];
  ()
(* let pt e =
   e
   |> Typing.typecheck
   |> Typing.map_to_string
   |> print_endline

   let%expect_test _ =
   pt e1;
   [%expect{| (((Var x) (Int))) |}]

   let%expect_test _ =
   pt e2;
   [%expect{| (((Var x) (Int)) ((Var y) (Int))) |}]

   let%expect_test _ =
   pt e3;
   [%expect{| (((Var u) (Int)) ((Var y) (Int)) ((Var z) (Int))) |}]

   let%expect_test _ =
   pt e4;
   [%expect{|
    (((Var f)
      ((Arrow (Var (Var t)) (Var (Var z))) (Arrow (Var (Var fx)) (Var (Var fr)))))
     ((Var fr) (Int)) ((Var fx) (Int)) ((Var t) (Int)) ((Var z) (Int))) |}]

   let%expect_test _ =
   pt e5;
   [%expect{|
    (((Var f)
      ((Arrow (Var (Var t)) (Var (Var z))) (Arrow (Var (Var s)) (Var (Var w)))
       (Arrow (Var (Var fx)) (Var (Var fr)))))
     ((Var fr) (Bool Int)) ((Var fx) (Bool Int)) ((Var s) (Bool Int))
     ((Var t) (Bool Int)) ((Var w) (Bool Int)) ((Var z) (Bool Int))) |}] *)

