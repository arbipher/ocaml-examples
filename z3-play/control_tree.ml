open Core

type 'a t =
  (* Leaf *)
  | Done of 'a * bool ref
  | Pending of 'a * bool ref
  (* Branch *)
  | Both of 'a * bool ref * 'a t * 'a t
  | Either of 'a * bool ref * 'a t * 'a t
[@@deriving show {with_path = false}]

(* 
  done_state = false, gate = false 
  done_state = true,  gate = ?


   *)

(* 
x = b ? {y=1}; {z=2}

 *)

type payload_constraint =
  | Payload of int
  | BothGate of int * int * int
  | EitherGate of int * int * int

  | GateState of int * bool
[@@deriving show {with_path = false}]

let rec collect_states = function
  | Done (a, s) ->   [GateState(a, !s)]
  | Pending (a, s) -> [GateState(a, !s)]
  | Both (a, s, ll, rr) ->   [GateState(a, !s)] @ collect_states ll @ collect_states rr
  | Either (a, s, ll, rr) -> [GateState(a, !s)] @ collect_states ll @ collect_states rr

let get_id = function
  | Done (a, _) -> a
  | Pending (a, _) -> a
  | Both (a, _, _, _) -> a
  | Either (a, _, _, _) -> a

let get_done_state = function
  | Done (_, s) -> !s
  | Pending (_, s) -> !s 
  | Both (_, s, _, _) -> !s
  | Either (_, s, _, _) -> !s

let make_phis t = 
  let phis = ref [] in
  let add_phi phi = 
    phis := phi :: !phis
  in
  let rec gen_phis t = 
    match t with
    | Done (p, s) -> 
      add_phi (Payload p);
      s := true;
      !s
    | Pending (p, s) ->
      add_phi (Payload p);
      s := false;
      !s
    | Both (p, s, left, right) -> (
        add_phi (Payload p);
        add_phi (BothGate (get_id left, get_id right, p));
        let r1 = gen_phis left in
        let r2 = gen_phis right in
        s := r1 && r2;
        !s
      )
    | Either (p, s, left, right) -> (
        add_phi (Payload p);
        add_phi (EitherGate (get_id left, get_id right, p));
        let r1 = gen_phis left in
        let r2 = gen_phis right in
        s := r1 || r2;
        !s
      )
  in

  ignore @@ gen_phis t;
  (* always true, incremental *)
  let payload = !phis in
  (* stateful, ephemeral *)
  let controls = collect_states t in
  payload, controls

(* 
  | Payload of int
  | BothGate of int * int * int
  | EitherGate of int * int * int
  | GateState of int * bool
   *)

let smt_constraints_of_constrait phi = 
  let open Assuming in
  let open Z3 in

  let gv i = bvar @@ "g_" ^ (string_of_int i) in
  let pv i = bvar @@ "p_" ^ (string_of_int i) in

  match phi with
  | Payload i -> 
    [Boolean.mk_implies ctx (gv i) (pv i)]
  | BothGate(ll, rr, tt) -> 
    [Boolean.mk_implies ctx 
       (Boolean.mk_and ctx [(gv ll);(gv rr)])
       (gv tt)]
  | EitherGate(ll,rr,tt) ->
    let pre = (Boolean.mk_xor ctx (gv ll) (gv rr)) in
    [Boolean.mk_implies ctx 
       pre
       (gv tt)]
  | GateState (i, s)->
    (* done_state || (not gate) *)
    [Boolean.mk_or ctx 
       [(Boolean.mk_val ctx s); (Boolean.mk_not ctx (gv i))]
    ]

let solve t = 
  let open Assuming in
  let open Z3 in
  Solver.reset solver;

  let payload, controls = make_phis t in
  let payload_phis = List.map payload ~f:smt_constraints_of_constrait |> List.concat in
  Solver.add solver payload_phis;
  let state_phis = List.map controls ~f:smt_constraints_of_constrait |> List.concat in
  check state_phis

let t1 = 
  Done (1, ref false)

let t2 = 
  Pending (1, ref false)

let t3 = 
  Both (1, ref false,
        Done (2, ref false),
        Done (3, ref false)
       )

let t4 = 
  Either (1, ref false,
          Done (2, ref false),
          Done (3, ref false)
         )

let t5 = 
  Both (1, ref false,
        Either (2, ref false,
                Both (3, ref false,
                      Done (4, ref false),
                      Either (5, ref false,
                              Done (6, ref false),
                              Done (7, ref false)
                             )
                     ),
                Done (8, ref false)
               ),
        Done (9, ref false)
       )

let t6 = 
  Both (1, ref false,
        Either (2, ref false,
                Both (3, ref false,
                      Done (4, ref false),
                      Either (5, ref false,
                              Done (6, ref false),
                              Pending (7, ref false)
                             )
                     ),
                Done (8, ref false)
               ),
        Done (9, ref false)
       )

let test_tree t = 
  let payload, controls = make_phis t in
  Fmt.(pr "%a" (Fmt.Dump.list pp_payload_constraint) (payload @ controls))

let%expect_test _ =
  test_tree t1;
  [%expect{| [(Payload 1); (GateState (1, true))] |}];

  test_tree t2;
  [%expect{| [(Payload 1); (GateState (1, false))] |}];

  test_tree t3;
  [%expect{|
    [(Payload 3); (Payload 2); (BothGate (2, 3, 1)); (Payload 1);
     (GateState (1, true)); (GateState (2, true)); (GateState (3, true))] |}];

  test_tree t4;
  [%expect{|
    [(Payload 3); (Payload 2); (EitherGate (2, 3, 1)); (Payload 1);
     (GateState (1, true)); (GateState (2, true)); (GateState (3, true))] |}];

  test_tree t5;
  [%expect{|
    [(Payload 9); (Payload 8); (Payload 7); (Payload 6); (EitherGate (6, 7, 5));
     (Payload 5); (Payload 4); (BothGate (4, 5, 3)); (Payload 3);
     (EitherGate (3, 8, 2)); (Payload 2); (BothGate (2, 9, 1)); (Payload 1);
     (GateState (1, true)); (GateState (2, true)); (GateState (3, true));
     (GateState (4, true)); (GateState (5, true)); (GateState (6, true));
     (GateState (7, true)); (GateState (8, true)); (GateState (9, true))] |}];
  ()

let%expect_test _ =
  solve t1;
  [%expect{|
    (declare-fun p_1 () Bool)
    (declare-fun g_1 () Bool)
    (assert (=> g_1 p_1))

    SAT
    (define-fun g_1 () Bool
      false) |}];

  solve t2;
  [%expect{|
    (declare-fun p_1 () Bool)
    (declare-fun g_1 () Bool)
    (assert (=> g_1 p_1))

    SAT
    (define-fun g_1 () Bool
      false) |}];

  solve t3;
  [%expect{|
    (declare-fun p_3 () Bool)
    (declare-fun g_3 () Bool)
    (declare-fun p_2 () Bool)
    (declare-fun g_2 () Bool)
    (declare-fun g_1 () Bool)
    (declare-fun p_1 () Bool)
    (assert (=> g_3 p_3))
    (assert (=> g_2 p_2))
    (assert (=> (and g_2 g_3) g_1))
    (assert (=> g_1 p_1))

    SAT
    (define-fun g_3 () Bool
      false)
    (define-fun g_2 () Bool
      false)
    (define-fun g_1 () Bool
      false) |}];

  solve t4;
  [%expect{|
    (declare-fun p_3 () Bool)
    (declare-fun g_3 () Bool)
    (declare-fun p_2 () Bool)
    (declare-fun g_2 () Bool)
    (declare-fun g_1 () Bool)
    (declare-fun p_1 () Bool)
    (assert (=> g_3 p_3))
    (assert (=> g_2 p_2))
    (assert (=> (xor g_2 g_3) g_1))
    (assert (=> g_1 p_1))

    SAT
    (define-fun g_3 () Bool
      false)
    (define-fun g_2 () Bool
      false)
    (define-fun g_1 () Bool
      false) |}];

  solve t5;
  [%expect{|
    (declare-fun p_9 () Bool)
    (declare-fun g_9 () Bool)
    (declare-fun p_8 () Bool)
    (declare-fun g_8 () Bool)
    (declare-fun p_7 () Bool)
    (declare-fun g_7 () Bool)
    (declare-fun p_6 () Bool)
    (declare-fun g_6 () Bool)
    (declare-fun g_5 () Bool)
    (declare-fun p_5 () Bool)
    (declare-fun p_4 () Bool)
    (declare-fun g_4 () Bool)
    (declare-fun g_3 () Bool)
    (declare-fun p_3 () Bool)
    (declare-fun g_2 () Bool)
    (declare-fun p_2 () Bool)
    (declare-fun g_1 () Bool)
    (declare-fun p_1 () Bool)
    (assert (=> g_9 p_9))
    (assert (=> g_8 p_8))
    (assert (=> g_7 p_7))
    (assert (=> g_6 p_6))
    (assert (=> (xor g_6 g_7) g_5))
    (assert (=> g_5 p_5))
    (assert (=> g_4 p_4))
    (assert (=> (and g_4 g_5) g_3))
    (assert (=> g_3 p_3))
    (assert (=> (xor g_3 g_8) g_2))
    (assert (=> g_2 p_2))
    (assert (=> (and g_2 g_9) g_1))
    (assert (=> g_1 p_1))

    SAT
    (define-fun g_1 () Bool
      false)
    (define-fun g_8 () Bool
      false)
    (define-fun g_9 () Bool
      false)
    (define-fun g_6 () Bool
      false)
    (define-fun g_5 () Bool
      false)
    (define-fun g_7 () Bool
      false)
    (define-fun g_4 () Bool
      false)
    (define-fun g_3 () Bool
      false)
    (define-fun g_2 () Bool
      false) |}];

  ();
  [%expect{| |}];

  ()