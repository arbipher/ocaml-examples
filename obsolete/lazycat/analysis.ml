open Base
open Ast_helper
open Abstract_ast
open Abstract_ast_helper

module Op = struct
  let is_int = function
    | AInt _ -> true
    | _ -> false

  let plus v1 v2 =
    match v1, v2 with
    | AInt Pos, AInt Pos
    | AInt Zero, AInt Pos
    | AInt Pos, AInt Zero -> Set.singleton (module AExpr) (AInt Pos)
    | AInt Zero, AInt Zero -> Set.singleton (module AExpr) (AInt Zero)
    | AInt Zero, AInt Neg
    | AInt Neg, AInt Zero
    | AInt Neg, AInt Neg -> Set.singleton (module AExpr) (AInt Neg)
    | AInt Pos, AInt Neg
    | AInt Neg, AInt Pos -> Set.of_list (module AExpr) [AInt Pos; AInt Zero; AInt Neg]
    | _ -> Set.empty (module AExpr)

  let minus v1 v2 =
    if not @@ is_int v2
    then Set.empty (module AExpr)
    else
      let neg_v2 = match v2 with
        | AInt Pos -> AInt Neg
        | AInt Zero -> AInt Zero
        | AInt Neg -> AInt Pos
        | _ -> failwith "unreachable"
      in plus v1 neg_v2

  let equal v1 v2 = 
    match v1, v2 with
    | AInt Zero, AInt Zero -> Set.singleton (module AExpr) (ABool true)
    | AInt a1, AInt a2 when (equal_aint a1 a2) -> Set.of_list (module AExpr) [ABool true; ABool false]
    | AInt _, AInt _ -> Set.singleton (module AExpr) (ABool false)
    | ABool b1, ABool b2 -> Set.singleton (module AExpr) (ABool (Bool.equal b1 b2))
    | _ -> Set.empty (module AExpr)

  let and_ v1 v2 = 
    match v1, v2 with
    | ABool b1, ABool b2 -> Set.singleton (module AExpr) (ABool (b1 && b2))
    | _ -> Set.empty (module AExpr)

  let or_ v1 v2 = 
    match v1, v2 with
    | ABool b1, ABool b2 -> Set.singleton (module AExpr) (ABool (b1 || b2))
    | _ -> Set.empty (module AExpr)

  let not_ = function 
    | ABool b -> Set.singleton (module AExpr) (ABool (not b))
    | _ -> Set.empty (module AExpr)
end

module AnalysisEval = struct
  module M = Monad.Make(Analysis_monad)
  open Analysis_monad
  open M
  (* open Analysis_monad.Monad_infix *)
  (* open M.Let_syntax *)

  type mon = aexpr Analysis_monad.t 

  let (let*) m f = M.bind m ~f

  let flat r : mon =
    fun store ->
    List.map ~f:(fun e -> Done (e, store)) (Set.elements r)

  let zero : mon =
    fun _store -> []

  let step x : mon =
    fun _store -> [Step x]

  let rec run_steps n (m : mon) : mon =
    if n <= 0 then
      m
    else
      fun store ->
        let tasks = m store in
        let dones, steps = List.partition_tf tasks ~f:(function
            | Done (_a, _s) -> true
            | Step _b -> false
          ) in
        Stdio.printf "n=%d\td=%d\ts=%d\n" n (List.length dones) (List.length steps);
        if List.length steps = 0 then
          dones
        else
          begin
            let step_pick = List.hd_exn steps
            and step_rest = match List.tl steps with
              | Some ts -> ts
              | None -> []
            in
            let step_ran = 
              match step_pick with
              | Done (_a, _s) -> failwith "run_step: Done"
              | Step s -> s store 
            in
            dones @ (run_steps (n-1) (fun _store -> step_rest @ step_ran)) store
          end

  let rec eval e env ctx : mon =
    let bin_op e1 e2 op =
      let* v1 = eval e1 env ctx in
      let* v2 = eval e2 env ctx in
      let result = op v1 v2 in
      flat result in
    let bin_map e1 e2 f =
      let* v1 = eval e1 env ctx in
      let* v2 = eval e2 env ctx in
      f v1 v2 in
    let unary_map e f =
      let* v = eval e env ctx in
      flat (f v) in
    let open M.Let_syntax in
    let open Op in
    (* e |> AExpr.sexp_of_t |> Sexp.to_string_hum |> Lazi.pe; *)
    (* env |> AEnv.sexp_of_t |> Sexp.to_string_hum |> Lazi.pe; *)
    match e with
    | AInt _ | ABool _ | AClosure _ as self
      -> return self
    | APlus (e1, e2) -> bin_op e1 e2 plus
    | AMinus (e1, e2) -> bin_op e1 e2 minus
    | AEqual (e1, e2) -> bin_op e1 e2 equal
    | AAnd (e1, e2) -> bin_op e1 e2 and_
    | AOr (e1, e2) -> bin_op e1 e2 or_
    | ANot e -> unary_map e not_
    | AIf (e1, e2, e3) -> (
        let* v1 = eval e1 env ctx in
        match v1 with
        | ABool true -> eval e2 env ctx
        | ABool false -> eval e3 env ctx
        | _ -> zero
      )
    | AVar v -> (
        match Map.find env v with
        | Some v -> return v
        | None -> zero
      )
    | AFunction _ as f -> (
        fun store ->
          let store1 = Map.add_multi store ~key:ctx ~data:env in
          [Done (AClosure (f, ctx), store1)]  
      )
    | AAppl (cs, e1, e2) -> (
        let appl v1 v2 = 
          match v1 with
          | AClosure (AFunction (id, e), saved_ctx) -> (
              Lazi.pf "\n-1-\n";
              e |> AExpr.sexp_of_t |> Sexp.to_string_hum |> Lazi.pe;

              let m = fun store -> (
                  Lazi.pf "\n-2-\n";
                  let saved_env_list = Map.find_multi store saved_ctx in
                  let ctx2 = bound_push_ctx ctx cs in
                  Lazi.pf "\n-3 %d -\n" (List.length saved_env_list);
                  List.concat_map saved_env_list ~f:(fun saved_env ->
                      (* saved_env |> AEnv.sexp_of_t |> Sexp.to_string_hum |> Lazi.pe; *)
                      let env2 = Map.update saved_env id ~f:(fun _ -> v2) in
                      (* env2 |> AEnv.sexp_of_t |> Sexp.to_string_hum |> Lazi.pe; *)
                      Lazi.pf "\n-4-\n";
                      step (eval e env2 ctx2) store
                    )
                ) 
              in
              m
              (* step m *)
            )
          | _ -> zero
        in
        bin_map e1 e2 appl
      )
    | ALet (cs, id, e1, e2) -> (
        let* v1 = eval e1 env ctx in
        let ctx2 = bound_push_ctx ctx cs in
        let env2 = Map.update env id ~f:(fun _ -> v1) in
        eval e2 env2 ctx2
      )
    | ALetRec (cs, id1, id2, e1, e2) -> (
        let v1 = AClosure(AFunction (id2, e1), ctx) in
        let env2 = Map.update env id1 ~f:(fun _ -> v1) in
        let m = fun store -> 
          let store2 = Map.add_multi store ~key:ctx ~data:env2 in
          let ctx2 = bound_push_ctx ctx cs in
          eval e2 env2 ctx2 store2 
        in
        step m
      )
    | ARecord fields -> (
        let keys = Lazi.keys_of_assoc fields
        and exps = Lazi.values_of_assoc fields in
        let runner e =
          fun store ->
            eval e env ctx store
        in
        let* vals = all (List.map exps ~f:runner) in
        let fields2 = List.zip_exn keys vals in
        return @@ ARecord fields2
        (* fun store ->
           List.hd_exn (vals store) *)
      )
    | ASelect (lab, e) -> (
        let* v1 = eval e env ctx in
        match v1 with
        | ARecord fields ->
          return @@ List.Assoc.find_exn fields ~equal:Ast.equal_label lab
        | _ ->
          zero
      )

  let filter_done result =
    List.filter_map result ~f:(function
        | Done (e, s) -> Some (e, s)
        | _ -> None)
end

let eval e = 
  let empty_env = Map.empty (module Id) in
  let work = AnalysisEval.eval e empty_env [] in
  let result = work empty_store |> AnalysisEval.filter_done
  in
  List.map result ~f:fst

let run_steps step e =
  let empty_env = Map.empty (module Id) in
  let work = AnalysisEval.eval e empty_env [] in
  let result = AnalysisEval.run_steps step work empty_store in
  let dones = result |> AnalysisEval.filter_done in
  List.map dones ~f:fst
