open Base
open Ast
open Ast_helper
open Abstract_ast
open Lazi

module Config = struct
  let k = ref 1
end

let bound_push_ctx ctx cs =
  let ctx2 = cs :: ctx in
  if List.length ctx2 <= !Config.k
  then
    ctx2
  else
    List.drop_last_exn ctx2

module AExpr = struct
  module T = struct
    type t = aexpr
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
end

module AEnv = struct
  module T = struct
    type t = aexpr Map.M(Id).t
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
end

module AStore = struct
  module T = struct
    type t = (AEnv.t list) Map.M(Context).t
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
end

let empty_store = Map.empty (module Context)

(* 
    let safe_add key elm store =
      let add_or_create = function
        | Some env_set -> Some (Env_set.add elm env_set)
        | None -> Some (Env_set.singleton elm) in
      update key add_or_create store *)

let safe_add_env store ctx env =
  let add_or_create = function
    | Some s -> Set.add s env
    | None -> Set.singleton (module AEnv) env
  in
  Map.update store ctx ~f:add_or_create

module ResultPair = struct
  module T = struct
    type t = aexpr * AStore.t
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
end

(* module AResults = struct
   module T = struct
    type t = Set.M(ResultPair).t
    [@@deriving sexp, compare]
   end
   include T
   include Comparator.Make(T)
   end *)

type aresult = Set.M(ResultPair).t

let empty_result = Set.empty (module ResultPair)

module Cache_key = struct
  module T = struct
    type t = aexpr * AEnv.t * Context.t * AStore.t
    [@@deriving sexp, compare]
  end
  include T
  include Comparator.Make(T)
end

type cache = aexpr Map.M(Cache_key).t

let aint_of_int i =
  if i > 0
  then Pos
  else if i = 0
  then Zero
  else Neg

let abool_of_bool b = b

let generate_counter = Lazi.make_counter ()

let rec of_expr (e: expr) : aexpr =
  match e with
  | Var v -> AVar v
  | Int i -> AInt (aint_of_int i)
  | Bool b -> ABool (abool_of_bool b)
  | Plus (e1, e2) -> APlus (of_expr e1, of_expr e2)
  | Minus (e1, e2) -> AMinus (of_expr e1, of_expr e2)
  | Equal (e1, e2) -> AEqual (of_expr e1, of_expr e2)
  | And (e1, e2) -> AAnd (of_expr e1, of_expr e2)
  | Or (e1, e2) -> AOr (of_expr e1, of_expr e2)
  | Not e -> ANot (of_expr e)
  | If (e1, e2, e3) -> AIf (of_expr e1, of_expr e2, of_expr e3)
  | Function (id, e) -> AFunction (id, of_expr e)
  | Appl (e1, e2) -> AAppl (generate_counter (), of_expr e1, of_expr e2)
  | Let (id, e1, e2) -> ALet (generate_counter (), id, of_expr e1, of_expr e2)
  | LetRec (id1, id2, e1, e2) -> ALetRec (generate_counter (), id1, id2, of_expr e1, of_expr e2)
  | Record fields -> ARecord (map_fields_value fields of_expr)
  | Select (lab, e) -> ASelect (lab, of_expr e)
  | _ -> failwith "not ready for states"

let rec endomap_aexpr e alg =
  match e with
  | AVar _ | AInt _ | ABool _ as self -> alg self
  | APlus (e1, e2) -> APlus (endomap_aexpr e1 alg, endomap_aexpr e2 alg)
  | AMinus (e1, e2) -> AMinus (endomap_aexpr e1 alg, endomap_aexpr e2 alg)
  | AEqual (e1, e2) -> AEqual (endomap_aexpr e1 alg, endomap_aexpr e2 alg)
  | AAnd (e1, e2) -> AAnd (endomap_aexpr e1 alg, endomap_aexpr e2 alg)
  | AOr (e1, e2) -> AOr (endomap_aexpr e1 alg, endomap_aexpr e2 alg)
  | ANot e -> ANot (endomap_aexpr e alg)
  | AIf (e1, e2, e3) -> AIf (endomap_aexpr e1 alg, endomap_aexpr e2 alg, endomap_aexpr e3 alg)
  | AFunction (id, e) -> AFunction (id, endomap_aexpr e alg)
  | AClosure (e, ctx) -> AClosure (endomap_aexpr e alg, ctx)
  | AAppl (cs, e1, e2) -> AAppl (cs, endomap_aexpr e1 alg, endomap_aexpr e2 alg)
  | ALet (cs, id, e1, e2) -> ALet (cs, id, endomap_aexpr e1 alg, endomap_aexpr e2 alg)
  | ALetRec (cs, id1, id2, e1, e2) -> ALetRec (cs, id1, id2, endomap_aexpr e1 alg, endomap_aexpr e2 alg)
  | ARecord fields -> ARecord (map_fields_value fields alg)
  | ASelect (lab, e) -> ASelect (lab, endomap_aexpr e alg)
