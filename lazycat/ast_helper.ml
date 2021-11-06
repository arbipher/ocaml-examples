open Base
open Ast
open Lazi

type callsite = Int.t 
[@@deriving sexp, compare, equal]

module Id = struct
  module T = struct
    type t = ident
    [@@deriving sexp, compare, equal]
  end
  include T
  include Comparator.Make(T)
end

(* map of id -> expr *)
module Env = struct
  module T = struct
    type t = expr Map.M(Id).t
    [@@deriving sexp_of, compare]
  end
  include T
  include Comparator.Make(T)
end

type env_set = Set.M(Env).t

(* list of int
   context and abstract context both use this type
*)
module Context = struct
  module T = struct
    type t = callsite list
    [@@deriving sexp, compare, equal]
  end
  include T
  include Comparator.Make(T)
end

module Store = struct
  module T = struct
    type t = Env.t Map.M(Context).t
    [@@deriving sexp_of, compare]
  end
  include T
  include Comparator.Make(T)
end

(* module Cache_key = struct
   module T = struct
    type t = expr * Env.t * Context.t * Store.t
    [@@deriving sexp_of, compare]
   end
   include T
   include Comparator.Make(T)
   end
   type cache = expr Map.M(Cache_key).t *)
(* 

let empty_env = Map.empty (module Id)

type context 

type env_store = Set.M(context).t
 *)


let rec endomap_expr e alg =
  match e with
  | Var _ | Int _ | Bool _ | Cell _ as self -> alg self
  | Plus (e1, e2) -> Plus (endomap_expr e1 alg, endomap_expr e2 alg)
  | Minus (e1, e2) -> Minus (endomap_expr e1 alg, endomap_expr e2 alg)
  | Equal (e1, e2) -> Equal (endomap_expr e1 alg, endomap_expr e2 alg)
  | And (e1, e2) -> And (endomap_expr e1 alg, endomap_expr e2 alg)
  | Or (e1, e2) -> Or (endomap_expr e1 alg, endomap_expr e2 alg)
  | Not e -> Not (endomap_expr e alg)
  | If (e1, e2, e3) -> If (endomap_expr e1 alg, endomap_expr e2 alg, endomap_expr e3 alg)
  | Function (id, e) -> Function (id, endomap_expr e alg)
  | Appl (e1, e2) -> Appl (endomap_expr e1 alg, endomap_expr e2 alg)
  | Let (id, e1, e2) -> Let (id, endomap_expr e1 alg, endomap_expr e2 alg)
  | LetRec (id1, id2, e1, e2) -> LetRec (id1, id2, endomap_expr e1 alg, endomap_expr e2 alg)
  | Record fields -> Record (map_fields_value fields alg)
  | Select (lab, e) -> Select (lab, endomap_expr e alg)
  | Ref e -> Ref (endomap_expr e alg)
  | Set (e1, e2) -> Set (endomap_expr e1 alg, endomap_expr e2 alg)
  | Get e -> Get (endomap_expr e alg)
