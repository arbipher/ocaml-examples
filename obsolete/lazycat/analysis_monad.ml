open Base
open Abstract_ast_helper

(* 'a itself is acturally aexpr *)
type 'a task = 
  | Done of 'a * AStore.t
  | Step of 'a t
and 'a t = AStore.t -> ('a task) list

(* let bind (m : 'a t) ~f:(f : 'a -> 'b t) : 'b t = *)
let rec bind m ~f =
  fun store0 ->
  let sr = m store0 in
  List.concat_map sr ~f:(fun t ->
      match t with
      | Done (ar, store1) -> (f ar) store1
      | Step step -> [Step (bind step ~f)]
      (* Set.fold sr
               ~init:(Set.empty (module ResultPair))
               ~f:(fun acc pair -> 
                      match pair with
                      | e, store -> Set.union acc @@ (f e) store) *)
    )
let return a = 
  fun store -> [Done (a, store)]

let map = `Define_using_bind