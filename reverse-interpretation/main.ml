#require "ppx_show.runtime";;

type id = string

module Toy = struct
  type exp =
    | Int of int
    | Var of id
    | Fun of id * exp
    | Plus of exp * exp
    | App of exp * exp
    | Let of id * exp * exp

    (* | Bool of bool
    | Eq of exp * exp *)

  let rec subst e id et =
    match e with
    | Int i -> Int i
    | Plus (e1, e2) -> Plus (subst e1 id et, subst e2 id et)
    | Var vid -> if vid = id then et else Var vid
    | Let (lid, e1, e2) -> Let (lid, subst e1 id et, if lid = id then e2 else subst e2 id et)
    | Fun (aid, ebody) -> Fun (aid, if aid = id then ebody else subst ebody id et)
    | App (e1, e2) -> App (subst e1 id et, subst e2 id et)

    (* | Bool b -> Bool b
    | Eq (e1, e2) -> Eq (subst e1 id et, subst e2 id et) *)

  let rec eval e =
    match e with
    | Int i -> Int i
    | Plus (e1, e2) -> (
      let v1 = eval e1 in
      let v2 = eval e2 in
      match v1, v2 with
      | Int i1, Int i2 -> Int (i1 + i2)
      | _ -> failwith "plus on non ints 1"
    )
    | Var id -> Var id
    | Let (id, e1, e2) -> (
      let v1 = eval e1 in
      eval (subst e2 id v1)
    )
    | Fun (id, e) -> Fun (id, e)
    | App (e1, e2) -> (
      let v1 = eval e1
      in match v1 with
      | Fun (id, ebody) -> (
        let v2 = eval e2 in
        eval (subst ebody id v2)
      )
      | _ -> failwith "eval: e1 not eval to Fun"
    )

    (* | Bool b -> Bool b
    | Eq (e1, e2) -> (
      let v1 = eval e1 in
      let v2 = eval e2 in
      match v1, v2 with
      | Int i1, Int i2 -> Bool (i1 = i2)
      | _ -> failwith "eq on non ints 1" 
        )
*)

  let int_of = function
  | Int i -> i
  | _ -> failwith "Toy.int_of: not int"
end

module Anf : (
  sig 
    type exp =
      | Let of id * clause * exp
      | BottomVar of id
    and clause = 
      | Value of value
      | Plus of id * id
      | Alias of id
      | App of id * id
    and value =
      | Int of int
      | Fun of id * exp
    [@@deriving show] 

    val eval : exp -> value
    val int_of : value -> int
  end
) = struct
  type exp =
    | Let of id * clause * exp
    | BottomVar of id
  and clause = 
    | Value of value
    | Plus of id * id
    | Alias of id
    | App of id * id
  and value =
    | Int of int
    | Fun of id * exp
    [@@deriving show]

  let rec subst env e =
    if List.length env = 0 then
      e
    else
      match e with
      | Let (id, c, e) -> Let (id, subst_clause env c, subst (List.remove_assoc id env) e)
      | BottomVar id -> BottomVar id
  and subst_clause env c =
    match c with
    | Value (Fun (id, e)) -> Value (Fun (id, subst (List.remove_assoc id env) e))
    | Alias vid -> (
      match List.assoc_opt vid env with
      | Some v -> Value v
      | None -> Alias vid
    )
    | rest -> rest
    
  let rec aeval env e =
    match e with
    | Let (id, c, e) -> (
        let v = 
          match c with
          | Value (Int v) -> Int v
          | Value (Fun (id, ebody)) -> Fun (id, subst (List.remove_assoc id env) ebody)
          | Plus (id1, id2) -> (
              let v1 = List.assoc id1 env
              and v2 = List.assoc id2 env in
              match v1, v2 with
              | Int i1, Int i2 -> Int (i1 + i2)
              | _ -> failwith "plus on non Ints 2"
            )
          | Alias id ->
            List.assoc id env
          | App (id1, id2) -> (
              let v1 = List.assoc id1 env in
              match v1 with
              | Fun (id, ebody) -> (
                let v2 = List.assoc id2 env in
                let env' = (id, v2)::env in
                aeval env' ebody
              )
              | _ -> failwith "App on non Fun"
            ) 
          in
        let env' = (id, v)::env in
        aeval env' e 
    )
    | BottomVar id -> List.assoc id env

  let eval e = aeval [] e

  let int_of = function
  | Int i -> i
  | _ -> failwith "Anf.int_of: not int"
end

module Inverse_anf : (
  sig 
    type exp =
      | Top
      | BottomVar of id * exp
      | InvLet of id * clause * exp
    and clause =
      | Value of value
      | Plus of id * id
      | Alias of id
      | App of id * id
    and value =
      | Int of int
      | Fun of id * exp
      | Lazi of value lazy_t
    [@@deriving show] 

    val eval : exp -> value
    val int_of : value -> int
  end
)  = struct
  type exp =
    | Top
    | BottomVar of id * exp
    | InvLet of id * clause * exp
  and clause =
    | Value of value
    | Plus of id * id
    | Alias of id
    | App of id * id
  and value =
    | Int of int
    | Fun of id * exp
    | Lazi of value lazy_t
  [@@deriving show {with_path = false}] 

  let rec inv_eval e src =
    let rec search id ew src =
      (* Printf.printf "search %s at \n%s guarded \n%s"
        id (show_exp ew) (show_exp ctx); *)
      match ew with
      | Top -> 
        if src <> Top then
          search id src Top
        else
          failwith ("search: undefined " ^ id)
      | InvLet (x, c, e) -> (
        if id = x then (
          match c with
          | Value (Int i) -> Int i, Top
          | Value (Lazi v) -> (Lazy.force_val v), Top
          | Plus (id1, id2) -> (
            match search id1 e src, search id2 e src with
            | (Int i1, _), (Int i2, _) -> Int (i1 + i2), Top
            | _ -> failwith "inv_eval: plus on non ints "
          ) 
          | Value (Fun (x, ef)) -> Fun (x, ef), e
          | App (id1, id2) -> (
            match search id1 e src with
            | Fun (x, e), src_f -> (
              let v2 = lazy (let v2, _ = search id2 ew src in v2) in
              let src_fbody = InvLet (x, Value (Lazi v2), src_f) in
              match inv_eval e src_fbody with
              | Fun (xa, ea), _ -> Fun (xa, ea), src_fbody
              | rest -> rest
            )
            | _ -> failwith "inv_eval: app on non fun"
          )
          | Alias a -> search a e src
        ) else
          search id e src
      )
      | BottomVar (x, e) -> 
        search id e src
        (* failwith "inv_eval: BottomVar" *)
      in
    match e with
    | BottomVar (x, e) -> search x e src
    | _ -> failwith "inv_eval: need a BottomVar"

  let eval e = 
    let v, _ctx = inv_eval e Top in 
    v

  let int_of = function
    | Int i -> i
    | _ -> failwith "Inverse_anf.int_of: not int"
end

module Convertor = struct
  let counter = ref 0

  let get_counter () : int =
    counter := !counter + 1;
    !counter

  let new_id () : string =
    "x-" ^ (string_of_int (get_counter ()))

  let hole = Anf.BottomVar "__"

  let rec linearize acc e =
    let open Anf in
    match e with
    | Let (id, c, e2) -> 
      let acc' = (Let (id, c, hole)) :: acc in
      linearize acc' e2
    | BottomVar x -> 
      (List.rev acc), x

  let rec anf_of_list el b =
    let open Anf in
    match el with
    | (Let (id, c, e)) :: es -> Let (id, c, (anf_of_list es b))
    | [] -> BottomVar b
    | _ -> failwith "any_of_list: never here"

  let rec anf_of_toy (e : Toy.exp) : Anf.exp =
    let merge e1 e2 kon =
      let v1 = anf_of_toy e1
      and v2 = anf_of_toy e2 in
      let el1, b1 = linearize [] v1
      and el2, b2 = linearize [] v2 in
      let x = new_id () in
      let ek = kon b1 b2 x in
      anf_of_list (el1 @ el2 @ [ek]) x 
    in
    let merge_let e1 e2 x =
      let v1 = anf_of_toy e1
      and v2 = anf_of_toy e2 in
      let el1, b1 = linearize [] v1
      and el2, b2 = linearize [] v2 in
      anf_of_list (el1 @ [Anf.Let (x, Anf.Alias b1, hole)] @el2) b2
    in
    let open Anf in
    match e with
    | Toy.Int i -> (
      let x = new_id () in
      Let (x, Value (Int i), BottomVar x)
    )
    | Toy.Var x -> (
      let xn = new_id () in
      Let (xn, Alias x, BottomVar xn)
    )
    | Toy.Fun (xa, e) -> (
      let x = new_id () in
      Let (x, Value (Fun (xa, anf_of_toy e)), BottomVar x)
    )
    | Toy.Plus (e1, e2) -> (
      let kon b1 b2 x = Let (x, Plus (b1, b2), hole) in
      merge e1 e2 kon
    )
    | Toy.App (e1, e2) -> (
      let kon b1 b2 x = Let (x, App (b1, b2), hole) in
      merge e1 e2 kon
    )
    | Toy.Let (x, e1, e2) -> (
      merge_let e1 e2 x
    )


  let rec inverse_exp ctx e =
    let open Inverse_anf in
    match e with
    | Anf.BottomVar id -> 
        BottomVar (id, ctx)
    | Anf.Let (id, c, e) ->
        let ctx' = InvLet (id, inverse_clause c, ctx) in
        inverse_exp ctx' e
  and inv_of_anf e = inverse_exp Top e
  and inverse_clause c =
    match c with
    | Anf.Value (Int i) -> Value (Int i)
    | Anf.Value (Fun (id, e)) -> Value (Fun (id, inv_of_anf e))
    | Anf.Plus (id1, id2) -> Plus (id1, id2)
    | Anf.Alias id -> Alias id
    | Anf.App (id1, id2) -> App (id1, id2)

  let test (pt : Toy.exp) =
    let pa = anf_of_toy pt in
    let pi = inv_of_anf pa in
    let it = pt |> Toy.eval |> Toy.int_of in
    let ia = pa |> Anf.eval |> Anf.int_of in
    let ii = pi |> Inverse_anf.eval |> Inverse_anf.int_of in
    begin
      assert (it = ia);
      assert (ia = ii);
      ()
    end
end

open Toy
open Convertor
;;

let p1 = Int 5

let p2 = Plus (Int 3, Int 4)

let p3 = Let ("x", p2, Plus (Var "x", Int 6))

let p4 = 
  Let ("add_one", 
    Fun ("x", Plus (Var "x", Int 1)),
  App (Var "add_one", Int 1)
)

let p5 = 
  Let ("apply", 
    Fun ("f", Fun ("x", App (Var "f", Var "x"))),
  Let ("add_one", 
    Fun ("x", Plus (Var "x", Int 1)),
  App ((App (Var "apply", Var "add_one")), Int 1)
))

let p6 = Plus (Plus (Int 1, Int 2), Plus (Int 3, Int 4))

let p7 = 
  Let ("t", Int 1,
  Let ("d", 
    Fun ("x", Var "t"),
  App (Var "d", Int 2)))

let p8 = 
  Let ("t", Int 1,
  Let ("d", 
    Fun ("x", Fun ("y", Var "t")),
  App ((App (Var "d", Int 1)), Int 2)))

let p9 = 
  Let ("add", 
    Fun ("x", Fun ("y", Plus (Var "x", Var "y"))),
  App ((App (Var "add", Int 1)), Int 2))

let p10 =
  Let ("add3", 
    Fun ("x1", Fun ("x2", Fun ("x3", 
      Plus (Var "x1", Plus (Var "x2", Var "x3"))))),
  App (App ((App (Var "add3", Int 1)), Int 2), Int 3))

let p11 = 
  Let ("p", Int 1, 
    Let ("f", Fun ("x", Var "p"), 
      Let ("p", Int 2, 
        App (Var "f", Int 1))));;

let p12 = 
  Let ("p", Int 1, 
    Let ("f", Fun ("x", Var "p"), 
      Let ("p", Int 2, 
        App (Var "f", Var "_"))));;

let ps = [p1; p2; p3; p4; p5; p6; p7; p8; p9; p10; p11]

let ieval p = p |> anf_of_toy |> inv_of_anf |> Inverse_anf.eval

;;
List.iter test ps;;