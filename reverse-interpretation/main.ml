type id = string

module Exp = struct
type exp =
  | Int of int
  | Plus of exp * exp
  | Var of id
  | Let of id * exp * exp
  | Fun of id * exp
  | Appl of exp * exp

let rec subst e id et =
  match e with
  | Int i -> Int i
  | Plus (e1, e2) -> Plus (subst e1 id et, subst e2 id et)
  | Var vid -> if vid = id then et else Var vid
  | Let (lid, e1, e2) -> Let (lid, subst e1 id et, if lid = id then e2 else subst e2 id et)
  | Fun (aid, ebody) -> Fun (aid, if aid = id then ebody else subst ebody id et)
  | Appl (e1, e2) -> Appl (subst e1 id et, subst e2 id et)

let rec eval e =
  match e with
  | Int i -> Int i
  | Plus (e1, e2) -> (
    let v1 = eval e1 in
    let v2 = eval e2 in
    match v1, v2 with
    | Int i1, Int i2 -> Int (i1 + i2)
    | _ -> failwith "plus on non ints"
  )
  | Var id -> Var id
  | Let (id, e1, e2) -> (
    let v1 = eval e1 in
    eval (subst e2 id v1)
  )
  | Fun (id, e) -> Fun (id, e)
  | Appl (e1, e2) -> (
    let v1 = eval e1
    in match v1 with
    | Fun (id, ebody) -> eval (subst ebody id v1)
    | _ -> failwith "eval: e1 not eval to Fun"
  )
end

open Exp;;
eval (Let ("x", Int 1, Plus (Var "x", Int 2)));;

module Anf = struct
type anf_exp =
  | Let of id * clause * anf_exp
  | TopVar of id
and clause = 
  | Value of value
  | Plus of id * id
  | Alias of id
  | App of id * id
and value =
  | Int of int
  | Fun of id * anf_exp

let rec subst env e =
  if List.length env = 0 then
    e
  else
    match e with
    | Let (id, c, e) -> Let (id, subst_clause env c, subst (List.remove_assoc id env) e)
    | TopVar id -> TopVar id
and subst_clause env c =
  match c with
  | Value (Fun (id, e)) -> Value (Fun (id, subst (List.remove_assoc id env) e))
  | Alias vid -> (
    match List.assoc_opt vid env with
    | Some v -> Value v
    | None -> Alias vid
  )
  | rest -> rest
  
let rec aeval env e : value =
  match e with
  | Let (id, c, e) -> (
      let v = 
        match c with
        | Value (Int v) -> Int v
        | Plus (id1, id2) -> (
            let v1 = List.assoc id1 env
            and v2 = List.assoc id2 env in
            match v1, v2 with
            | Int i1, Int i2 -> Int (i1 + i2)
            | _ -> failwith "plus on non Ints"
          )
        (* | Alias aid -> List.assoc aid env *)
        | Value (Fun (id, ebody)) -> Fun (id, subst (List.remove_assoc id env) ebody)
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
  | TopVar id -> List.assoc id env

  let ainterp e = aeval [] e
end

open Anf;;

let p1 = 
  (Let ("x", (Value (Int 1)),
    (Let ("y", (Value (Int 2)),
      (Let ("z", (Plus ("x", "y")),
        (TopVar "z")
      ))
    ))
  ))

;;
ainterp p1;;

let p2 = 
  (Let ("one", (Value
    (Int 1)),
  (Let ("add1", (Value 
    (Fun ("x", 
      (Let ("xa1", (Plus ("x", "one")), 
        TopVar "xa1"
    ))))),
  (Let ("two", (Value 
    (Int 2)),
  (Let ("z", (App ("add1", "two")),
  TopVar "z"
  ))
  ))
  ))
  ))

;;
ainterp p2;;

module Inverse_anf = struct
  type exp =
    | Top
    | BottomVar of id * exp
    | InvLet of id * invClause * exp
  and clause =
    | Value of value
  and invValue =
    | 

  type inverse_anf_exp =
    | End (* top *)
    | Answer of int (* special for answer *)
    | IVar of id * inverse_anf_exp (* end *)
    | IInt of id * int * inverse_anf_exp
    | IPlus of id * id * id * inverse_anf_exp

  let rec inverse_exp ctx e =
    match e with
    | Var id -> IVar (id, ctx)
    | LetInt (id, i, e) -> (
      let ctx' = IInt (id, i, ctx) in
      inverse_exp ctx' e
    )
    | LetPlus (id, id1, id2, e) -> (
      let ctx' = IPlus (id, id1, id2, ctx) in
      inverse_exp ctx' e
    )

  let inv e = inverse_exp End e

  let ieval e =
    let rec eval id e = 
      match e with
      | IInt (idi, i, ie) -> 
        if idi = id then 
          Answer i 
        else
          eval id ie
      | IPlus (idi, id1, id2, ie) ->
        if idi = id then
          let v1 = eval id1 ie
          and v2 = eval id2 ie in
          match v1, v2 with
          | Answer i1, Answer i2 -> Answer (i1 + i2)
          | _ -> failwith "ieval: not Answer"
        else
          eval id ie
      | IVar _ -> failwith "ieval: IVar"
      | End -> failwith "ieval: End"
      | Answer i -> failwith "ieval: not Answer in ieval"
    in
    match e with
    | IVar (id, ie) -> eval id ie
    | _ -> failwith "not valid anf"
end

open Inverse_anf;;

let check p =
  let v1 = eval p in
  let v2 = p |> inv |> ieval in
  match v1, v2 with
  | Int i1, Answer i2 when i1 = i2 -> i2
  | _ -> -1

;;
check p;;
