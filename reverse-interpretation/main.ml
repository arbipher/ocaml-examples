(* type 'a tree = 
  | Leaf of 'a 
  | Tree of 'a * 'a tree * 'a tree

let t1 = Tree (1, Leaf 2, Tree (3, Tree (4, Leaf 5, Leaf 6), Leaf 7));;

let rec string_of_tree tree : string = 
  match tree with
  | Leaf a -> string_of_int a
  | Tree (a, left, right) ->
    Printf.sprintf "[%s [%s] [%s]]" (string_of_int a) (string_of_tree left) (string_of_tree right)

;;
print_endline (string_of_tree t1);;

type 'a inverse_tree = 
  | ILeaf of 'a
  | ITree of 'a * 'a inverse_tree * 'a inverse_tree

let rec reverse_tree tree =
  match tree with
  | Leaf a -> ILeaf a
  | Tree (a, left, Leaf b) ->
    ITree (b, reverse_tree left, ILeaf a)
    (* let ir = reverse_tree right in
    let il = reverse_tree left in
    ITree (ir, il, a) *)
  | Tree (a, left, Tree (b, bleft, bright)) -> (

  )
   *)

type id = string

type exp =
  | Int of int
  | Plus of exp * exp
  | Var of id
  | Let of id * exp * exp
  | Fun of id * exp
  | Appl of exp * exp

let rec eval env e =
  match e with
  | Int i -> Int i
  | Plus (e1, e2) -> (
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    match v1, v2 with
    | Int i1, Int i2 -> Int (i1 + i2)
    | _ -> failwith "plus on non ints"
  )
  | Var id -> List.assoc id env
  | Let (id, e1, e2) -> (
    let v1 = eval env e1 in
    let env' = (id, v1)::env in
    eval env' e2
  )

let interp e = eval [] e

;;
interp (Let ("x", Int 1, Plus (Var "x", Int 2)));;

type anf_exp =
  | LetInt of id * int * anf_exp
  | LetPlus of id * id * id * anf_exp
  | Var of id

let rec aeval env e =
  match e with
  | LetInt (id, i, e) -> (
    let env' = (id, Int (i))::env in
    aeval env' e )
  | LetPlus (id, id1, id2, e) -> (
    let v1 = List.assoc id1 env and 
        v2 = List.assoc id2 env in
    match v1, v2 with
    | Int i1, Int i2 -> (
      let v = Int (i1 + i2) in
      let env' = (id, v)::env in
      aeval env' e
      )
    | _ -> failwith "plus on non ints"
  )
  | Var id -> List.assoc id env

let ainterp e = aeval [] e

let p = LetInt ("x", 1, LetInt ("y", 2, LetPlus("z", "x", "y", Var "z")))

;;
ainterp p;;

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

let check p =
  let v1 = ainterp p in
  let v2 = p |> inv |> ieval in
  match v1, v2 with
  | Int i1, Answer i2 when i1 = i2 -> i2
  | _ -> -1

;;
check p;;