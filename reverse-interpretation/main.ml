type id = string

module Toy = struct
  type exp =
    | Int of int
    | Plus of exp * exp
    | Var of id
    | Let of id * exp * exp
    | Fun of id * exp
    | App of exp * exp

  let rec subst e id et =
    match e with
    | Int i -> Int i
    | Plus (e1, e2) -> Plus (subst e1 id et, subst e2 id et)
    | Var vid -> if vid = id then et else Var vid
    | Let (lid, e1, e2) -> Let (lid, subst e1 id et, if lid = id then e2 else subst e2 id et)
    | Fun (aid, ebody) -> Fun (aid, if aid = id then ebody else subst ebody id et)
    | App (e1, e2) -> App (subst e1 id et, subst e2 id et)

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

  let int_of = function
  | Int i -> i
  | _ -> failwith "Toy.int_of: not int"
end

module Anf = struct
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
    
  let rec aeval env e : value =
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
          | Alias aid -> (
            let env' = (id, List.assoc aid env)::env in
            aeval env' e
          )
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

module Inverse_anf = struct
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
    | Empty (* exception for Non_Found *)

  let rec inverse_exp ctx e =
    match e with
    | Anf.BottomVar id -> 
        BottomVar (id, ctx)
    | Anf.Let (id, c, e) ->
        let ctx' = InvLet (id, inverse_clause c, ctx) in
        inverse_exp ctx' e
  and inv e = inverse_exp Top e
  and inverse_clause c =
    match c with
    | Anf.Value (Int i) -> Value (Int i)
    | Anf.Value (Fun (id, e)) -> Value (Fun (id, inv e))
    | Anf.Plus (id1, id2) -> Plus (id1, id2)
    | Anf.Alias id -> Alias id
    | Anf.App (id1, id2) -> App (id1, id2)

  let rec inv_eval e =
    let rec eval id e =
      match e with
      | Top -> Empty
      | BottomVar (x, e) -> eval x e
      | InvLet (x, c, e) -> (
        if id = x then (
          match c with
          | Value (Int i) -> Int i
          | Value (Fun (x, e)) -> Fun (x, e)
          | Value Empty -> failwith "inv_eval: Empty is returned only"
          | Plus (id1, id2) -> (
            match eval id1 e, eval id2 e with
            | Int i1, Int i2 -> Int (i1 + i2)
            | _ -> failwith "inv_eval: plus on non ints "
          ) 
          | App (id1, id2) -> (
            match eval id1 e with
            | Fun (x, e) -> (
              let v2 = eval id2 e in
              let e' = InvLet (x, Value v2, e) in
              inv_eval e'
            )
            | _ -> failwith "inv_eval: app on non fun"
          )
          | Alias a -> (
            let v = eval a e in
            if v <> Empty then
              v
            else
              eval id e
          ) 
        ) else
          eval id e
      ) in
    match e with
    | BottomVar (x, e) -> eval x e
    | _ -> failwith "inv_eval: need a BottomVar"

  let eval = inv_eval

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

(* 
  let rec linearize (e : Toy.exp) : Toy.exp =
    let hole = Var "_" in
    let last_id el =
      match List.hd el with
      | Let (x, _, _) -> x
      | Var x -> x
      | _ -> failwith "last_id"
    in
    match e with
    | Int i -> Let (new_id ()), Int i, hole)]
    | Var id -> [Var id]
    | Fun (id, e) -> [Let (new_id ()), Fun (id, linearize e), hole)]
    | Plus (e1, e2) -> (
      let v1, v2 = linearize e1, linearize e2 in
      let x1, x2 = last_id v1, last_id v2 in
      let eplus = [
        Let (new_id (), Plus (Var x1, Var x2), hole)
      ]
    )

  let rec letize (e : Toy.exp) : Toy.exp = 
    let rec replace_last e k =
      match e with
      | Var x -> x, k
      | Let (x, e1, Var x1) -> x1, Let (x, e1, k)
      | Let (x, e1, e2) -> (
        let xk, xe = replace_last e2 k in
        xk, Let (x, e1, xe)
      ) 
      | _ -> failwith "replace_last"
    in
    let open Toy in
      match e with
      | Int i -> (
        let x = new_id () in
        Let (x, Int i, Var x)
      )
      | Fun (id, e) -> (
        let x = new_id () in
        Let (x, Fun (id, letize e), Var x)
      )
      | Var x -> Var x
      | Plus (e1, e2) -> (
        let v1, v2 = letize e1, letize e2 in
        let v1x, v1' = replace_last v1 v2 in
        let v2x, 
      )

  let rec letize (e : Toy.exp) : Toy.exp = 
    let open Toy in
      match e with
      | Int i -> Int i
      | Fun (id, e) -> Fun (id, letize e)
      | Var x -> Var x
      | Plus (e1, e2) -> (
        let e1' = letize e1 
        and e2' = letize e2 in
          match e1', e2' with
          | Int i1, Int i2 -> (
            let x1, x2, x3 = new_id (), new_id (), new_id () in
            Let (x1, e1', 
            Let (x2, e2', 
            Let (x3, Plus (Var x1, Var x2), 
            (Var x3)))))
          | _ -> failwith "hehe"
      )
      | Let (id, e1, e2) -> Let (id, letize e1, letize e2)
      | App (e1, e2) -> (
        let e1' = letize e1 
        and e2' = letize e2 in
        let x1, x2, x3 = new_id (), new_id (), new_id () in
        Let (x1, e1', 
        Let (x2, e2', 
        Let (x3, App (Var x1, Var x2),
        (Var x3))))
      ) *)

  let anf_of_toy (e : Toy.exp) : Anf.exp =
    match e with
    | Toy.Int i -> (
      let x = new_
    )

  let rec inv_of_anf (e : Anf.exp) : Inverse_anf.exp =
    Inverse_anf.Top

  let test (pt : Toy.exp) : Inverse_anf.exp =
    let pa = anf_of_toy pt in
    let pi = inv_of_anf pa in
    let it = pt |> Toy.eval |> Toy.int_of in
    let ia = pa |> Anf.eval |> Anf.int_of in
    let ii = pi |> Inverse_anf.eval |> Inverse_anf.int_of in
    begin
      assert (it = ia);
      assert (ia = ii);
      pi
    end
end

open Toy
open Convertor
;;

let p1 = Int 5

let p2 = Plus (Int 3, Int 4)

let p3 = Let ("x", p2, Plus (Var "x", Int 6))

let p4 = 
  Let ("add_one", Fun ("x", Plus (Var "x", Int 1)),
  App (Var "add_one", Int 1)
)

let p5 = 
  Let ("apply", Fun ("f", Fun ("x", App (Var "f", Var "x"))),
  Let ("add_one", Fun ("x", Plus (Var "x", Int 1)),
  App ((App (Var "apply", Var "add_one")), Int 1)
))

let p6 = Plus (Plus (Int 1, Int 2), Plus (Int 3, Int 4))

let ps = [p1; p2; p3; p4; p5; p6]

;;
List.iter (fun p -> assert (eval p = eval (letize p))) ps;;

;;
letize p6;;

(* anf_of_toy @@ Int 3;; *)

  (* type inverse_exp =
    | End (* top *)
    | Answer of int (* special for answer *)
    | IVar of id * inverse_exp (* end *)
    | IInt of id * int * inverse_exp
    | IPlus of id * id * id * inverse_exp

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
    | _ -> failwith "not valid anf" *)
(* 
open Exp;;
eval (Let ("x", Int 1, Plus (Var "x", Int 2)));;

open Anf;;

let p1 = 
  (Let ("x", (Value (Int 1)),
    (Let ("y", (Value (Int 2)),
      (Let ("z", (Plus ("x", "y")),
        (BottomVar "z")
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
        BottomVar "xa1"
    ))))),
  (Let ("two", (Value 
    (Int 2)),
  (Let ("z", (App ("add1", "two")),
  BottomVar "z"
  ))
  ))
  ))
  ))

;;
ainterp p2;;


open Inverse_anf;;

let check p =
  let v1 = eval p in
  let v2 = p |> inv |> ieval in
  match v1, v2 with
  | Int i1, Answer i2 when i1 = i2 -> i2
  | _ -> -1

;;
check p;; *)
