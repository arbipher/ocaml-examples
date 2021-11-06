(* open Z3

   let ctx : context = mk_context []

   let solver : Solver.solver = Solver.mk_solver ctx None

   let intSort : Sort.sort = Arithmetic.Integer.mk_sort ctx

   let boolSort : Sort.sort = Boolean.mk_sort ctx

   let intConstructor = Datatype.mk_constructor_s ctx "IntV"
    (Symbol.mk_string ctx "is-IntV") [Symbol.mk_string ctx "i"] [Some intSort] [1]
   let boolConstructor = Datatype.mk_constructor_s ctx "BoolV"
    (Symbol.mk_string ctx "is-BoolV") [] [] []

   let nodeConstructor = Datatype.mk_constructor_s ctx "node"
    (Symbol.mk_string ctx "is-node") 
    [Symbol.mk_string ctx "left"; Symbol.mk_string ctx "node"; Symbol.mk_string ctx "right"]
    [None; Some intSort; None] [0; 0; 0]

   let ibSort = Datatype.mk_sort_s ctx "IntOrBool"
    [intConstructor; boolConstructor]
   let e1 = Expr.mk_const_s ctx "random" ibSort

   let true_ = Boolean.mk_val ctx true

   let intcDecl = Datatype.Constructor.get_constructor_decl intConstructor

   let intv1 = FuncDecl.apply intcDecl [Expr.mk_numeral_int ctx 42 intSort]

   let f1 = Boolean.mk_eq ctx e1 intv1
   (* true_ *)

   let () =
   Solver.add solver [f1];
   print_endline @@ Solver.to_string solver;
   match Solver.check solver [] with
   | Solver.SATISFIABLE ->
    begin
      match Solver.get_model solver with
      | None -> print_endline "none"
      | Some model -> print_endline @@ Model.to_string model
    end
   | Solver.UNSATISFIABLE ->
    print_endline "UNSAT"
   | Solver.UNKNOWN ->
    failwith @@ Printf.sprintf "Unknown result in solve: %s"
      (Solver.get_reason_unknown solver) *)

