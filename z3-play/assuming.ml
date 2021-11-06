open Z3

let ctx : context = mk_context []

let solver : Solver.solver = Solver.mk_solver ctx None

let true_ = Boolean.mk_val ctx true

let bvar n = Boolean.mk_const_s ctx n

let a = bvar "a"
let na = Boolean.mk_not ctx a
let f1 = Boolean.mk_eq ctx true_ a

let check_assuming pres = 
  Printf.printf "\n-> %s\n" (Solver.to_string solver);
  (match Solver.check solver pres with
   | Solver.SATISFIABLE ->
     begin
       match Solver.get_model solver with
       | None -> print_endline "none"
       | Some model -> 
         print_endline "SAT";
         print_endline @@ Model.to_string model
     end
   | Solver.UNSATISFIABLE ->
     print_endline "UNSAT"
   | Solver.UNKNOWN ->
     failwith @@ Printf.sprintf "Unknown result in solve: %s"
       (Solver.get_reason_unknown solver)
  );
  Printf.printf "<- %s\n" (Solver.to_string solver)

let () =
  Solver.add solver [f1];
  check_assuming [];
  check_assuming [na];
  check_assuming []

let%expect_test _ = ()

let check pres = 
  Printf.printf "%s\n" (Solver.to_string solver);
  (match Solver.check solver pres with
   | Solver.SATISFIABLE ->
     begin
       match Solver.get_model solver with
       | None -> print_endline "none"
       | Some model -> 
         print_endline "SAT";
         print_endline @@ Model.to_string model
     end
   | Solver.UNSATISFIABLE ->
     print_endline "UNSAT"
   | Solver.UNKNOWN ->
     failwith @@ Printf.sprintf "Unknown result in solve: %s"
       (Solver.get_reason_unknown solver)
  )