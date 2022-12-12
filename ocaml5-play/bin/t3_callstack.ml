open Effect
open Effect.Deep

type _ Effect.t += Sleep: bool * string -> int t

let rec run (f : unit -> int) = 
  Effect.Deep.try_with f ()
    { effc = fun (type a) (eff: a Effect.t) -> match eff with
      | (Sleep (c,s)) -> Some (fun (k: (a, _) continuation) -> 
        Printf.printf "In handler %s\n" s;
        Printexc.print_raw_backtrace stdout (get_callstack k 20);

        (if c then
          let _ = run (fun () -> perform (Sleep (false, "inner " ^ s))) in ()
        else
          ()
        );

        let _ = continue k 3 in 42
      )
        (* Printexc.print_raw_backtrace stdout (get_callstack k 20); *)
      | _ -> None
  }

let () = 
  Printf.printf "\n[Effect handler stack]\n";
  Printf.printf "%d\n" @@ run (fun () -> 
    let _ = perform (Sleep (true, "Step1")) in
    let _ = perform (Sleep (true, "Step2")) in
    let _ = perform (Sleep (true, "Step3")) in
  3);
  ()

