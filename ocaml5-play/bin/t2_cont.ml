open Effect
open Effect.Deep

type _ Effect.t += Sleep: string -> int t

let run (f : unit -> int) = 
  Effect.Deep.try_with f ()
    { effc = fun (type a) (eff: a Effect.t) -> match eff with
      | (Sleep _s) -> Some (fun (_k: (a, _) continuation) -> 3)
      | _ -> None
  }

let () = 
  Printf.printf "\n[Print callstack]\n";
  Printf.printf "%d\n" @@ run (fun () -> let _ = perform (Sleep "Zzz") in failwith "ignored");
  ()

let run (f : unit -> int) = 
    Effect.Deep.try_with f ()
      { effc = fun (type a) (eff: a Effect.t) -> match eff with
        | (Sleep _s) -> Some (fun (k: (a, _) continuation) -> 
            ignore @@ continue k 3;
            ignore @@ continue k 4;
            42)
        | _ -> None
    }
  
let () = 
  Printf.printf "\n[Duplicate resumption]\n";
  (* Printf.printf "%d\n" @@ run (fun () -> let _ = perform (Sleep "Zzz") in 4); *)
  Printf.printf "%d\n" @@ run (fun () -> 
    let _ = perform (Sleep "Zzz") in failwith "ignored");
  ()
