(* This code is from the official manual https://v2.ocaml.org/releases/5.0/htmlman/effects.html#s%3Aeffects-shallow *)

open Effect
open Effect.Shallow

type _ Effect.t += Send : int -> unit Effect.t | Recv : int Effect.t

let run (comp : unit -> unit) : unit =
  let rec loop_send : type a. (a, unit) continuation -> a -> unit =
   fun k v ->
    continue_with k v
      {
        retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Send n -> Some (fun (k : (b, _) continuation) -> loop_recv n k ())
            | Recv -> failwith "protocol violation"
            | _ -> None);
      }
  and loop_recv : type a. int -> (a, unit) continuation -> a -> unit =
   fun n k v ->
    continue_with k v
      {
        retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Recv -> Some (fun (k : (b, _) continuation) -> loop_send k n)
            | Send _ -> failwith "protocol violation"
            | _ -> None);
      }
  in
  loop_send (fiber comp) ()

let printf = Printf.printf

let () =
  run (fun () ->
      printf "Send 42\n";
      perform (Send 42);
      printf "Recv: %d\n" (perform Recv);
      printf "Send 43\n";
      perform (Send 43);
      printf "Recv: %d\n" (perform Recv))

let () =
  run (fun () ->
      Printf.printf "Send 0\n";
      perform (Send 0);
      Printf.printf "Send 1\n";
      perform (Send 1) (* protocol violation *))
