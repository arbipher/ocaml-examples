open Effect
open Effect.Deep

type _ Effect.t += Get: int t | Put: int -> unit t

let run (f : unit -> unit) (init : int) : unit =
  (* try_with : ('b -> 'a) -> 'b -> 'a effect_handler -> 'a *)
  let s = ref init in
  ignore @@ (try_with f ()
    {effc = fun (type a) (eff: a Effect.t) -> match eff with
      | Get     -> Some (fun (k: (a, _) continuation) -> 
        Printf.printf "get: start\n";
        continue k !s)
      | (Put v) -> Some (fun (k: (a, _) continuation) -> 
        s := v;
        continue k ())
      | _ -> None}) init

let simple () =
  Printf.printf "client: 1\n";
  assert (perform Get = 0);
  Printf.printf "client: 2\n";
  perform (Put 42);
  Printf.printf "client: 3\n";
  assert (perform Get = 42)

let () = 
  Printf.printf "\n";
  run simple 0