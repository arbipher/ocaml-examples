(* Use Effect in OCaml 5 *)

open Effect
open Effect.Deep

type _ Effect.t += E_some: 'a -> 'a t | E_none : unit t

let run (f : unit -> unit) : unit =
  try_with f ()
    {effc = fun (type a) (eff: a Effect.t) -> match eff with
      | E_some x -> Some (fun (k: (a, _) continuation) -> continue k x)
      | E_none -> Some (fun (k: (a, _) continuation) -> discontinue k (Failure "None"))
      | _ -> None}

let some x = perform (E_some x)

let none () = perform (E_none)

let simple () =
  let s = some 3 in
  (* let t = none () in *)
  let t = some 1 in
  some (s + t)

let () = 
  let _v = run simple in ()
