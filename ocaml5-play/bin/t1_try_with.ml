(* Vanilla OCaml exception *)

exception Stop of int

let run f =
  try
    f ()
  with
  | Stop x -> x + 1
  | _ -> 42

let () =
  Printf.printf "\n[Vanilla OCaml exception]\n";
  Printf.printf "%d\n" @@ run (fun () -> raise (Stop 2));
  Printf.printf "%d\n" @@ run (fun () -> failwith "Sth else")

(* Observation:
1. try ... with is a language feature 
2. try body `f ()` and with case `x + 1` and `42` share the same return type.
*)

open Core

let oi_to_string = function
  | Some i -> Int.to_string i
  | None -> "none"

let orun f =
  (* (unit -> 'a) -> 'a option *)
  let ans = Option.try_with f in
  Printf.printf "outer: post try_with\n";
  ans

let () = 
  Printf.printf "\n[Option.try_with]\n";
  Printf.printf "%s\n" @@ oi_to_string @@ orun (fun () -> 3);
  Printf.printf "%s\n" @@ oi_to_string @@ orun (fun () -> failwith "no int")

(* Observation:
`try_with` exists in other modules before OCaml 5.
*)

(* Variation of try_with in OCaml 5 *)

open Effect
open Effect.Deep

type _ Effect.t += Sleep: string -> int t

(* 
utop # Effect.Deep.try_with;;
- : ('b -> 'a) -> 'b -> 'a Effect.Deep.effect_handler -> 'a = <fun>

utop # Effect.perform;;
- : 'a Effect.t -> 'a = <fun>
*)

(* For short:
type `'b Effect.t` is user-defined effect variant.

type `'a effect_handler` is a (simple) record containing field `effc` a function to handle the effect.
    'a is the return type for the computation. 

a `'a effect_handler` is used in `Deep.try_with`

type `('a, 'b) handler` is a (full) record containing `effc`,
    `retc : 'a -> 'b` for post-processing non-effect normal returning, and
    `exnc`: exn -> 'b` for post-processing exception
*)

let run f = 
  Effect.Deep.try_with f ()
    { effc = fun (type a) (eff: a Effect.t) -> match eff with
      | (Sleep _s) -> Some (fun (k: (a, _) continuation) -> continue k 0)
      | _ -> None
  }

let () = 
  Printf.printf "\n[Effect.Deep.try_with]\n";
  Printf.printf "%d\n" @@ run (fun () -> perform (Sleep "Zzz"));
  ()

(* Observation:
`try_with` wraps the world of effect. 
Many examples also use `run` function to wrap `try_with`, 
however, it's just a normal OCaml function.
*)

(* 
utop # Effect.Deep.match_with;;
- : ('c -> 'a) -> 'c -> ('a, 'b) Effect.Deep.handler -> 'b = <fun>   
*)

let run (f : unit -> int) = 
  Effect.Deep.match_with f ()
    { effc = (fun (type a) (eff: a Effect.t) -> match eff with
      | (Sleep _s) -> Some (fun (k: (a, _) continuation) -> continue k 0)
      | _ -> None);
     retc = (fun i -> "retc: " ^ (Int.to_string i));
     exnc = raise;
  }

let () = 
  Printf.printf "\n[Effect.Deep.try_with (effc)]\n";
  Printf.printf "%s\n" @@ run (fun () -> perform (Sleep "Zzz"));
  ()

let run (f : unit -> int) = 
  Effect.Deep.match_with f ()
    { effc = (fun (type a) (eff: a Effect.t) -> match eff with
      | (Sleep _s) -> Some (fun (k: (a, _) continuation) -> discontinue k (Stop 1)  (* failwith "boo" *) )
      | _ -> None);
      retc = (fun i -> "retc: " ^ (Int.to_string i));
      exnc = fun _ -> "exec: ";
  }

let () = 
  Printf.printf "\n[Effect.Deep.try_with (exception)]\n";
  Printf.printf "%s\n" @@ run (fun () -> perform (Sleep "Zzz"));
  ()