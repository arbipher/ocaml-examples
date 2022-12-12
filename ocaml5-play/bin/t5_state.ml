(* Use Effect in OCaml 5 *)

open Effect
open Effect.Deep

type _ Effect.t += Get: int t | Put: int -> unit t

let run (f : unit -> unit) (init : int) : unit =
  (* try_with : ('b -> 'a) -> 'b -> 'a effect_handler -> 'a *)
  ignore @@ (try_with 
    (fun () -> (
      Printf.printf "try_with: before f\n";
      f (); 
      Printf.printf "try_with: after f\n";
      fun x -> x)) 
    ()
    {effc = fun (type a) (eff: a Effect.t) -> match eff with
      | Get     -> Some (fun (k: (a, _) continuation) -> 
        Printf.printf "get: start\n";
        let fg = fun (s : int) -> 
          Printf.printf "get: before cont, %d\n" s;
          let c = continue k s in
          Printf.printf "get: after cont\n";
          let ct = c s in
          Printf.printf "get: final cont, %d\n" ct;
          ct
        in fg
        ) 
      | (Put v) -> Some (fun (k: (a, _) continuation) -> 
        Printf.printf "put: start\n";
        let fp = fun _ -> 
          Printf.printf "pub: before cont\n";
          let c = continue k () in
          Printf.printf "pub: after cont\n";
          let ct = c v in
          Printf.printf "pub: final cont, %d\n" ct;
          ct
        in fp
        )
      | _ -> None}) 
      (
        Printf.printf "init: eval\n";
        init
      )

let simple () =
  Printf.printf "client: 1\n";
  assert (perform Get = 0);
  Printf.printf "client: 2\n";
  perform (Put 42);
  Printf.printf "client: 3\n"
  (* ;
  assert (perform Get = 42);
  Printf.printf "client: 4\n" *)

let () = 
  Printf.printf "\n";
  run simple 0

(* 
let get () = perform Get

let put i = perform (Put i)

let simple () =
  Printf.printf "client: 1\n";
  assert (get () = 0);
  Printf.printf "client: 2\n";
  put 42;
  Printf.printf "client: 3\n" *)


(* Use Monad *)

module State_monad = struct
  type 'a t = int -> 'a * int

  let bind (a : 'a t) (f : 'a -> 'b t) : 'b t = 
    fun s ->
      let a', s' = a s in
      (f a') s'
  
  let (>>=) a f = bind a f

  let return x = fun i -> x, i

  let get () : int t = fun i -> i, i

  let put i : unit t = fun _ -> (), i

  let run a init = 
    ignore @@ a () init
end

open State_monad;;

let simple () =
  get () >>= (fun x ->
  assert (x = 0);
  put 42) >>= (fun _ ->
  get ()) >>= (fun x ->
  assert (x = 42);
  return ())

let () = run simple 0