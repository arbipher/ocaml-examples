
(* module FunctionalMonad = struct
  type ('a, 'e) t = 'a -> int

  let bind m ~f =
    fun a ->
      f @@ m a
  
  let return a =
    fun a -> 0

  let map = `Define_using_bind
end

module M = Monad.Make2(FunctionalMonad) *)
(* 
module IO = struct
  type 'a t = {
    react : unit -> 'a
    }

  let bind m ~f =
    f @@ m.react ()

  let return a = 
    { react = fun () -> a }

  let map = `Define_using_bind
end

module M = Monad.Make(IO)

module IO2 = struct
  type 'a t = unit -> 'a

  let bind m ~f =
    f @@ m ()

  let return a = 
    fun () -> a

  let map = `Define_using_bind
end

module M2 = Monad.Make(IO2)


module IO3 = struct
  type 'a t = {
    react : unit -> 'a
    }

  let bind m ~f =
    f @@ m.react ()

  let return a = 
    { react = fun () -> a }

  let map = `Define_using_bind
end

module M3 = Monad.Make(IO3) *)

(* 
module Concurrent_symbolic = struct
  (* 'a = bool *)
  type ('a, 'constraints) task =
    | Done of 'a
    | Step of 'a mon
  and 'a mon = 'constraints -> ('a task * 'constraints) list
end

module Concurrent_analysis = struct
  (* 'a = 'abs_result *)
  type ('a, 'constraints) task =
    | Done of 'a
    | Step of 'a mon
  and 'a mon = 'constraints -> ('a task * 'constraints) list
end

module Analysis = struct
  type ('abs_store, 'abs_result) t = 'abs_store -> 'abs_result
end

*)