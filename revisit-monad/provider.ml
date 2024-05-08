type exit = int

(* module type BOX = sig
     type 'a t

     val wrap : 'a -> 'a t
   end

   module Script (Box : BOX) = struct
     let step1 : int Box.t = Box.wrap 42
     let step2 : int -> string Box.t = fun i -> Box.wrap (string_of_int i)
     let step3 : string -> exit Box.t = fun _s -> Box.wrap 0
   end *)

module Naive_play = struct
  let step1 : int = 42
  let step2 : int -> string = fun i -> string_of_int i
  let step3 : string -> exit = fun s -> if String.length s > 0 then 0 else -1
  let script () = step1 |> step2 |> step3
end

let () =
  let ret = Naive_play.script () in
  Fmt.pr "Naive play = %d@." ret

module Id = struct
  type 'a t = 'a

  let return (a : 'a) : 'a t = a
  let bind (ma : 'a) (f : 'a -> 'b t) : 'b t = f ma
  let run (ma : 'a t) : 'a = ma
end

module Id_play = struct
  let step1 : int Id.t = 42
  let step2 : int -> string Id.t = fun i -> Id.return (string_of_int i)

  let step3 : string -> exit Id.t =
   fun s ->
    let code = if String.length s > 0 then 0 else -1 in
    Id.return code

  let script () : exit Id.t = Id.bind (Id.bind step1 step2) step3
end

let () =
  let ret = Id.run (Id_play.script ()) in
  Fmt.pr "Id play = %d@." ret

module Provider = struct
  (*
     Unlike Id.t (type 'a t = 'a) where the box holds the value 'a, a Provider of 'a is a shy holder. It doesn't give you a value of 'a, but it can fullfil a thing that needs an 'a.
  *)
  type 'a t = ('a -> exit) -> exit

  let return (a : 'a) : 'a t = fun need_a -> need_a a

  let bind (provider_a : 'a t) (f : 'a -> 'b t) : 'b t =
    let provider_b need_b = (provider_a f) need_b in
    provider_b
end

let () = Fmt.pr "a"
