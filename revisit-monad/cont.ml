[@@@warning "-32"]
(* https://discuss.ocaml.org/t/continuation-monad-with-polymorphic-return-type/6330 *)

module Cont = struct
  type ('a, 'r) cont = 'a -> 'r
  type 'a t = { cont : 'r. ('a, 'r) cont -> 'r }

  (*  'a                    'a -> 'r   'a *)
  let ret (x : 'a) = { cont = (fun k -> k x) }

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
    { cont = (fun k -> x.cont (fun v -> (f v).cont k)) }

  let fish (x : 'a -> 'b t) (y : 'b -> 'c t) : 'a -> 'c t =
   (* fun a -> bind (x a) y *)
   fun a -> { cont = (fun k -> (x a).cont (fun v -> (y v).cont k)) }

  (* https://gist.github.com/divarvel/d638c12edc335838f7da *)
  let join (x : 'a t t) : 'a t =
    { cont = (fun k -> x.cont (fun k' -> k'.cont (fun a -> k a))) }
end

type a = A
type b = B
type c = C

let given_a = Cont.ret A
let a_to_b (_ : a) : b Cont.t = { cont = (fun k -> k B) }
let b_to_c (_ : b) : c Cont.t = { cont = (fun k -> k C) }
let taken_c (_ : c) : unit Cont.t = { cont = (fun k -> k ()) }
let run : unit Cont.t -> unit = fun k -> k.cont (fun () -> ())
let a_to_c a = Cont.bind (a_to_b a) b_to_c

let see_a a : a Cont.t =
  {
    cont =
      (fun k ->
        Fmt.pr "A";
        k a);
  }

let see_b b : b Cont.t =
  {
    cont =
      (fun k ->
        Fmt.pr "B";
        k b);
  }

let see_c c : c Cont.t =
  {
    cont =
      (fun k ->
        Fmt.pr "C";
        k c);
  }

let () = run @@ Cont.bind (Cont.bind (Cont.bind given_a a_to_b) b_to_c) taken_c

let () =
  run
  @@ Cont.bind
       (Cont.bind (Cont.bind (Cont.bind given_a a_to_b) b_to_c) see_c)
       taken_c

let ( let* ) = Cont.bind

let () =
  let task =
    let* a = given_a in
    let* b = a_to_b a in
    let* c = b_to_c b in
    taken_c c
  in
  run task

let () =
  let task =
    let* a = given_a in
    let* a' = see_a a in
    let* b = a_to_b a' in
    let* b' = see_b b in
    let* c = b_to_c b' in
    taken_c c
  in
  run task
