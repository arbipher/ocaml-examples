module type Mon = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* a reader monad with unit as the environment *)
(* it's also an applicative *)
module Function_monad : Mon with type 'a t = unit -> 'a = struct
  type 'a t = unit -> 'a

  let return x () = x

  let bind ma f () =
    let a = ma () in
    let mb = f a in
    let b = mb () in
    b
end

module Writer_monad : Mon with type 'a t = 'a * string = struct
  type 'a t = 'a * string

  let return x = (x, "")

  let bind ma f =
    let a, s1 = ma in
    let b, s2 = f a in
    (b, s1 ^ s2)
end

let%expect_test _ =
  let open Writer_monad in
  let mr =
    bind
      (bind (return 1) (fun x -> (true, string_of_int x)))
      (fun b -> ("end", string_of_bool b))
  in
  let s, r = mr in
  print_string s ;
  print_string r ;
  [%expect {| end1true |}]

module type Applicative = sig
  type 'a t

  val pure : 'a -> 'a t

  (* val ap : ('a -> 'b) t -> 'a t -> 'b t *)
  val product : 'a t -> 'b t -> ('a * 'b) t
  (* val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t *)
end

module Side_effect_applicative : Applicative with type 'a t = 'a -> unit =
struct
  type 'a t = 'a -> unit

  let pure _ _ = ()

  (* let ap f ma = *)

  let product fa fb ab =
    let a, b = ab in
    fa a ;
    fb b ;
    ()
  (*
           let lift2 f2 sa sb c =
             sa a ;
             sb b ;
             f2 a b *)
end

(* module Side_effect_applicative :
     Applicative with type 'a t = ('a -> unit) -> unit = struct
     type 'a t = ('a -> unit) -> unit

     let pure a f = f a

     let product fa fb fab =
       (* let a, b = ab in
          fa a ;
          fb b ; *)
       ()
     (*
        let lift2 f2 sa sb c =
          sa a ;
          sb b ;
          f2 a b *)
   end *)

module type Contra = sig
  type 'a t

  val contramap : ('a -> 'b) -> 'b t -> 'a t
end

module Iter : Contra with type 'a t = 'a -> unit = struct
  type 'a t = 'a -> unit

  let contramap f mb a = mb (f a)
end

let%expect_test _ =
  let open Iter in
  let f_int i = print_int i in
  let g = contramap String.length f_int in
  let r = g "foobar" in
  [%expect {| 6 |}]

module type Divisible = sig
  type 'a t

  val contramap : ('a -> 'b) -> 'b t -> 'a t
  val divide : ('a -> 'b * 'c) -> 'b t -> 'c t -> 'a t
  val conquer : 'a t
end

module Iter_more : Divisible with type 'a t = 'a -> unit = struct
  type 'a t = 'a -> unit

  let contramap f mb a = mb (f a)

  let divide unpair fb fc bc =
    let b, c = unpair bc in
    fb b ;
    fc c ;
    ()

  let conquer _ = ()
end

let%expect_test _ =
  let open Iter_more in
  let f_int i = print_int i in
  let f_string i = print_string i in
  let g =
    divide
      (fun ab ->
        let a, b = ab in
        (a, b))
      f_int f_string
  in
  g (1, "2") ;
  [%expect {| 12 |}]