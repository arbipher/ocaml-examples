let%expect_test _ =
  print_endline "Hello, world!" ;
  [%expect {| Hello, world! |}]

module type Monad_T = sig
  type t
end

module type Monad_S = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Nondeter_in_list (T : Monad_T) : Monad_S with type 'a t = 'a list =
struct
  type 'a t = 'a list

  let return e = [ e ]
  let bind ma f = List.map f ma |> List.concat
end

(* test *)

module ML = struct
  include Nondeter_in_list (struct
    type t = int
  end)

  let fork s = [ s; s ]
end

let%test _ =
  let open ML in
  let s0 = [ 1; 2; 3 ] in
  let s1 = bind s0 fork in
  assert (List.length s1 = 6) ;
  let s2 = bind s1 fork in
  assert (List.length s2 = 12) ;
  true