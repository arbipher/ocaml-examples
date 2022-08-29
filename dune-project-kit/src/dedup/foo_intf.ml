(* Reference https://www.craigfe.io/posts/the-intf-trick *)

(* Type definitions go here: *)

module type S_in = sig
  type t
end

module type S = sig
  type arg
end

type error = [ `Msg of string | `Code of int ]

(* The interface of [foo.ml]: *)

module type Intf = sig
  type error
  (** [error] is the type of errors returned by {!S}. *)

  module type S = S

  module Make (A : S_in) : S with type arg = A.t
end