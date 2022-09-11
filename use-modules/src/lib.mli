val magic : int

module type Input = sig
  type t

  val dump : t -> unit
end

module type Output = sig
  type t

  val dump_twice : t -> unit
end

module Make (I : Input) : Output with type t = I.t
