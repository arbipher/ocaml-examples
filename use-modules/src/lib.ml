let magic = 42

module type Input = sig
  type t

  val dump : t -> unit
end

module type Output = sig
  type t

  val dump_twice : t -> unit
end

module Make (I : Input) : Output with type t = I.t = struct
  type t = I.t

  let dump_twice v =
    I.dump v ;
    I.dump v
end
