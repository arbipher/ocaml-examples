(* module type AT = sig
     type t

     val v : t
   end

   module A : AT = struct
     type t = int

     let v : t = 3
   end

   module type BT = sig
     type t = int

     val v : t
   end

   module B : BT = struct
     (* include A *)
     type t = int

     let v = 4
   end

   let foo (a : A.t) : B.t = Obj.magic a
   let () = print_int (foo A.v) *)

(* --- *)

module type A = sig
  type t
end

module type B = sig
  type u
end

module type C = sig
  module X : A
  module Y : B

  type t
end

module F (Z : C with type X.t = int and type Y.u = bool and type t = int) =
struct
  module Z = Z
end

module Imp_A : A with type t = int = struct
  type t = int
end

module Imp_B : B with type u = bool = struct
  type u = bool
end

module Imp_C : C with type X.t = int and type Y.u = bool and type t = int =
struct
  module X = Imp_A
  module Y = Imp_B

  type t = int
end

module Imp_F = F (Imp_C)

(* --- *)
module type A2 = sig
  type t
end

module type B2 = sig
  type u
end

(*
module type C2 = sig
  module X2 : A2
  module Y2 : B2
end
with 
 type X2.t = Y2.u *)

module type A3 = sig
  type t
end

module type B3 = sig
  type u
end

(* module type C3 = sig
     module X3 : A3
     module Y3 : B3

     type v
   end
   with type v = X3.t
    and type v = Y3.u *)

module type J = sig
  type p
  type q

  val f : p -> q -> p * q
end

(* module G (H : J with type p = q) = struct
     module H = H
   end *)

module type A4 = sig
  type t
end

module type B4 = sig
  type u
end

module type C4 = sig
  module X4 : A4
  module Y4 : B4
end

module F4 (X4 : A4) (Y4 : B4 with type u = X4.t) = struct
  module Z4 : C4 = struct
    module X4 = X4
    module Y4 = Y4
  end
end
