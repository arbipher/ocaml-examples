(* Fetch module types and type definitions from the [_intf] file *)
include Foo_intf

(* : S with type arg = A.t  *)
module Make (A : S_in) = struct
  type arg = A.t
end
