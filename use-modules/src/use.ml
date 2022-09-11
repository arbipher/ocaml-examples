(* Make a static stately module *)

module IntOutput = Lib.Make (struct
  type t = int

  let dump i = print_int i
end)

(* Bad: use mutable variable as state.
   but, how can we retreive it?
*)

module StatefulIntOutput = Lib.Make (struct
  type t = int

  let counter = ref 0

  let dump i =
    counter := !counter + 1 ;
    print_int i
end)

(* Bad: use a global mutable varaible as state *)

module IntStateWrapper = struct
  let counter_int = ref 0

  module StatefulIntOutput = Lib.Make (struct
    type t = int

    let counter = counter_int

    let dump i =
      counter := !counter + 1 ;
      print_int i
  end)
end

(* Now making another one for another Make result *)

module StringStateWrapper = struct
  let counter_string = ref 0

  module GlobalStatefulStringOutput = Lib.Make (struct
    type t = string

    let counter = counter_string

    let dump i =
      counter := !counter + 1 ;
      print_string i
  end)
end

(* Make a outer Make *)