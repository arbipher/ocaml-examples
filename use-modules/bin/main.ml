open Mylib

let _ =
  print_endline "\nNormally making a module:" ;
  Use.IntOutput.dump_twice 3 ;
  print_endline "\n\nUse a stateful module, but silent to the state:" ;
  Use.StatefulIntOutput.dump_twice 4 ;
  Use.StatefulIntOutput.dump_twice 4 ;
  print_endline "\n\nUse a stateful module" ;
  Use.IntStateWrapper.StatefulIntOutput.dump_twice 5 ;
  print_endline "\n\nCheck this counter" ;
  print_int !Use.IntStateWrapper.counter_int ;
  print_endline "\n\nUse another stateful module" ;
  Use.StringStateWrapper.GlobalStatefulStringOutput.dump_twice "aha" ;
  Use.StringStateWrapper.GlobalStatefulStringOutput.dump_twice "aha" ;
  print_endline "\n\nCheck another counter" ;
  print_int !Use.StringStateWrapper.counter_string ;
  ()
