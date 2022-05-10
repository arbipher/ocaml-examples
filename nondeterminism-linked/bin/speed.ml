open Core
open Core_bench

let () =
  Command.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"List ver" (fun () ->
             ignore @@ Nondeter.MS_example_1.invariant_length 10);
         Bench.Test.create ~name:"Set ver" (fun () ->
             ignore @@ Nondeter.MSS_example.invariant_length 10);
       ])
