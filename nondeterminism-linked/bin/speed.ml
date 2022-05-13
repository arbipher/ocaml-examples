open Core
open Core_bench
open Nondeter

let () =
  let open Associativity in
  Command.run
    (Bench.make_command
       [
         (* Bench.Test.create ~name:"List in set" (fun () ->
                ignore @@ In_set.MS_example_1.invariant_length 10);
            Bench.Test.create ~name:"Set in set" (fun () ->
                ignore @@ In_set.MSS_example.invariant_length 10);
            Bench.Test.create ~name:"Linked List" (fun () ->
                ignore @@ In_linked.MSLL.invariant_length 10); *)

         (* sum unrolling *)
         (* Bench.Test.create ~name:"Sum" (fun () -> ignore @@ sum 1 4000);
            Bench.Test.create ~name:"Sum in 2" (fun () ->
                ignore @@ sum_in_2 sum 4000);
            Bench.Test.create ~name:"Sum in 4" (fun () ->
                ignore @@ sum_in_4 sum 4000);
            Bench.Test.create ~name:"Sum rec" (fun () -> ignore @@ sum_rec 1 4000);
            Bench.Test.create ~name:"Sum rec in 2" (fun () ->
                ignore @@ sum_in_2 sum_rec 4000);
            Bench.Test.create ~name:"Sum rec in 4" (fun () ->
                ignore @@ sum_in_4 sum_rec 4000);
            Bench.Test.create ~name:"Sum acc" (fun () -> ignore @@ sum_acc 1 4000);
            Bench.Test.create ~name:"Sum acc in 2" (fun () ->
                ignore @@ sum_in_2 sum_acc 4000);
            Bench.Test.create ~name:"Sum acc in 4" (fun () ->
                ignore @@ sum_in_4 sum_acc 4000) *)
         (*  *)
         Bench.Test.create ~name:"List level 22" (fun () ->
             ignore @@ list_make_to touch 22);
         Bench.Test.create ~name:"List level 23" (fun () ->
             ignore @@ list_make_to touch 23);
         Bench.Test.create ~name:"List level 24" (fun () ->
             ignore @@ list_make_to touch 24);
         Bench.Test.create ~name:"List level 25" (fun () ->
             ignore @@ list_make_to touch 25)
         (* Bench.Test.create ~name:"List level 30" (fun () ->
             ignore @@ make_to touch 30); *);
       ])
