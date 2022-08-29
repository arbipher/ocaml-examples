let magic = 42

let%test _ = 
  magic = 42

let%expect_test _ =
  print_endline "Hello, world!";
  [%expect{| Hello, world! |}]