open Base
open Stdio

let create_number_file filename numbers =
  let outc = Out_channel.create filename in
  List.iter numbers ~f:(fun x -> Out_channel.fprintf outc "%d\n" x);
  Out_channel.close outc


let () = create_number_file "numbers.txt" [1;2;3;4;5];;
let s = In_channel.(with_file "../../while-src/succ.while" ~f:input_all)

let files_of_path path =
  let open Core in
  Sys.ls_dir  path
  |> List.map ~f:(fun s -> path ^ "/" ^ s)
  |> List.filter ~f:(fun s -> Sys.is_file_exn s)

let test_parse path =
  let test_one fpath =
    ignore
      (
        (* print_endline fpath; *)
        In_channel.(with_file fpath ~f:input_all)
        |> Parser.eval : Ast.program)
  in
  let all_files = files_of_path path in
  List.iter all_files ~f:test_one

let () = test_parse "../../while-src"

let magic = 42

let%test _ = 
  magic = 42

let%expect_test _ =
  print_endline "Hello, world!";
  [%expect{| Hello, world! |}]