open Effect
open Effect.Deep

type _ Effect.t += One: int t | Jump : int -> int t

let perform_get x = match x with 
  | 1 -> (perform One) + 10
  | _ -> perform (Jump 5)

let simple () =
  Printf.printf "\n";
  Printf.printf "begin\n";
  let b1 = perform_get 1 in
  Printf.printf "b1 = %d\n" b1;
  let b2 = perform_get 5 in
  Printf.printf "b2 = %d\n" b2;
  let ans = b1 + b2 + 50 in
  Printf.printf "simple: ans = %d\n" ans;
  1000 + ans

let run f =
  try_with f ()
    { effc = fun (type a) (eff: a t) -> match eff with
      | One -> Some (fun (k: (a, _) continuation) ->
          Printf.printf "one: before kont\n";
          let this = (continue k 1) in
          Printf.printf "one: after kont: %d\n" this;
          let later = this + 100 in
          later)
      | Jump x -> Some (fun (k: (a, _) continuation) ->
        Printf.printf "jump: before kont\n";
        let this = (continue k x) in
        Printf.printf "jump: after kont: %d\n" this;
        let later = this + 200 in
        later)
      | _ -> None } 

let () = 
  let ans = run simple in
  Printf.printf "outer: ans = %d\n" ans