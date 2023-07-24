open Core
open Effect
open Effect.Deep

type _ Effect.t += Divz : int Effect.t

let newdiv x y = match y with 0 -> perform Divz | _ -> Stdlib.(x / y)

let ( / ) n m =
  try_with
    (fun () -> newdiv n m)
    ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Divz ->
              Printf.printf "Div by 0, forcing return of 1\n";
              Out_channel.flush stdout;
              Some (fun (k : (a, _) continuation) -> continue k 1)
          | _ -> None);
    }
          ;;
let _ = (3 / 0) + (8 / 0) + 1
        ;;