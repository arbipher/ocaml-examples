let pow2 n = List.init n (fun _ -> 2) |> List.fold_left (fun acc s -> acc * s) 1
let rec loop n f s = if n = 0 then s else loop (n - 1) f (f s)

module type Monad_S = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Poor_monad_S = sig
  type a
  type t

  val return : a -> t
  val bind : t -> (a -> t) -> t
end

module Int_set = Set.Make (struct
  type t = int

  let compare = Int.compare
end)

let pp_int_set oc s = Fmt.(Dump.seq Fmt.int) oc (Int_set.to_seq s)

let mutation deltas s =
  Int_set.fold
    (fun e s -> List.fold_left (fun s i -> Int_set.add (e + i) s) s deltas)
    s s

module Set_set = Set.Make (struct
  type t = Int_set.t

  let compare = Int_set.compare
end)

let pp_set_set oc ss = Fmt.(Dump.seq pp_int_set) oc (Set_set.to_seq ss)