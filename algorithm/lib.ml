open Base
include Base.Fn

type nat = int

let (@:) = List.cons
let (@.) = Fn.compose
let id = Fn.id
let null = List.is_empty
let head = List.hd_exn
let tail = List.tl_exn
let init = List.drop_last_exn 
let last = List.last_exn
let reverse = List.rev
let rec tails = function
  | [] -> [[]]
  | x::xs -> (x::xs) :: tails xs
let inits xs = 
  let rec loop = function
    | [] -> [[]]
    | xs -> xs :: loop (init xs)
  in
  loop xs |> reverse
let length = List.length
let single = function
  | [_] -> true
  | _ -> false
let wrap x = [x]
let unwrap = function
  | [x] -> x
  | _ -> failwith "unwrap"
let foldr f init = List.fold_right ~f ~init 

let and_ = foldr (&&) true
let rec foldr1 f = function
  | [x] -> x
  | x::xs -> f x (foldr1 f xs)
  | [] -> failwith "foldr1"
let rec foldl1 f = function
  | [x] -> x
  | x::y::xs -> foldl1 f (f x y ::xs)
  | [] -> failwith "foldl1"
let rec foldrn f g xs =
  match xs with
  | [x] -> g x
  | x::xs -> f x (foldrn f g xs)
  | [] -> failwith "foldrn"
let take n xs = List.take xs n
let drop n xs = List.drop xs n

let concat = List.concat
let concat_map f = List.concat_map ~f

let smaller cmp f x y = 
  if cmp (f x) (f y) then x else y
let min_with_cmp cmp f =
  foldr1 (smaller cmp f)
let min_with f = 
  foldr1 (smaller (<=) f)
let max_with_cmp cmp f =
  foldr1 (fun x y -> smaller cmp f y x)
let max_with f =
  foldr1 (smaller (>=) f)


let sum = List.fold_right ~f:(+) ~init:0
let rec cp : 'a list list -> 'a list list = 
  function
  | [] -> [[]]
  | xs::xss -> 
    List.bind (cp xss) ~f:(fun ts -> 
        List.bind xs ~f:(fun x -> [x::ts])
      )
(* let cp : 'a list list -> 'a list list = 
   foldr (fun tss xs ->
      List.bind tss ~f:(fun ts ->
          List.bind xs ~f:(fun x ->
              [x::ts]
            ))
    ) [[]] *)
let filter f = List.filter ~f
let merge_by f xss = 
  let merge_two = List.merge ~compare:(fun x y -> if f x y then -1 else +1) in
  List.reduce_balanced_exn xss ~f:merge_two
let scanl : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b list =
  fun op e xs ->
  let f b a = op b a, op b a in
  e :: List.folding_map xs ~init:e ~f
let scanr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b list =
  fun op e xs ->
  let f b a = op a b in
  scanl f e xs |> reverse

let (@!!) = List.nth_exn
let (++) = (@)
let zip = List.zip_exn
let zip_with f = List.map2_exn ~f
let map f xs = List.map xs ~f
let rec fetch k xs =
  if k = 0 then
    head xs
  else
    fetch (k-1) (tail xs)

let rec until : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a =
  fun p f x ->
  if p x then x else until p f (f x)

let nrange l r =
  List.range ~stride:1 ~start:`inclusive ~stop:`inclusive l r

let (@..) l r = nrange l r
let (>>=) = List.(>>=) ;;

let range_downto l r =
  List.range ~stride:(-1) ~start:`inclusive ~stop:`inclusive l r

let thin_by : ('a -> 'a -> bool) -> 'a list -> 'a list =
  fun (@<=) ts ->
  let bump x = function
    | [] ->[x]
    | y::ys -> 
      if x @<= y then x::ys
      else if y @<= x then y::ys
      else x::y::ys
  in
  foldr bump [] ts

let prs s = s |> Sexp.to_string_hum |> Stdio.print_endline

let prll_sp = Fmt.(pr "@[%a@]" 
                     (list ~sep:(any "; ") 
                      @@ (any "[") ++ (list ~sep:sp int) ++ (any "]")))

let prll = Fmt.(pr "@[%a@]" 
                  (list ~sep:(any "; ") 
                   @@ (any "[") ++ (list ~sep:(any "; ") int) ++ (any "]")))

let prpll_sp = Fmt.(pr "@[%a@]" 
                      (list ~sep:(any "; ") 
                       @@ (any "[") ++ (pair ~sep:comma int ((any "[") ++ (list ~sep:sp int) ++ (any "]"))) ++ (any "]")))

let pr_list = Fmt.(pr "@[%a@]"
                     ((any "[")
                      ++ (list ~sep:(any "; ") int )
                      ++ (any "]"))
                  )

let pr_pair_list = Fmt.(pr "@[%a@]" 
                          (list ~sep:(any "; ") 
                           @@ (any "[") ++ (pair ~sep:comma int int) ++ (any "]")))

let pr_p3_list = Fmt.(pr "@[%a@]" 
                        (list ~sep:(any "; ") 
                         @@ (any "[") ++ (pair ~sep:comma int int) ++ (any "]")))
