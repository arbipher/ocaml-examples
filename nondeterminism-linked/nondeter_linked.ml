open Common

module Nondeter_in_linked_list : Monad_S = struct
  type 'a t = Leaf | Node of 'a node list
  and 'a node = { mutable next : 'a t ref; v : 'a }

  let _dummy = Node []
  let return v = Node [ { next = ref Leaf; v } ]

  let bind (ma : 'a t) (f : 'a -> 'b t) =
    match ma with
    | Leaf -> Leaf
    | Node es ->
        Node
          (List.map
             (fun (n : 'a node) ->
               let (b : 'b t) = f n.v in
               { next = ref b; v = n.v })
             es)
end
