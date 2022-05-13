(* Questions on cartesian products:
   1. Can cartesian be forgettable
   2. Does half-divide a chain help
    Or does associativity help e.g ((2 * 2) * 2) * 2 vs ((2 * 2) * (2 * 2))
    aside: the only _associative_ in Base is `List.reduce`

   3. What design of cache (accumulated computed value) helps
   4. What is a good way to evaluate the iteration
   (how do we use the cartesian product result? do we need the point-wise result?
   If no point-wise result needed, why do we do cartesian product?
   We can not use point-wise result finally, but use cartesian product intermediately
   to **merge** states eagerly
   )
   5. What is the difference and connections between c/p and bind?
   Normally a cartisian product is input-irrelated while bind is input-related
*)

module Left_to_right = struct
  type 'a t = 'a frame list
  and 'a frame = 'a list

  let s0 : 'a t = []
  let choice_a : 'a frame = [ "a1"; "a2" ]
  let sa : 'a t = choice_a :: s0
  let choice_b : 'a frame = [ "b1"; "b2"; "b3" ]
  let sb : 'a t = choice_b :: sa
end