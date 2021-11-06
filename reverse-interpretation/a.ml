module More = struct
  type t =
    | Complete
    | Incomplete
end

module Input = struct
  type t = string
  let dummy : t = ""
  let client_committed_bytes (_ : t) : int = 0
  let parser_committed_bytes (_ : t) : int = 0
end

type 'a state =
  | Partial of 'a partial
  | Done    of int * 'a
  | Fail    of int * string list * string
and 'a partial =
  { committed : int
  ; continue  : string -> off:int -> len:int -> More.t -> 'a state }

type 'a with_state = Input.t ->  int -> More.t -> 'a

type 'a failure = (string list -> string -> 'a state) with_state
type ('a, 'r) success = ('a -> 'r state) with_state

type 'a t =
  { run : 'r. ('r failure -> ('a, 'r) success -> 'r state) with_state }

let fail_k    input pos _ marks msg = Fail(pos - Input.client_committed_bytes input, marks, msg)
let succeed_k input pos _       v   = Done(pos - Input.client_committed_bytes input, v)

let fail_to_string marks err =
  String.concat " > " marks ^ ": " ^ err

let state_to_option = function
  | Done(_, v) -> Some v
  | _          -> None

let state_to_result = function
  | Done(_, v)          -> Ok v
  | Partial _           -> Error "incomplete input"
  | Fail(_, marks, err) -> Error (fail_to_string marks err)

let parse p =
  p.run (Input.dummy) 0 Incomplete fail_k succeed_k

module Monad = struct
  let return v =
    { run = fun input pos more _fail succ ->
          succ input pos more v
    }

  let (>>=) p f =
    { run = fun input pos more fail succ ->
          let succ' input' pos' more' v = (f v).run input' pos' more' fail succ in
          p.run input pos more fail succ'
    }

  let (>>|) p f =
    { run = fun input pos more fail succ ->
          let succ' input' pos' more' v = succ input' pos' more' (f v) in
          p.run input pos more fail succ'
    }
end

module Choice = struct
  let (<?>) p mark =
    { run = fun input pos more fail succ ->
          let fail' input' pos' more' marks msg =
            fail input' pos' more' (mark::marks) msg in
          p.run input pos more fail' succ
    }

  let (<|>) p q =
    { run = fun input pos more fail succ ->
          let fail' input' pos' more' marks msg =
            (* The only two constructors that introduce new failure continuations are
             * [<?>] and [<|>]. If the initial input position is less than the length
             * of the committed input, then calling the failure continuation will
             * have the effect of unwinding all choices and collecting marks along
             * the way. *)
            if pos < Input.parser_committed_bytes input' then
              fail input' pos' more marks msg
            else
              q.run input' pos more' fail succ in
          p.run input pos more fail' succ
    }
end