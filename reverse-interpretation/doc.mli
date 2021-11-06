val (<|>) : 'a t -> 'a t -> 'a t
(** [p <|> q] runs [p] and returns the result if succeeds. If [p] fails, then
    the input will be reset and [q] will run instead. *)

val (<?>) : 'a t -> string -> 'a t
(** [p <?> name] associates [name] with the parser [p], which will be reported
    in the case of failure. *)

val commit : unit t
(** [commit] prevents backtracking beyond the current position of the input,
    allowing the manager of the input buffer to reuse the preceding bytes for
    other purposes. *)

(** {2 Monadic/Applicative interface} *)

val return : 'a -> 'a t
(** [return v] creates a parser that will always succeed and return [v] *)

val fail : string -> _ t
(** [fail msg] creates a parser that will always fail with the message [msg] *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** [p >>= f] creates a parser that will run [p], pass its result to [f], run
    the parser that [f] produces, and return its result. *)

val (>>|) : 'a t -> ('a -> 'b) -> 'b t
(** [p >>| f] creates a parser that will run [p], and if it succeeds with
    result [v], will return [f v] *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** [f <*> p] is equivalent to [f >>= fun f -> p >>| f]. *)

val (<$>) : ('a -> 'b) -> 'a t -> 'b t
(** [f <$> p] is equivalent to [p >>| f] *)

val ( *>) : _ t -> 'a t -> 'a t
(** [p *> q] runs [p], discards its result and then runs [q], and returns its
    result. *)

val (<* ) : 'a t -> _ t -> 'a t
(** [p <* q] runs [p], then runs [q], discards its result, and returns the
    result of [p]. *)

val lift : ('a -> 'b) -> 'a t -> 'b t
val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
(** The [liftn] family of functions promote functions to the parser monad.
    For any of these functions, the following equivalence holds:
{[liftn f p1 ... pn = f <$> p1 <*> ... <*> pn]}
    These functions are more efficient than using the applicative interface
    directly, mostly in terms of memory allocation but also in terms of speed.
    Prefer them over the applicative interface, even when the arity of the
    function to be lifted exceeds the maximum [n] for which there is an
    implementation for [liftn]. In other words, if [f] has an arity of [5] but
    only [lift4] is provided, do the following:
{[lift4 f m1 m2 m3 m4 <*> m5]}
    Even with the partial application, it will be more efficient than the
    applicative implementation. *)
