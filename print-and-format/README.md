# Print and Format in OCaml

One of the notorious OCaml _feature_ for me is it's hard to print a list. In general, you could either use ppx tools to automatically generate the printing function(`to_string` in other languages) or use (standard) libraries to print something yourself.

## basic usage of ppx_show

`ppx_show` is a reimplementation of [`show`](https://github.com/ocaml-ppx/ppx_deriving#plugin-show) plugin from `ppx_deriving`. This ppx will generate `show_<t>` and `pp_<t>` functions for a type `t`. Noting if the id of the type is just `t`, the function will name `show` and `pp` directly. This may be OCaml convention that we usually define a `type t` in a module `M` as a representation, `M.show` is sufficient than `M.show_t`.

(Noting, in compiled projects, `ppx_show` behaves similar to `ppx_deriving.show`, however currently in toplevel e.g. `utop`, `ppx_show` may have some problems on OCaml version or I haven't figure it out. If you want to play with the code in utop, please use `ppx_deriving.show`)

```OCaml
utop # #require "ppx_deriving.show";;
```

```OCaml
utop # type t = A | B of int | C of bool * int | D of { a : int; b : string } [@@deriving show];;

type t = A | B of int | C of bool * int | D of { a : int; b : string; }
val pp : Format.formatter -> t -> unit = <fun>
val show : t -> string = <fun>

utop # type foo = Foo [@@deriving show];;
type foo = Foo
val pp_foo : Format.formatter -> foo -> unit = <fun>
val show_foo : foo -> string = <fun>
```

The signature for `show_foo` is straightforward, consuming a value of type `foo` then producing a `string`.

```OCaml
utop # show_foo Foo;;
- : string = "Foo"
```

The signature for `pp_foo` is a little obscure, consuming a `formatter` and a `foo`, then producing(return) a `unit`. A function returning `unit` usually hints it will have some side-effect, like `print_endline : string -> unit`. Let's talk about it later.

## print a list

We can come up three ways to print a list of `foo` since we have `show_foo`.
1. iter and print each element in a list
2. concatenate each string of element and print one longer string
3. define a type of `foo list` and gene

```OCaml
utop # List.iter (fun x -> print_string @@ show_foo x) [Foo;Foo];;
FooFoo- : unit = ()

utop # List.fold_left (fun acc ele -> acc ^ (show_foo ele)) "" [Foo;Foo];;
- : string = "FooFoo"
```

Either 1 and 2 are not smart since they are too long.

```OCaml
utop # type foolist = foo list [@@deriving show]
val pp_foolist : Format.formatter -> foolist -> unit = <fun>
val show_foolist : foolist -> string = <fun>

utop # show_foolist [Foo;Foo];;
- : string = "[Foo; Foo]"
```

`"[Foo; Foo]"` looks prettier than `"FooFoo"`. `Show`ing a `list` of `foo` is not the same as concatenating a list of `show`ing `foo`. Extra wraper `[` `]` and delimiter `;` makes it prettier.

## print a list of other module

For our own modules, adding `[@@deriving show]` is not a big deal. If we want to print a list of expressions of the type from other modules, it's not possible or good to modify their code to deriving `show`. A solution is to use `ppx_import`. As the document says: _import_ is a syntax extension that allows to pull in types or signatures from other compiled interface files.

`ppx_import` is doing syntax extension and it cannot import from local modules. See issue [#2](https://github.com/ocaml-ppx/ppx_import/issues/2)

```OCaml
(* magic.ml *)
module Magic = struct
  type t = Sparkling
end

(* lib.ml *)
type myt2 = [%import: Magic.t] [@@deriving show]

- val pp_myt2 : formatter -> myt2 -> unit
- val show_myt2 : myt2 -> string
;;
```

## Printf and Format

`Printf` and `Format` are standard libraries for printing. The signature of [Printf](https://caml.inria.fr/pub/docs/manual-ocaml/libref/type_Printf.html) just contains `*printf`. The signature of [Format]

```OCaml
(* In OCaml 4.08, Pervasive is renamed to Stdlib *)
(* module Printf *)
val printf :
    ('a, out_channel, unit) format -> 'a
val fprintf :
    Stdlib.out_channel -> ('a, Stdlib.out_channel, unit) Stdlib.format -> 'a

(* module Format *)
val printf :
    ('a, Format.formatter, unit) format -> 'a = <fun>
val fprintf :
    Format.formatter -> ('a, Format.formatter, unit) format -> 'a
```

In general, a `printf` specifies how(`format`) to print what(`'a`) to where(`out_channel`/`formatter`). `formatter` is a smart box to hold the content and insert indentation, break, and delimiters automatically.

`format` is implemented with GADT but we can think of it as a `string` with type hints for the following arguments.

```OCaml
utop # Printf.printf "%d" 1;;
- 1

utop # Printf.printf "%d";;
- : int -> unit = <fun>

utop # Printf.printf "%a";;
- : (out_channel -> '_weak2 -> unit) -> '_weak2 -> unit = <fun>

utop # Format.printf "%d";;
- : int -> unit = <fun>

utop # Format.printf "%a";;
- : (Format.formatter -> '_weak3 -> unit) -> '_weak3 -> unit = <fun>
```

`Printf.printf "%d" 1` finishes the printing. `Printf.printf "%d"` is a function taking an `int`.

`Printf.printf "%a"` is a little special. It takes two arguments: a printer of `(out_channel -> '_weak2 -> unit)` and a value of `'_weak2`. The value is _what_ we want to print. `%a` denotes a special type, unlike the primitive types int(`%d`) bool (`%B`) which OCaml knows _how_ to print them. You need to provide a printer for it.

`Format.printf "%a"` is similar. It takes a printer of `(Format.formatter -> '_weak3 -> unit)` and a value of `'_weak3`. The type of printer is compatible with `pp_*` in the early section.

```OCaml
utop # Format.printf "%a" pp_foo F;;
F- : unit = ()
```

Noting we cannot use `pp_foo` for `Printf.printf`.

```OCaml
utop # Printf.printf "%a" pp_foo F;;
Error: This expression has type formatter -> foo -> unit
       but an expression was expected of type out_channel -> 'a -> unit
       Type formatter is not compatible with type out_channel

utop # let p_foo oc foo = Printf.fprintf oc "%s" "My Foo";;
val p_foo : out_channel -> 'a -> unit = <fun>

utop # Printf.printf "%a" p_foo F;;
My Foo- : unit = ()

utop # p_foo;;
- : out_channel -> 'a -> unit = <fun>
```

This [tutorial](https://ocaml.org/learn/tutorials/format.html) covers some details of `Format`. The general idea is `Format` is a pretty version among `Printf`.

## Format.print a list

```OCaml
utop # open Format

utop # pp_print_list pp_foo std_formatter [F;F];;
F
F- : unit = ()

utop # printf "%a" (pp_print_list pp_foo) [F;F];;
F
F- : unit = ()
```

## on directive #install_printer

In toplevel (`utop`), we can register the printer for the expression evaluated in REPL via the directive `#install_printer`. The directive accept a `pp` function. It use this function to print an expression of such type.

```OCaml
utop # pp_foo;;
- : formatter -> foo -> unit = <fun>

utop # #install_printer pp_foo;;

utop # #install_printer p_foo;;
p_foo has a wrong type for a printing function.
```

Since OCaml infers the type of the `pp` function supported. It can be _ruined_ easily. e.g.

```OCaml
utop # let print_everything e = Format.print_string "everything";;
val print_everything : 'a -> unit = <fun>
utop # #install_printer print_everything;;

utop # 1;;
everything- : int =

utop # 2;;
everything- : int = 
```

## Fmt

[`Fmt`](https://github.com/dbuenzli/fmt) is a package for _Format pretty-printer combinators_.

The `Fmt.Dump` is to quickly format values for inspection.

```OCaml
pr "%a" (list pp_e) [Foo;Foo];;
pr "%a" (Dump.list pp_e) [Foo; Foo];;

let ppp fmt e = pr "%a" fmt e
```

## Dmp

[Dmp](https://github.com/mjambon/dum) is inspecting arbitrary OCaml values for debugging or just for fun.

```OCaml
$ utop -require dum
# Dum.to_stdout (123, "abc", Not_found, [`A; `B 'x']);;
(123 "abc" object-7 () [ 65 (66 120) ])
```

## Others

Toplevel must have a smart polymorphic `print_list`. How does it do?

Check this later:
https://stackoverflow.com/questions/7261248/how-can-ocaml-values-be-printed-outside-the-toplevel
https://stackoverflow.com/questions/20037967/how-do-i-printf-a-map-in-ocaml

_Surprisingly, structural printing is something that OCamlers rarely do._
http://alan.petitepomme.net/cwn/2008.07.08.html#4

http://gallium.inria.fr/~fpottier/pprint/

https://github.com/clarus/smart-print