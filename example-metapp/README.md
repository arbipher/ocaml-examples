# understanding

`[%meta ...]` requires a `Parsetree.expression`.

`[%e ...]` is used inside `[%meta ]`, e.g. `[%meta [%e ...]]`, to convert an OCaml expression to a `Parsetree.expression`.

`[%p? pa]` converts `pa` to `Ppxlib.pattern`.

`[%%metapackage]` supports `base`, but not `fmt` (why?)

`[%stri]` makes a `Parsetree.structure_item`.

`[%str]` should make a `Parsetree.structure_item list`, but I haven't figured it out.

## reference
https://github.com/mjambon/dune-starter
https://github.com/github/gitignore