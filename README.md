# ...
`basic.ml`

`poly.ml`

`error.ml`

`ctx.ml`

`state.ml`

# Learning from Haskell

In `Haskell`'s `Preclude`, the section of [`Monads and functors`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#g:10) contains typeclasses `Functor` `Applicative` `Monad`. The section of [`Folds and traversals`] contains typeclasses `Foldable` `Traversable`.

```haskell
class Foldable t where
- Data structures that can be folded.

class (Functor t, Foldable t) => Traversable t where
- Functors representing data structures that can be traversed from left to right.
```

If `Fodable` has a feeling of `cata`, `Traversable` gives a hint to compute the data in a _sequential_ way, rather than to `fmap f` _equitably or simultaneously_.

`traverse.ml`
