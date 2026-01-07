We could run a database using the `IOE` effect, but it is better to describe the high-level operations available to the application as a custom effect:

    #EMBED Example.Effects.Todos data Todos

    #EMBED Example.Effects.Todos loadAll


Just like built-in effects, we add it to any `HyperView` and `Page` that needs it as a constraint.

    {-# LANGUAGE UndecidableInstances #-}

    #EMBED Example.Todos.Todo simplePage

We run a custom effect in our Application just like any other. The [[/examples/todos]] example implements the Todos `Effect` using `Hyperbole` `session`s, but you could write a different runner that connects to a database instead.

    #EMBED Example.Todos.Todo main

Implementing a database runner as a custom `Effect` is beyond the scope of this documentation, but see the following:

* [Effectful.Dynamic.Dispatch](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html) - Introduction to Effects
* [NSO.Data.Datasets](https://github.com/DKISTDC/level2/blob/main/src/NSO/Data/Datasets.hs) - Production Data Effect with a database runner
* [Effectful.Rel8](https://github.com/DKISTDC/level2/blob/main/types/src/Effectful/Rel8.hs) - Effect for the [Rel8](https://hackage.haskell.org/package/rel8) Postgres Library
