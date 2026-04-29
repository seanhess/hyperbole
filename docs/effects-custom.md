We can access a database using the `IOE` effect, but it is better to describe the high-level operations available to the application as a custom effect:

    #EMBED Example.Effects.Todos data Todos

To use it we need a `run` function which converts the abstract operations into a specific implementation. We could implement the Todos `Effect` using `Hyperbole` `Session`s.

[[/examples/todos]]

    #EMBED Example.Todos.Todo main

Alternatively, we could implement the effect using a database instead.

[[/examples/todosdb]]

    #EMBED Example.Effects.Database main

Whichever implementation we use, a `HyperView` or `Page` can interact with the data using the same high-level `Todos` effect by adding it as a constraint:

    {-# LANGUAGE UndecidableInstances #-}

    #EMBED Example.Todos.Todo simplePage

* [Effectful.Dynamic.Dispatch](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html) - Introduction to Effects
* [[/examples/todos]] - Implemented with Session
* [[/examples/todosdb]] - Implemented with SQLite / [Selda](https://valderman.github.io/selda/)
