Hyperbole relies heavily on [Effectful](https://hackage.haskell.org/package/effectful) to run and compose side effects. We can use these `Effect`s in any `Page` or `update`.

The `Hyperbole` effect is automatically available in both, and gives us direct access to the client connection. We can use it to get information about the `request`, update the page directly, and more. Here is how you might use it to set the page title.

    #EMBED Example.Docs.SideEffects data Titler
 
    #EMBED Example.Docs.SideEffects instance HyperView Titler

For more information on the `Hyperbole` `Effect`, see [[/hyperboleeffect]]

