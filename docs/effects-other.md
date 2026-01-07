If we want to use an `Effect` besides `Hyperbole`, add it as a constraint to any `Page` and `HyperView` that needs it

The following `HyperView` uses a `Reader` to get a message set at the application level. It also uses `Concurrent` to delay the response by 500ms:

    {-# LANGUAGE UndecidableInstances #-}
    
    #EMBED Example.Docs.SideEffects page

    #EMBED Example.Docs.SideEffects data SlowReader

    #EMBED Example.Docs.SideEffects instance (Concurrent

Then make sure to add the effect when you run your application

    #EMBED Example.Docs.SideEffects app
