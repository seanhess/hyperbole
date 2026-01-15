The second argument of `liveApp` is an `Effect` monad which returns a `Response`. We will rarely return a `Response` directly, so we use the `runPage` function to turn `Page es '[...]` into an `Eff es Response`

The idea is that an application is divided into _independent_ `Page`s, which completely reload when you navigate between them. This is a deliberate choice to simplify development. In Single Page Applications, internal navigation often causes all sorts of strange state synchronization issues.

We suggest you create a module for each `Page`, each with its own `page` function

    #EMBED Example.Docs.MultiView page
