Since we have more than one `Page`, we need a way to choose between them. You could create a manual function `Hyperbole :> es => Eff es Response` which reads the `Request`, and `runPage` different `Pages` depending on the `Path`, but Hyperbole comes with support for type-safe `Route`s:

    #EMBED Example.Docs.App data AppRoute

    #EMBED Example.Docs.App instance Route

```
>>> routeUri Main
"/"

>>> routeUri (User 3)
"/user/3"
```


Then, as the second argument to `liveApp`, you can pattern match on the `Route` to run various `Page`s using `routeRequest`. If your route has data in it, you can pass it to the corresponding page

    #EMBED Example.Docs.App router

    #EMBED Example.Docs.App app
