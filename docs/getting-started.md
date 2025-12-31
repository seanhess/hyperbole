Hyperbole applications run via [Warp](https://hackage.haskell.org/package/warp) and [WAI](https://hackage.haskell.org/package/wai)

They are divided into top-level `Page`s, which may run side effects (such as loading data from a database), then respond with an HTML `View`. The following application has a single `Page` that displays a static "Hello World"
