`Hyperbole` can manage action-threaded state automatically by setting `ViewState` in your `ViewId`:

    #EMBED Example.State.ViewState data Counter
    #EMBED Example.State.ViewState instance ViewId

Instead of `hyper`, use `hyperState` to embed the `HyperView` with a starting state:

    #EMBED Example.State.ViewState page

`State (ViewState viewId) :> es`  is already included in `update`, and can be accessed with familiar functions like `get` `put` and `modify` from [Effectful.State.Dynamic](https://hackage-content.haskell.org/package/effectful-core/docs/Effectful-State-Dynamic.html)

    #EMBED Example.State.ViewState instance HyperView Counter

To read the state in `View` use the `viewState` function

    #EMBED Example.State.ViewState viewCount
