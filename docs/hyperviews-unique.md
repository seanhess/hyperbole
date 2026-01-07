`ViewId` values must be unique. So if we want more than one of the same `HyperView` on the same `Page`, we need a way to differentiate them. In the example above we used two distinct constructors for Message, but we could also use a product type:

    #EMBED Example.Docs.UniqueViewId data Item

This is especially useful if we put identifying information in our `ViewId`, such as a database id. When we embed an item using `hyper`, we use a unique `ViewId` generated from that id

    #EMBED Example.Docs.UniqueViewId page

The `viewId` function can then give us access to said identifier in `update` or a `View`

    #EMBED Example.Docs.UniqueViewId instance HyperView

    #EMBED Example.Docs.UniqueViewId itemUnloaded

The `ViewId` is constant for the lifetime of the `HyperView`, so it won't work to try to cram state into it. Instead, use one of the approaches outlined in [[/state]]
