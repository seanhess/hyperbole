We can nest smaller, more specific `HyperView`s inside of a larger parent. You might need this technique to display a list of items which also need to update themselves individually

To illustrate, let's enhance the previous example by creating a parent `HyperView` for the list of items, with an `Action` that can reset them

    #EMBED Example.Docs.Nesting data ItemList

Embed the parent `HyperView` into the page

    #EMBED Example.Docs.Nesting page

And embed the individual Item `HyperView`s into the Item `View`

    #EMBED Example.Docs.Nesting itemList

Add any nested `HyperView`s to `Require` to make sure they are handled. The compiler will let you know if you forget

    #EMBED Example.Docs.Nesting instance HyperView ItemList
