We can embed more than one of the same `HyperView` as long as the _value_ of `ViewId` is unique. Let's update `Message` to allow for more than one value:

    #EMBED Example.Simple data Message

Now we can embed multiple `Message` `HyperView`s into the same `Page`. Each will update independently

    #EMBED Example.Simple page

<!-- TODO CHANG EME -->

This is especially useful if we put identifying information in our `ViewId`, such as a database id. The `viewId` function can give us access to that info:

    #EMBED Example.DataLists.LoadMore data Languages

    #EMBED Example.DataLists.LoadMore instance HyperView Languages
