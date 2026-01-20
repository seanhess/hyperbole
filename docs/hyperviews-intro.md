Let's get interactive! Using `Hyperbole`, we divide our `Page` into independent live subsections called `HyperView`s

To start, define a data type that uniquely identifies an interactive section of the page. Make it an instance of `ViewId`. We will call this datatype a `ViewId`

    #EMBED Example.Simple data Message

Next we make the `ViewId` an instance of `HyperView`:

* Create an `Action` type with a constructor for every possible way that the user can interact with it
* Write an `update` for each `Action`

```
#EMBED Example.Simple instance HyperView Message
```

If an `Action` occurs, the contents of our `HyperView` will be replaced with the result of `update`.

Choose where the new `HyperView` will appear on the page using the `hyper` function, and add the `ViewId` type to the type-level list of `Page`

    #EMBED Example.Docs.Interactive page

Finally, let's create a `View` with a button that triggers our `Action`. Instead of using a generic `context` in the `View` type signature, we must set it to our `ViewId`. The compiler will tell us if we try to trigger actions that don't belong to our `HyperView` instance

    #EMBED Example.Simple messageView

When the user clicks the button, the contents of `hyper` will be replaced with the result of `update`, leaving the rest of the page untouched.
