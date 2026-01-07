Let's get interactive! Using `Hyperbole`, we divide our `Page` into independent live subsections called `HyperView`s

To start, define a data type (a `ViewId`) that uniquely identifies some sections of the page:

    #EMBED Example.Simple data Message

Then make our `ViewId` an instance of `HyperView`:

* Create an `Action` type with a constructor for every possible way that the user can interact with it
* Write an `update` for each `Action`

```
#EMBED Example.Simple instance HyperView Message
```

If an `Action` occurs, the contents of our `HyperView` will be replaced with the result of `update`.

To use our new `HyperView`, add the `ViewId` to the type-level list of `Page`, and then embed it in the page view with `hyper`

    #EMBED Example.Docs.Interactive page

Now let's use a button to trigger our `Action`. Instead of using a generic `context` in the `View` type signature, we must set it to our `ViewId`. The compiler will tell us if we try to trigger actions that don't belong to our `HyperView`

    #EMBED Example.Simple messageView

When the user clicks the button, the contents of `hyper` will be replaced with the result of `update`, leaving the rest of the page untouched.
