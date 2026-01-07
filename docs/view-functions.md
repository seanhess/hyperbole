We showed in [[basics]] how we can factor `View`s into functions. It's best practice to have a main `View` function for each `HyperView`. Create views as pure functions of input data and state:

    inputs -> View viewId ()

We can write multiple view functions with our `HyperView` as the `context`, and factor them however is most convenient:

    #EMBED Example.Docs.ViewFunctions ^messageButton

Generic `View` functions can be used in any `context`:

    #EMBED Example.Docs.ViewFunctions ^header

Now that we have created multiple smaller view functions, we can refactor our main `View` function to use them and avoid repeating ourselves

    #EMBED Example.Docs.ViewFunctions messageView

