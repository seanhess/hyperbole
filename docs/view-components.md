You may be tempted to use `HyperView`s to create reusable \"_Components_\". This leads to object-oriented designs that don't compose well. We are using a functional language; our main unit of reuse should be functions!

The `header` view function we defined above has a generic `context` that can be used in any view. This approach is great for reusing styles or layout. But what if we want to reuse interactivity? We can pass an `Action` into the view function as a parameter:

    #EMBED Example.Docs.Component styledButton

We can create more complex view functions by passing state in as a parameter. Here's a button that toggles between a checked and unchecked state for any `HyperView`:

    #EMBED Example.View.Inputs toggleCheckbox

    #EMBED Example.Docs.ViewFunctions toggler

