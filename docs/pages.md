
An app will usually have multiple 'Page's with different 'Route's that each map to a unique url path:

@
#EMBED Example/Docs/MultiPage.hs data AppRoute
@

When we create our app, we can add a router function which maps a 'Route' to a 'Page' with 'routeRequest'. The web page is completely reloaded each time you switch routes. Each 'Page' is completely isolated.

@
#EMBED Example/Docs/MultiPage.hs main
@

We can add type-safe links to other pages using 'route'

@
#EMBED Example/Docs/MultiPage.hs menu
@

If you need the same header or menu on all pages, use a view function:

@
#EMBED Example/Docs/MultiPage.hs layout

#EMBED Example/Docs/MultiPage.hs examplePage
@

As shown above, each 'Page' can contain multiple interactive 'HyperView's to add interactivity
