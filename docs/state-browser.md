The state `Action` threading and `ViewState` both live in on the web page itself, and are reset when the user navigates away or refreshes  the browser.

Using the `Hyperbole` effect, we can store state in the browser `Query` string. This is useful for faceted search, or any time a user might want to share a url and have the page load with local state changes.

    #EMBED Example.State.Query data Preferences
    #EMBED Example.State.Query instance Default Preferences

Access the current query (or the default) using `query`, or work directly with `param`s. Save changes using `modifyQuery`, `setQuery`, `setParam`, etc

State stored in the `Query` is page-wide, and survives refreshes or as long as the url is available.
