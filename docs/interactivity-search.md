We've used `button` to trigger a `Action` in quite a few examples. Some actions require custom input from the user. For example `search` allows the user to freely input text. Notice that instead of a full action, it requires a partially applied action constructor `Text -> Action id`, and a delay in ms before sending changes to the server.

    #EMBED Example.Interactivity.Search instance HyperView CustomText

    #EMBED Example.Interactivity.Search customText

