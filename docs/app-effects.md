Your application will want to support various side effects. It's helpful to create a single function that runs all shared effects:

    #EMBED Example.Docs.App runApp

Add all your effects by calling your runner:

    #EMBED Example.Docs.App app'

A few pages might use effects that the rest don't need, or a specific implementation. You can choose to run an effect for a single page

    #EMBED Example.Docs.App router'

