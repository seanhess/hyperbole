Likewise we can store state in a browser cookie using `Session`. This is useful for user preferences, login state, and any time you want client-specific state to persist across navigation and refreshes

    #EMBED Example.State.Sessions data Preferences
    #EMBED Example.State.Sessions instance Default Preferences


Access the current session (or the default) using `session`, or manipulate it with `saveSession`, `deleteSession`, etc

State stored in the `Session` is page-specific by default, but can be configured to be application-wide. It survives until manually cleared by the user or the application. 
