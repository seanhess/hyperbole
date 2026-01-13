For any real application, most persistent state will need to use a separate `Effect`, like a database. In [[/sideeffects]] we demonstrated how to use a custom effect to wrap a database.

Another way to store application-wide state is to use the `Concurrent` effect. It gives us the ability to work with `TVar`s, `MVar`s and `STM`. Here is another counter, implemented using `Reader (TVar Int)`

    #EMBED Example.State.Effects page

    #EMBED Example.State.Effects getCount

Notice how this state is shared among all users application-wide, and survives until the app restarts
