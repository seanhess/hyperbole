In [[/basics]] we showed how to run a simple application:

    #EMBED Example.Docs.BasicPage main

 Let's go over the arguments of `liveApp`, starting with the `Document` function

    liveApp
      :: (BL.ByteString -> BL.ByteString)
      -> Eff '[Hyperbole, Concurrent, IOE] Response
      -> Wai.Application
    liveApp = _
