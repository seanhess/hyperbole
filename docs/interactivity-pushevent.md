## PushEvent

The server can push a Javascript event to be dispatched on a `HyperView`

    #EMBED Example.Javascript update AlertMe

```
function listenServerEvents() {
  // you can listen on document instead, the event will bubble
  Hyperbole.hyperView("Message").addEventListener("server-message", function(e) {
    alert("Server Message: " + e.detail)
  })
}
```
