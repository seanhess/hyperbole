// This isn't magic. If you want custom behavior, copy and modify this however you like. 
//
// As with any custom js, add to a single page via the `script` combinator
//  page = do
//     pure $ do
//        el "This is my page"
//        script "custom.js"
//
// or to the entire app by adding a script tag to your document function. See Example.App.toDocument
//
// Consider conditionally adding it based on ENV
console.log("Live Reload enabled")

function showNotification(message) {
  const notification = document.createElement('div');
  {
    const style = notification.style;
    style.position = 'fixed';
    style.bottom = '15px';
    style.left = '15px';
    style.right = '15px';
    // style.left = '50%';
    style.backgroundColor = 'rgba(240, 0, 0, 0.9)';
    style.color = '#fff';
    style.padding = '30px';
    style.borderRadius = '3px';
    style.zIndex = '1000';
  }
  notification.innerHTML = message;
  document.body.appendChild(notification);
}

document.addEventListener("hyp-socket-disconnect", () => {
  showNotification("<div style='font-weight:bold'>DISCONNECTED</div><div>will reload on reconnect</div>")

})

document.addEventListener("hyp-socket-reconnect", () => {
  setTimeout(() => {
    location.reload()
  }, 0)
})
