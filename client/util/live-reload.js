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
  notification.classList.add("live-reload")
  notification.innerHTML = message;
  jackIn(notification.style)
  notification.addEventListener('click', function() {
    notification.remove()
  })
  document.body.appendChild(notification);
}

document.addEventListener("hyp-socket-disconnect", () => {
  showNotification("DISCONNECTED - will reload on reconnect")
})

document.addEventListener("hyp-socket-reconnect", () => {
  setTimeout(() => {
    location.reload()
  }, 0)
})


// duplicate cyber style stuff here so the default live reload is fun
function jackIn(style) {
  style.position = 'fixed';
  style.bottom = '15px';
  style.left = '15px';
  style.right = '15px';
  style.backgroundColor = 'rgba(160, 63, 56, 1.0)';
  style.color = '#fff';
  style.borderTop = 'solid #EC6458 4px';
  style.padding = '15px';
  style.zIndex = '1000';
  style.clipPath = 'polygon(0 0, 100% 0, 100% calc(100% - 16px), calc(100% - 16px) 100%, 0 100%)';
}
