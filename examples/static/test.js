console.log('test.js')


window.addEventListener('load', function() {

  let other = Hyperbole.hyperView("Other")
  document.addEventListener("hello", function(e) {
    console.log("got event", e.type, e.detail, e)
    other.runAction("Sneaky")
  })
})
