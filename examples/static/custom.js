console.log("Custom JS")

// Wait for load
window.onload = function() {
  // lookup the hyperview
  if (!document.getElementById("Message2")) return
  let message2 = Hyperbole.hyperView("Message2")

  if (message2) {
    // call runAction() on the HyperView
    message2.runAction(Hyperbole.action('Louder', ["asdf"]))

    // Alternatively, call runAction() on window.Hyperbole
    setTimeout(() => {
      Hyperbole.runAction(message2, Hyperbole.action('Reset', ["reset"]))
    }, 2000)
  }
}
