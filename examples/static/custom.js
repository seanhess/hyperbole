console.log("Custom JS 2!")


// Wait for load
window.onload = function() {
  let boxes = Hyperbole.hyperView("Boxes")
  // document.addEventListener("mouseover", function(e) {
  //   //   if (e.target.classList.contains("box")) {
  //   //     let selected = Hyperbole.action("Selected", parseInt(e.target.innerHTML))
  //   //     boxes.runAction(selected)
  //   //   }
  // })
  // document.addEventListener("mouseout", function(e) {
  //   //   if (e.target.classList.contains("box")) {
  //   //     boxes.runAction("Clear")
  //   //   }
  // })
  // document.addEventListener("hyp-content", function(e) {
  //   // console.log("HYP CONTENT", e.target)
  //   // document.querySelectorAll("box").forEach(node => {
  //   //   node.
  //   // })
  // })

  // // lookup the hyperview
  // let message2 = Hyperbole.hyperView("Message2")
  //
  // if (message2) {
  //   // call runAction() on the HyperView
  //   message2.runAction(Hyperbole.action('Louder', "asdf"))
  //
  //   // Alternatively, call runAction() on window.Hyperbole
  //   setTimeout(() => {
  //     Hyperbole.runAction(message2, Hyperbole.action('Reset', "reset"))
  //   }, 2000)
  // }
}
