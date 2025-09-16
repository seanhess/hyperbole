console.log("Custom JS!")

window.onload = function() {
  let boxes = Hyperbole.hyperView("Boxes")
  console.log("Found HyperView 'Boxes'")

  boxes.addEventListener("mouseover", function(e) {
    if (e.target.classList.contains("box")) {
      let action = Hyperbole.action("Selected", parseInt(e.target.innerHTML))
      boxes.runAction(action)
    }
  })
  boxes.addEventListener("mouseout", function(e) {
    if (e.target.classList.contains("box")) {
      boxes.runAction("Clear")
    }
  })

  listenServerEvents()
}

function listenServerEvents() {
  // you can listen on document instead, the event will bubble
  Hyperbole.hyperView("Message").addEventListener("server-message", function(e) {
    alert("Server Message: " + e.detail)
  })
}
