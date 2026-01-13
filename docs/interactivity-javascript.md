Include custom js on a page with the script tag on only the page where it is needed, or globally via your toDocument function

    #EMBED Example.Javascript page


```
let boxes = Hyperbole.hyperView("JBoxes")
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
```

## RunAction

JS can call the server via an API attached to `window.Hyperbole`. Here we re-implement mouseover boxes from the events example using Javascript
