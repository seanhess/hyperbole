
import * as debounce from 'debounce'

export type UrlFragment = string

export function listenClick(cb:(target:HTMLElement, action:string) => void): void {
  document.addEventListener("click", function(e) {
    let el = e.target as HTMLInputElement

    // clicks can fire on internal elements. Find the parent with a click handler
    let source = el.closest("[data-on-click]") as HTMLElement

    // console.log("CLICK", source?.dataset.onClick)

    // they should all have an action and target
    if (source?.dataset.onClick && source?.dataset.target) {
      e.preventDefault()

      let target = document.getElementById(source.dataset.target)

      if (!target) {
        console.error("Missing target: ", source.dataset.target)
        return
      }

      cb(target, source.dataset.onClick)
    }
  })
}


export function listenLoadDocument(cb:(target:HTMLElement, action:string) => void): void {

  document.addEventListener("hyp-load", function(e) {
    let load = e.target as HTMLElement
    let action = load.dataset.onLoad
    let target = document.getElementById(load.dataset.target)

    if (!target) {
      console.error("Missing load target: ", target)
      return
    }

    cb(target, action)
  })

}

export function listenLoad(node:HTMLElement): void {
  // it doesn't really matter WHO runs this except that it should have target
  node.querySelectorAll("[data-on-load]").forEach((load:HTMLElement) => {
    let delay = parseInt(load.dataset.delay) || 0

    setTimeout(() => {
      let event = new Event("hyp-load", {bubbles:true})
      load.dispatchEvent(event)
    }, delay)
  })
}


export function listenChange(cb:(target:HTMLElement, action:string) => void): void {
  document.addEventListener("change", function(e) {
    let el = e.target as HTMLElement

    // clicks can fire on internal elements. Find the parent with a click handler
    let source = el.closest("[data-on-change]") as HTMLInputElement

    // they should all have an action and target
    if (source?.dataset.target && source.value) {
      e.preventDefault()

      let target = document.getElementById(source.dataset.target)

      if (!target) {
        console.error("Missing target: ", source.dataset.target)
        return
      }

      cb(target, source.value)
    }
  })
}

interface LiveInputElement extends HTMLInputElement {
  debouncedCallback?: Function;
}

export function listenInput(cb:(target:HTMLElement, actionConstructor:string, term:string) => void): void {
  document.addEventListener("input", function(e) {
    let el = e.target as HTMLElement
    let source = el.closest("[data-on-input]") as LiveInputElement
    let delay = parseInt(source.dataset.delay) || 100


    if (delay < 100) {
      console.warn("Input delay < 100 can result in poor performance")
    }

    if (source?.dataset.onInput && source?.dataset.target) {
      e.preventDefault()

      let target = document.getElementById(source.dataset.target)

      if (!target) {
        console.error("Missing target: ", source.dataset.target)
        return
      }

      if (!source.debouncedCallback) {
        source.debouncedCallback = debounce(() => cb(target, source.dataset.onInput, source.value), delay)
      }

      source.debouncedCallback()
    }
  })
}


export function listenFormSubmit(cb:(target:HTMLElement, action:string, form:FormData) => void): void {
  document.addEventListener("submit", function(e) {
    let form = e.target as HTMLFormElement

    // they should all have an action and target
    if (form?.dataset.onSubmit && form?.dataset.target) {
      e.preventDefault()

      let target = document.getElementById(form.dataset.target)

      if (!target) {
        console.error("Missing target: ", form.dataset.target)
        return
      }

      const formData = new FormData(form)
      cb(target, form.dataset.onSubmit, formData)
    }
  })
}
