
import * as debounce from 'debounce'
import { encodedParam } from './action'
import { HyperView, isHyperView } from './hyperview'

export type UrlFragment = string

export function listenKeydown(cb: (target: HyperView, action: string) => void): void {
  listenKeyEvent("keydown", cb)
}

export function listenKeyup(cb: (target: HyperView, action: string) => void): void {
  listenKeyEvent("keyup", cb)
}

export function listenKeyEvent(event: "keyup" | "keydown", cb: (target: HyperView, action: string) => void): void {

  document.addEventListener(event, function(e: KeyboardEvent) {
    if (!(e.target instanceof HTMLElement)) {
      console.warn("listenKeyEvent received event with non HTMLElment as EventTarget: %o", e)
      return
    }
    let source = e.target

    let datasetKey = "on" + event + e.key
    let action = source.dataset[datasetKey]
    if (!action) return

    e.preventDefault()
    const target =  nearestHyperViewTarget(source)
    if (!target) {
      console.error("Missing target: ", source)
      return
    }
    cb(target, action)
  })
}

export function listenBubblingEvent(event: string, cb: (_target: HyperView, action: string) => void): void {
  document.addEventListener(event, function(e) {
    if (!(e.target instanceof HTMLElement)) {
      console.warn("listenBubblingEvent received an event with non HTMLElment as EventTarget: %o", e)
      return
    }
    let el = e.target

    // clicks can fire on internal elements. Find the parent with a click handler
    let source = el.closest<HTMLElement>("[data-on" + event + "]")
    if (!source) return

    e.preventDefault()
    let target = nearestHyperViewTarget(source)
    if (!target) {
      console.error("Missing target: ", source)
      return
    }
    const action = source.dataset["on" + event]
    if (action === undefined) {
      console.error("Missing action: ", source, event)
      return
    }
    cb(target, action)
  })
}

export function listenClick(cb: (target: HyperView, action: string) => void): void {
  listenBubblingEvent("click", cb)
}

export function listenDblClick(cb: (target: HyperView, action: string) => void): void {
  listenBubblingEvent("dblclick", cb)
}


export function listenTopLevel(cb: (target: HyperView, action: string) => void): void {
  document.addEventListener("hyp-load", function(e: CustomEvent) {
    let action = e.detail.onLoad
    let target = e.detail.target
    cb(target, action)
  })

  document.addEventListener("hyp-mouseenter", function(e: CustomEvent) {
    let action = e.detail.onMouseEnter
    let target = e.detail.target
    cb(target, action)
  })

  document.addEventListener("hyp-mouseleave", function(e: CustomEvent) {
    let action = e.detail.onMouseLeave
    let target = e.detail.target
    cb(target, action)
  })
}


export function listenLoad(node: HTMLElement): void {

  // it doesn't really matter WHO runs this except that it should have target
  node.querySelectorAll<HTMLElement>("[data-onload]").forEach((load) => {
    let delay = parseInt(load.dataset.delay || "") || 0
    let onLoad = load.dataset.onload
    // console.log("load start", load.dataset.onLoad)

    // load no longer exists!
    // we should clear the timeout or back out if the dom is replaced in the interem
    setTimeout(() => {
      let target = nearestHyperViewTarget(load)
      // console.log("load go", load.dataset.onLoad)

      if (load.dataset.onload != onLoad) {
        // the onLoad no longer exists
        return
      }

      const event = new CustomEvent("hyp-load", { bubbles: true, detail: { target, onLoad } })
      load.dispatchEvent(event)
    }, delay)
  })
}

export function listenMouseEnter(node: HTMLElement): void {
  node.querySelectorAll<HTMLElement>("[data-onmouseenter]").forEach((node) => {
    let onMouseEnter = node.dataset.onmouseenter

    let target = nearestAnyTarget(node)

    node.onmouseenter = () => {
      const event = new CustomEvent("hyp-mouseenter", { bubbles: true, detail: { target, onMouseEnter } })
      node.dispatchEvent(event)
    }
  })
}

export function listenMouseLeave(node: HTMLElement): void {
  node.querySelectorAll<HTMLElement>("[data-onmouseleave]").forEach((node) => {
    let onMouseLeave = node.dataset.onmouseleave

    let target = nearestAnyTarget(node)

    node.onmouseleave = () => {
      const event = new CustomEvent("hyp-mouseleave", { bubbles: true, detail: { target, onMouseLeave } })
      node.dispatchEvent(event)
    }
  })
}


export function listenChange(cb: (target: HyperView, action: string) => void): void {
  document.addEventListener("change", function(e) {
    if (!(e.target instanceof HTMLElement)) {
      console.warn("listenChange received an event with non HTMLElment as EventTarget: %o", e)
      return
    }
    let el = e.target

    let source = el.closest<HTMLInputElement>("[data-onchange]")

    if (!source) return
    e.preventDefault()

    if (source.value === null) {
      console.error("Missing input value:", source)
      return
    }

    let target = nearestHyperViewTarget(source)
    if (!target) {
      console.error("Missing target: listenChange")
      return
    }
    if (source.dataset.onchange === undefined) {
      console.error("source.dataset.onchange is undefined")
      return
    }
    let action = encodedParam(source.dataset.onchange, source.value)
    cb(target, action)
  })
}

interface LiveInputElement extends HTMLInputElement {
  debouncedCallback?: Function;
}

export function listenInput(startedTyping: (target: HyperView) => void, cb: (target: HyperView, action: string) => void): void {
  document.addEventListener("input", function(e) {
    if (!(e.target instanceof HTMLElement)) {
      console.warn("listenInput received an event with non HTMLElment as EventTarget: %o", e)
      return
    }
    let el = e.target
    const source = el.closest<LiveInputElement>("[data-oninput]")

    if (!source) return

    let delay = parseInt(source.dataset.delay || "") || 250
    if (delay < 250) {
      console.warn("Input delay < 250 can result in poor performance.")
    }

    e.preventDefault()

    const target = nearestHyperViewTarget(source)
    if (!target) {
      console.error("Missing target: ", source)
      return
    }

    // I want to CANCEL the active request as soon as we start typing
    startedTyping(target)

    if (!source.debouncedCallback) {
      source.debouncedCallback = debounce(() => {
        if (!source.dataset.oninput) {
          console.error("Missing onInput: ", source)
          return
        }
        const action = encodedParam(source.dataset.oninput, source.value)
        cb(target, action)
      }, delay)
    }

    source.debouncedCallback()
  })
}



export function listenFormSubmit(cb: (target: HyperView, action: string, form: FormData) => void): void {
  document.addEventListener("submit", function(e) {
    if (!(e.target instanceof HTMLFormElement)) {
      console.warn("listenFormSubmit received an event with non HTMLElment as EventTarget: %o", e)
      return
    }
    let form = e.target


    if (!form.dataset.onsubmit) {
      console.error("Missing onSubmit: ", form)
      return
    }

    e.preventDefault()

    let target = nearestHyperViewTarget(form)
    const formData = new FormData(form)
    if (!target) {
      console.error("Missing target: ", form)
      return
    }
    cb(target, form.dataset.onsubmit, formData)
  })
}

function nearestTargetId(node: HTMLElement): string | undefined {
  let targetData = node.closest<HTMLElement>("[data-target]")
  return targetData?.dataset.target || node.closest("[id]")?.id
}

function nearestHyperViewTarget(node: HTMLElement): HyperView | undefined {
  const target = nearestAnyTarget(node)

  if (!isHyperView(target)) {
    console.error("Non HyperView target: ", target)
    return
  }

  return target
}

function nearestAnyTarget(node: HTMLElement): HTMLElement | undefined {
  let targetId = nearestTargetId(node)
  let target = targetId && document.getElementById(targetId)

  if (!target) {
    console.error("Cannot find target: ", targetId, node)
    return
  }

  return target
}
