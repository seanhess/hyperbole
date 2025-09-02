import { patch, create } from "omdomdom/lib/omdomdom.es.js"
import { SocketConnection } from './sockets'
import { listenChange, listenClick, listenDblClick, listenFormSubmit, listenLoad, listenTopLevel, listenInput, listenKeydown, listenKeyup, listenMouseEnter, listenMouseLeave } from './events'
import { actionMessage, ActionMessage, requestId, ViewId, parseMetadata, Metadata } from './action'
import { sendActionHttp } from './http'
import { setQuery } from "./browser"
import { parseResponse, Response, LiveUpdate } from './response'

let PACKAGE = require('../package.json');


// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("Hyperbole " + PACKAGE.version)


let rootStyles: HTMLStyleElement;
let addedRulesIndex = new Set();


async function sendAction(msg: ActionMessage): Promise<Response> {
  if (sock.isConnected) {
    return sock.sendAction(msg)
  }
  else {
    return sendActionHttp(msg)
  }
}



async function runAction(target: HyperView, action: string, form?: FormData) {
  // console.log("runAction", target.id, action)

  if (target === undefined) {
    console.error("Undefined HyperView!")
    return
  }

  if (action === undefined) {
    console.error("Undefined Action!", target, "this is a bug, please report: https://github.com/seanhess/hyperbole")
    return
  }

  let timeout = setTimeout(() => {
    // add loading after 100ms, not right away
    // if it runs shorter than that we probably don't want to add loading effects
    target.classList.add("hyp-loading")
  }, 100)

  let reqId = requestId()
  let msg = actionMessage(target.id, action, reqId, form)

  // Ignore any request if a requestId is active
  if (target.dataset.requestId) {
    console.warn("Action overlaps with active request (" + target.dataset.requestId + ")", action)
    return
  }

  // Set the requestId
  target.dataset.requestId = reqId

  try {
    let res: Response = await sendAction(msg)

    if (res.meta.requestId != target.dataset.requestId) {
      let err = new Error()
      err.name = "Concurrency Error"
      err.message = "Stale Action (" + reqId + "):" + action
      throw err
    }
    else {
      delete target.dataset.requestId
    }

    runMetadataImmediate(res.meta)

    let update: LiveUpdate = parseResponse(res.body)

    if (!update.content) {
      let err = new Error("Empty Response")
      err.message = res.toString()
      throw err
    }

    // First, update the stylesheet
    addCSS(update.css)


    // Patch the node
    const old: VNode = create(target)
    let next: VNode = create(update.content)
    next.attributes = old.attributes
    patch(next, old)


    // Emit relevant events
    let newTarget = document.getElementById(target.id)
    dispatchContent(newTarget)

    if (newTarget) {
      // execute the metadata, anything that doesn't interrupt the dom update
      runMetadataDOM(res.meta, newTarget)

      // now way for these to bubble)
      listenLoad(newTarget)
      listenMouseEnter(newTarget)
      listenMouseLeave(newTarget)
      fixInputs(newTarget)
      enrichHyperViews(newTarget)
    }
    else {
      console.warn("Target Missing: ", target.id)
    }

  }
  catch (err) {
    console.error("Caught Error in HyperView (" + target.id + "):\n", err)

    // Hyperbole catches handler errors, and the server controls what to display to the user on an error
    //  but if you manage to crash your parent server process somehow, the response may be empty
    target.innerHTML = err.body || "<div style='background:red;color:white;padding:10px'>Hyperbole Internal Error</div>"
  }


  // Remove loading and clear add timeout
  clearTimeout(timeout)
  target.classList.remove("hyp-loading")
}

function runMetadataImmediate(meta: Metadata) {
  if (meta.redirect) {
    // perform a redirect immediately
    window.location.href = meta.redirect
  }

  if (meta.query != null) {
    setQuery(meta.query)
  }

  if (meta.pageTitle != null) {
    document.title = meta.pageTitle
  }
}

function runMetadataDOM(meta: Metadata, target?: HTMLElement) {
  for (var remoteEvent of meta.events) {
    setTimeout(() => {
      // console.log("dipsatching custom event", remoteEvent)
      let event = new CustomEvent(remoteEvent.name, { bubbles: true, detail: remoteEvent.detail })
      let eventTarget = target || document
      eventTarget.dispatchEvent(event)
    }, 10)
  }

  meta.actions.forEach(([viewId, action]) => {
    setTimeout(() => {
      let view = window.Hyperbole.hyperView(viewId)
      if (view) {
        runAction(view, action)
      }
    }, 10)
  })
}


function fixInputs(target: HTMLElement) {
  let focused = target.querySelector("[autofocus]") as HTMLInputElement
  if (focused?.focus) {
    focused.focus()
  }

  target.querySelectorAll("input[value]").forEach((input: HTMLInputElement) => {
    let val = input.getAttribute("value")
    if (val !== undefined) {
      input.value = val
    }
  })

  target.querySelectorAll("input[type=checkbox]").forEach((checkbox: HTMLInputElement) => {
    let checked = checkbox.dataset.checked == "True"
    checkbox.checked = checked
  })
}

function addCSS(src: HTMLStyleElement | null) {
  if (!src) return;
  const rules: any = src.sheet.cssRules
  for (const rule of rules) {
    if (addedRulesIndex.has(rule.cssText) == false) {
      rootStyles.sheet.insertRule(rule.cssText);
      addedRulesIndex.add(rule.cssText);
    }
  }
}




function init() {
  // metadata attached to initial page loads need to be executed
  let meta = parseMetadata(document.getElementById("hyp.metadata").innerText)
  runMetadataImmediate(meta)
  runMetadataDOM(meta)

  rootStyles = document.querySelector('style')

  listenTopLevel(async function(target: HyperView, action: string) {
    runAction(target, action)
  })

  listenLoad(document.body)
  listenMouseEnter(document.body)
  listenMouseLeave(document.body)
  enrichHyperViews(document.body)


  listenClick(async function(target: HyperView, action: string) {
    // console.log("CLICK", target.id, action)
    runAction(target, action)
  })

  listenDblClick(async function(target: HyperView, action: string) {
    // console.log("DBLCLICK", target.id, action)
    runAction(target, action)
  })

  listenKeydown(async function(target: HyperView, action: string) {
    // console.log("KEYDOWN", target.id, action)
    runAction(target, action)
  })

  listenKeyup(async function(target: HyperView, action: string) {
    // console.log("KEYUP", target.id, action)
    runAction(target, action)
  })

  listenFormSubmit(async function(target: HyperView, action: string, form: FormData) {
    // console.log("FORM", target.id, action, form)
    runAction(target, action, form)
  })

  listenChange(async function(target: HyperView, action: string) {
    // console.log("CHANGE", target.id, action)
    runAction(target, action)
  })

  listenInput(async function(target: HyperView, action: string) {
    runAction(target, action)
  })
}



function enrichHyperViews(node: HTMLElement): void {
  // enrich all the hyperviews
  node.querySelectorAll("[id]").forEach((element: HyperView) => {
    element.runAction = function(action: string) {
      runAction(this, action)
    }.bind(element)

    dispatchContent(node)
  })
}

function dispatchContent(node: HTMLElement): void {
  let event = new Event("hyp-content", { bubbles: true })
  node.dispatchEvent(event)
}

document.addEventListener("DOMContentLoaded", init)




// Should we connect to the socket or not?
const sock = new SocketConnection()
sock.connect()






type VNode = {
  // One of three value types are used:
  // - The tag name of the element
  // - "text" if text node
  // - "comment" if comment node
  type: string

  // An object whose key/value pairs are the attribute
  // name and value, respectively
  attributes: [string: string]

  // Is set to `true` if a node is an `svg`, which tells
  // Omdomdom to treat it, and its children, as such
  isSVGContext: Boolean

  // The content of a "text" or "comment" node
  content: string

  // An array of virtual node children
  children: Array<VNode>

  // The real DOM node
  node: Node
}





declare global {
  interface Window {
    Hyperbole?: HyperboleAPI;
  }
}

export interface HyperboleAPI {
  runAction(target: HTMLElement, action: string, form?: FormData): Promise<void>
  action(con: string, ...params: any[]): string
  hyperView(viewId: ViewId): HyperView | undefined
  parseMetadata(input: string): Metadata
}




export interface HyperView extends HTMLElement {
  runAction(target: HTMLElement, action: string, form?: FormData): Promise<void>
}



window.Hyperbole =
{
  runAction: runAction,
  parseMetadata: parseMetadata,
  action: function(con, ...params: any[]) {
    let ps = params.reduce((str, param) => str + " " + JSON.stringify(param), "")
    return con + ps
  },
  hyperView: function(viewId) {
    let element = document.getElementById(viewId) as any
    if (!element?.runAction) {
      console.error("Element id=" + viewId + " was not a HyperView")
      return undefined
    }
    return element
  },
}
