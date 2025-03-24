import { patch, create } from "omdomdom/lib/omdomdom.es.js"
import { SocketConnection } from './sockets'
import { listenChange, listenClick, listenDblClick, listenFormSubmit, listenLoad, listenLoadDocument, listenInput, listenKeydown, listenKeyup } from './events'
import { actionMessage, ActionMessage, requestId, RequestId } from './action'
import { sendActionHttp } from './http'
import { setQuery } from "./browser"
import { parseResponse, Response, LiveUpdate } from './response'

let PACKAGE = require('../package.json');

// import { listenEvents } from './events';
// import { WEBSOCKET_ADDRESS, Messages } from './Messages'
// import { INIT_PAGE, INIT_STATE, State, Class } from './types';
// import { fromVDOM, VDOM } from './vdom'


// const CONTENT_ID = "yeti-root-content"

// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("Hyperbole " + PACKAGE.version)


let rootStyles: HTMLStyleElement;
let addedRulesIndex = new Set();


async function sendAction(reqId: RequestId, msg: ActionMessage): Promise<Response> {
  if (sock.isConnected) {
    return sock.sendAction(reqId, msg)
  }
  else {
    return sendActionHttp(reqId, msg)
  }
}


async function fetchAction(reqId: RequestId, msg: ActionMessage): Promise<Response> {
  try {
    let res = await sendAction(reqId, msg)

    if (res.location) {
      window.location.href = res.location
      return // not reachable
    }

    if (res.query != null) {
      setQuery(res.query)
    }

    return res
  }
  catch (err) {
    // handle error here
    document.body.innerHTML = errorHTML(err)
    throw err

  }
}

async function runAction(target: HTMLElement, action: string, form?: FormData) {

  if (action === undefined) {
    console.error("Undefined Action!", target, "this is a bug, please report: https://github.com/seanhess/hyperbole")
    return
  }


  let timeout = setTimeout(() => {
    // add loading after 100ms, not right away
    // if it runs shorter than that we probably don't want to add loading effects
    target.classList.add("hyp-loading")
  }, 100)

  let msg = actionMessage(target.id, action, form)

  // Set the requestId
  let reqId = requestId()
  target.dataset.requestId = reqId

  let res: Response = await fetchAction(reqId, msg)

  if (reqId != target.dataset.requestId) {
    console.warn("Ignoring Stale Action (" + reqId + "):", action)
    return
  }

  let update: LiveUpdate = parseResponse(res.body)

  if (!update.css || !update.content) {
    // TODO: error handling
    console.error("Empty Response", res)
    return
  }

  // First, update the stylesheet
  addCSS(update.css)

  // Patch the node
  const next: VNode = create(update.content)
  const old: VNode = create(target)
  patch(next, old)

  // console.log("NEXT", next)

  // Emit relevant events
  let newTarget = document.getElementById(target.id)
  // let event = new Event("content", {bubbles:true})
  // newTarget.dispatchEvent(event)
  //

  if (newTarget) {
    // now way for these to bubble)
    listenLoad(newTarget)
    fixInputs(newTarget)
    enrichHyperViews(newTarget)
  }
  else {
    console.warn("Target Missing: ", target.id)
  }

  // Remove loading and clear add timeout

  clearTimeout(timeout)
  target.classList.remove("hyp-loading")
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

function addCSS(src: HTMLStyleElement) {
  const rules: any = src.sheet.cssRules
  for (const rule of rules) {
    if (addedRulesIndex.has(rule.cssText) == false) {
      rootStyles.sheet.insertRule(rule.cssText);
      addedRulesIndex.add(rule.cssText);
    }
  }
}




function init() {
  rootStyles = document.querySelector('style')

  listenLoadDocument(async function(target: HTMLElement, action: string) {
    runAction(target, action)
  })

  listenLoad(document.body)

  enrichHyperViews(document.body)

  listenClick(async function(target: HTMLElement, action: string) {
    // console.log("CLICK", target.id, action)
    runAction(target, action)
  })

  listenDblClick(async function(target: HTMLElement, action: string) {
    // console.log("DBLCLICK", target.id, action)
    runAction(target, action)
  })

  listenKeydown(async function(target: HTMLElement, action: string) {
    // console.log("KEYDOWN", target.id, action)
    runAction(target, action)
  })

  listenKeyup(async function(target: HTMLElement, action: string) {
    // console.log("KEYUP", target.id, action)
    runAction(target, action)
  })

  listenFormSubmit(async function(target: HTMLElement, action: string, form: FormData) {
    // console.log("FORM", target.id, action, form)
    runAction(target, action, form)
  })

  listenChange(async function(target: HTMLElement, action: string) {
    // console.log("CHANGE", target.id, action)
    runAction(target, action)
  })

  listenInput(async function(target: HTMLElement, action: string) {
    console.log("INPUT", target.id, action)
    runAction(target, action)
  })
}

function enrichHyperViews(node: HTMLElement): void {
  // enrich all the hyperviews
  node.querySelectorAll("[id]").forEach((element: any) => {
    console.log("Found HyperView", element.dataset)

    // Add 
    element.runAction = function(action: string) {
      runAction(this, action)
    }.bind(element)
  })
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



// no it should take over the whole page...
function errorHTML(error: Error): string {

  // TODO: match on error.name and handle it differently
  let style = [
    ".hyp-error {background-color:#DB3524; color:white; padding: 10px}",
    ".hyp-details {padding: 10px}"
  ]
  let content = `<div class='hyp-error'>${error.name}</div>`
  let details = `<div class='hyp-details'>${error.message}</div>`

  return ["<style>" + style.join("\n") + "</style>", content, details].join("\n")
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
}




export interface HyperView extends HTMLElement {
  runAction(target: HTMLElement, action: string, form?: FormData): Promise<void>
}

export type ViewId = string


window.Hyperbole =
{
  runAction: runAction,
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
  }
}
