import { patch, create } from "omdomdom/lib/omdomdom.es.js"
import { SocketConnection } from './sockets'
import { listenChange, listenClick, listenFormSubmit, listenLoad, listenLoadDocument, listenInput } from './events'
import { actionMessage, ActionMessage, ActionResponse, Trigger, parseResponse } from './action'
import { parseTrigger } from './meta'


// import { listenEvents } from './events';
// import { WEBSOCKET_ADDRESS, Messages } from './Messages'
// import { INIT_PAGE, INIT_STATE, State, Class } from './types';
// import { fromVDOM, VDOM } from './vdom'



// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("Hyperbole 0.4.1a")


let rootStyles: HTMLStyleElement;
let addedRulesIndex = new Set();


async function sendAction(msg: ActionMessage): Promise<ActionResponse> {
  async function sendActionHttp(msg: ActionMessage): Promise<ActionResponse> {
    console.log("HTTP sendAction", msg.url.toString())

    let res = await fetch(msg.url, {
      method: "POST",
      headers: { 'Accept': 'text/html', 'Content-Type': 'application/x-www-form-urlencoded' },
      body: msg.form,
      // we never want this to be redirected
      redirect: "manual"
    })

    if (res.headers.get('location')) {
      // manual redirect with status 200
      console.log("Found Redirect", res.headers.get('location'))
      window.location.href = res.headers.get('location')
      return
    }

    console.log("RES", res.headers.get("location"))

    if (res.headers.get("location")) {
      window.location.href = res.headers.get("location")
      return
    }

    if (!res.ok) {
      let error = new Error()
      error.name = "Fetch Error " + res.status
      let body = await res.text()
      error.message = body
      throw error
    }

    let triggers: Trigger[] = []

    if (res.headers.get('hyp-trigger')) {
      triggers = res.headers.get('hyp-trigger').split("||").map(parseTrigger)
    }

    let html = await res.text()

    // TODO: parse triggers from headers
    return { content: html, triggers }
  }

  if (sock.isConnected) {
    return sock.sendAction(msg)
  }
  else {
    return sendActionHttp(msg)
  }
}




async function fetchAction(msg: ActionMessage): Promise<ActionResponse> {
  try {
    let ret = await sendAction(msg)
    return ret
  }
  catch (err) {
    // handle error here
    document.body.innerHTML = errorHTML(err)
    throw err

  }
}

async function runAction(target: HTMLElement, action: string, form?: FormData) {

  let timeout = setTimeout(() => {
    // add loading after 200ms, not right away
    target.classList.add("hyp-loading")
  }, 200)

  let msg = actionMessage(target.id, action, form)

  let ret = await fetchAction(msg)

  let res = parseResponse(ret)

  if (!res.css || !res.content) {
    console.error("Empty Response", res)
    return
  }

  // First, update the stylesheet
  addCSS(res.css)

  // Patch the node
  const next: VNode = create(res.content)
  const old: VNode = create(target)
  patch(next, old)

  // Emit relevant events
  let newTarget = document.getElementById(target.id)
  // let event = new Event("content", {bubbles:true})
  // newTarget.dispatchEvent(event)

  // load doesn't bubble
  listenLoad(newTarget)

  // Remove loading and clear add timeout
  clearTimeout(timeout)
  target.classList.remove("hyp-loading")

  // Run any triggers
  setTimeout(() => ret.triggers.forEach(runTrigger), 10)
}


async function runTrigger(trigger: Trigger) {
  console.log("RUNNING TRIGGER", trigger)
  let target = document.getElementById(trigger.view)
  runAction(target, trigger.action)
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
    // console.log("INIT LOAD", target.id, action)
    runAction(target, action)
  })

  listenLoad(document.body)

  listenClick(async function(target: HTMLElement, action: string) {
    console.log("CLICK", target.id, action)
    runAction(target, action)
  })

  listenFormSubmit(async function(target: HTMLElement, action: string, form: FormData) {
    console.log("FORM", target.id, action, form)
    runAction(target, action, form)
  })

  listenChange(async function(target: HTMLElement, action: string) {
    console.log("CHANGE", target.id, action)
    runAction(target, action)
  })

  // WARNING: security flaw, unescaped output. no closing quotes allowed?
  listenInput(async function(target: HTMLElement, actionConstructor: string, term: string) {
    console.log("INPUT", target.id, actionConstructor, term)
    let action = `${actionConstructor} "${sanitizeInput(term)}"`
    runAction(target, action)
  })
}

function sanitizeInput(input: string): string {
  return input.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
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
