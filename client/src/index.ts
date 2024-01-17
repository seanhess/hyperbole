import { patch, create } from "omdomdom/lib/omdomdom.es.js"
import { SocketConnection } from './sockets'
import  { listenChange, listenClick, listenFormSubmit } from './events'
import  { actionMessage, ActionMessage } from './action'


// import { listenEvents } from './events';
// import { WEBSOCKET_ADDRESS, Messages } from './Messages'
// import { INIT_PAGE, INIT_STATE, State, Class } from './types';
// import { fromVDOM, VDOM } from './vdom'


// const CONTENT_ID = "yeti-root-content"

// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("Hyperbole 0.2.0")


let rootStyles: HTMLStyleElement;


listenClick(async function(target:HTMLElement, action:string) {
  console.log("CLICK", target.id, action)
  runAction(target, action)
})

listenFormSubmit(async function(target:HTMLElement, action:string, form:FormData) {
  console.log("FORM", target.id, action,form)
  runAction(target, action, form)
})

listenChange(async function(target:HTMLElement, action:string) {
  console.log("CHANGE", target.id, action)
  runAction(target, action)
})

async function sendAction(msg:ActionMessage) {
  async function sendActionHttp(msg:ActionMessage) {
    console.log("HTTP sendAction", msg.url.toString())

    let res = await fetch(msg.url, {
      method: "POST",
      headers: { 'Accept': 'text/html', 'Content-Type': 'application/x-www-form-urlencoded'},
      body: msg.form
    })

    return res.text()
  }

  if (sock.isConnected) {
    return sock.sendAction(msg)
  }
  else {
    return sendActionHttp(msg)
  }
}



async function runAction(target:HTMLElement, action:string, form?:FormData) {

  let timeout = setTimeout(() => {
    // add loading after 200ms, not right away
    target.classList.add("hyp-loading")
  }, 200)

  let msg = actionMessage(target.id, action, form)

  let ret = await sendAction(msg)
  let res = parseResponse(ret)

  // First, update the stylesheet
  addCSS(res.css.textContent)

  // Patch the node
  const next:VNode = create(res.content)
  const old:VNode = create(target)
  patch(next, old)

  // Remove loading and clear add timeout
  clearTimeout(timeout)
  target.classList.remove("hyp-loading")
}


function addCSS(text:string) {
  let rules = text.split("\n")
  rules.forEach((rule) => {
    rootStyles.sheet.insertRule(rule)
  })
}

type Response = {
  content: HTMLElement
  css: HTMLStyleElement
}

function parseResponse(ret:string):Response {
  const parser = new DOMParser()
  const doc = parser.parseFromString(ret, 'text/html')
  const css = doc.querySelector("style") as HTMLStyleElement
  const content = doc.querySelector("div") as HTMLElement

  return {
    content: content,
    css: css
  }
}

function init() {
  rootStyles = document.querySelector('style')
}

document.addEventListener("DOMContentLoaded", init)


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
