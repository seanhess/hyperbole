import { render, patch, create } from "omdomdom/lib/omdomdom.es.js"




// import { listenEvents } from './events';
// import { WEBSOCKET_ADDRESS, Messages } from './Messages'
// import { INIT_PAGE, INIT_STATE, State, Class } from './types';
// import { fromVDOM, VDOM } from './vdom'
import  { listenChange, listenClick, listenFormSubmit } from './events'


// const CONTENT_ID = "yeti-root-content"

// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("Hyperbole 0.1.4")

// const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
// const address = `${protocol}//${window.location.host}`
// const socket = new WebSocket(address)
//

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

async function sendAction(id:string, action:string, form?:FormData) {
  let url = new URL(window.location.href)
  url.searchParams.append("id", id)
  url.searchParams.append("action", action)

  console.log("ACTION", url.toString())

  let res = await fetch(url, {
    method: "POST",
    headers: { 'Accept': 'text/html', 'Content-Type': 'application/x-www-form-urlencoded'},
    body: toSearch(form)
  })

  return res.text()
}

async function runAction(target:HTMLElement, action:string, form?:FormData) {

  let timeout = setTimeout(() => {
    // add loading after 200ms, not right away
    target.classList.add("hyp-loading")
  }, 200)

  let ret = await sendAction(target.id, action, form)
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

function toSearch(form?:FormData):URLSearchParams | undefined {
  if (!form) return undefined
    
  const params = new URLSearchParams()

  form.forEach((value, key) => {
    params.append(key, value as string)
  })

  return params
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



// socket.addEventListener('open', (event) => {
//   console.log("Opened")
// })
//
// socket.addEventListener('error', (event) => {
//   console.log("Error")
// })
//
// socket.addEventListener('message', (event) => {
//   console.log("message", event.data)
//   let {command, data} = parseCommand(event.data)
//   console.log("CMD", command)
//   console.log("DATA", data)
// })
//
// socket.addEventListener('close', (event) => {
//   console.log("close")
// })
//
// type Command<T> = {
//   command: string,
//   data: T
// }
//
// function parseCommand<T>(message:string): Command<T> {
//   const match = message.match(/^(\w+)\s+(.*)$/)
//
//   if (!match) console.error("Could not parse command: ", message)
//
//   return {
//     command: match[1],
//     data: JSON.parse(match[2])
//   }
// }


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
