import { create, patch } from 'omdomdom/lib/omdomdom.es.js';
import { actionMessage, newRequest, type Request } from './action'
import { setQuery } from './browser';
import { listenChange, listenClick, listenDblClick, listenFormSubmit, listenInput, listenKeydown, listenKeyup, listenLoad, listenMouseEnter, listenMouseLeave, listenTopLevel } from './events';
import { Metadata, parseMetadata } from './message';
import { LiveUpdate, parseResponse } from './response';
import { Redirect, SocketConnection, Update } from './sockets';

let rootStyles: HTMLStyleElement;
let addedRulesIndex = new Set();

type VNode = {
  // One of three value types are used:
  // - The tag name of the element
  // - "text" if text node
  // - "comment" if comment node
  type: string

  // An object whose key/value pairs are the attribute
  // name and value, respectively
  attributes: { [key: string]:string | undefined }

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

export interface HyperView extends HTMLElement {
  runAction(action: string): Promise<void>;
  activeRequest?: Request;
  cancelActiveRequest(): void;
  concurrency: ConcurrencyMode;
  _timeout?: number;
}

export const isHyperView = (ele: any): ele is HyperView => {
  return ele?.runAction !== undefined;
};

export type ConcurrencyMode = string;

// Run an action in a given HyperView
export async function runAction(target: HyperView, action: string, form?: FormData) {

  if (target.activeRequest && !target.activeRequest?.isCancelled) {
    // Active Request!
    if (target.concurrency == "Drop") {
      console.warn("Drop action overlapping with active request (" + target.activeRequest + ")", action)
      return
    }
  }

  target._timeout = window.setTimeout(() => {
    // add loading after 100ms, not right away
    // if it runs shorter than that we probably don't want to show the user any loading feedback
    target.classList.add("hyp-loading")
  }, 100)

  let state = target.dataset.state

  let req = newRequest()
  let msg = actionMessage(target.id, action, state, req.requestId, form)

  // Set the requestId
  target.activeRequest = req

  sock.sendAction(msg)
}


function enrichHyperViews(node: HTMLElement): void {
  // enrich all the hyperviews
  node.querySelectorAll<HyperView>("[id]").forEach((element) => {
    element.runAction = function(action: string) {
      return runAction(element, action)
    }

    element.concurrency = element.dataset.concurrency || "Drop"

    element.cancelActiveRequest = function() {
      if (element.activeRequest && !element.activeRequest?.isCancelled) {
        element.activeRequest.isCancelled = true
      }
    }

    dispatchContent(node)
  })
}


function dispatchContent(node: HTMLElement): void {
  let event = new Event("hyp-content", { bubbles: true })
  node.dispatchEvent(event)
}

function runMetadata(meta: Metadata, target?: HTMLElement) {
  if (meta.query != null) {
    setQuery(meta.query)
  }

  if (meta.pageTitle != null) {
    document.title = meta.pageTitle
  }

  meta.events?.forEach((remoteEvent) => {
    setTimeout(() => {
      let event = new CustomEvent(remoteEvent.name, { bubbles: true, detail: remoteEvent.detail })
      let eventTarget = target || document
      eventTarget.dispatchEvent(event)
    }, 10)
  })

  meta.actions?.forEach(([viewId, action]) => {
    setTimeout(() => {
      let view = window.Hyperbole?.hyperView(viewId)
      if (view) {
        runAction(view, action)
      }
    }, 10)
  })
}

function fixInputs(target: HTMLElement) {
  let focused = target.querySelector<HTMLInputElement>("[autofocus]")
  if (focused?.focus) {
    focused.focus()
  }

  target.querySelectorAll<HTMLInputElement>("input[value]").forEach((input) => {
    let val = input.getAttribute("value")
    if (val !== null) {
      input.value = val
    }
  })

  target.querySelectorAll<HTMLInputElement>("input[type=checkbox]").forEach((checkbox) => {
    let checked = checkbox.dataset.checked == "True"
    checkbox.checked = checked
  })
}

function applyCookies(cookies: string[]) {
  cookies.forEach((cookie: string) => {
    console.log("SetCookie: ", cookie)
    document.cookie = cookie
  })
}

function addCSS(src: HTMLStyleElement | null) {
  if (!src) return;
  const rules = src.sheet?.cssRules
  if (!rules) return;
  for (let i = 0; i < rules.length; i++) {
    const rule = rules.item(i)
    if (rule && addedRulesIndex.has(rule.cssText) == false && rootStyles.sheet) {
      rootStyles.sheet.insertRule(rule.cssText);
      addedRulesIndex.add(rule.cssText);
    }
  }
}

export function init() {
  // metadata attached to initial page loads need to be executed
  let meta = parseMetadata(document.getElementById("hyp.metadata")?.innerText ?? "")
  // runMetadataImmediate(meta)
  runMetadata(meta)

  const style = document.body.querySelector('style')

  if (style !== null) {
    rootStyles = style
  } else {
    console.warn("rootStyles missing from page, creating...")
    rootStyles = document.createElement("style")
    rootStyles.type = "text/css"
    document.body.appendChild(rootStyles)
  }

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
    runAction(target, action)
  })

  function onStartedTyping(target: HyperView) {
    if (target.concurrency == "Replace") {
      target.cancelActiveRequest()
    }
  }

  listenInput(onStartedTyping, async function(target: HyperView, action: string) {
    runAction(target, action)
  })
}

function handleUpdate(res: Update): HyperView | undefined {
  // console.log("|UPDATE|", res)

  let targetViewId = res.targetViewId || res.viewId
  let target = document.getElementById(targetViewId)

  if (!isHyperView(target)) {
    console.error("Missing Update HyperView Target: ", targetViewId, res)
    return
  }

  if (target.activeRequest?.requestId && res.requestId < target.activeRequest.requestId) {
    // this should only happen on Replace, since other requests should be dropped
    // but it's safe to assume we never want to apply an old requestId
    console.warn("Ignore Stale Action (" + res.requestId + ") vs (" + target.activeRequest.requestId + "): " + res.action)
    return target
  }
  else if (target.activeRequest?.isCancelled) {
    console.warn("Cancelled request", target.activeRequest?.requestId)
    delete target.activeRequest
    return target
  }

  let update: LiveUpdate = parseResponse(res.body)

  if (!update.content) {
    console.error("Empty Response!", res.body)
    return target
  }

  // First, update the stylesheet
  addCSS(update.css)


  // Patch the node
  const old: VNode = create(target)
  let next: VNode = create(update.content)
  let atts = next.attributes

  if (atts["id"] !== target.id) {
    console.error("Mismatched ViewId in update - ", atts["id"], " target:", target.id)
    return
  }

  let state = atts["data-state"]
  next.attributes = old.attributes


  patch(next, old)


  // Emit relevant events
  let newTarget = document.getElementById(target.id)

  if (!newTarget) {
    console.warn("Target Missing: ", target.id)
    return target
  }

  dispatchContent(newTarget)

  // re-add state attribute
  if (state === undefined || state == "()")
    delete newTarget.dataset.state
  else
    newTarget.dataset.state = state

  // execute the metadata, anything that doesn't interrupt the dom update
  runMetadata(res.meta, newTarget)
  applyCookies(res.meta.cookies ?? [])

  // now way for these to bubble)
  listenLoad(newTarget)
  listenMouseEnter(newTarget)
  listenMouseLeave(newTarget)
  fixInputs(newTarget)
  enrichHyperViews(newTarget)

  return target
}

// TODO: redirect concurrency
function handleRedirect(red: Redirect) {
  console.log("REDIRECT", red)

  // the other metdata doesn't apply, they are all specific to the page
  applyCookies(red.meta.cookies ?? [])

  window.location.href = red.url
}

// in-process update
function handleResponse(res: Update) {
  // console.log("Handle Response", res)
  let target = handleUpdate(res)
  if (!target) return

  // clean up the request
  delete target.activeRequest
  clearTimeout(target._timeout)
  target.classList.remove("hyp-loading")
}

export const sock = new SocketConnection()
sock.addEventListener("update", (ev: CustomEvent<Update>) => { handleUpdate(ev.detail) })
sock.addEventListener("response", (ev: CustomEvent<Update>) => handleResponse(ev.detail))
sock.addEventListener("redirect", (ev: CustomEvent<Redirect>) => handleRedirect(ev.detail))
