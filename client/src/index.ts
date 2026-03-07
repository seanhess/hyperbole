import { SocketConnection } from './sockets'
import { ViewId, Metadata, parseMetadata } from './message'
import { HyperView, init, isHyperView, runAction, sock } from "./hyperview"

let PACKAGE = require('../package.json');


// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("Hyperbole " + PACKAGE.version + "b")

document.addEventListener("DOMContentLoaded", init)

declare global {
  interface Window {
    Hyperbole?: HyperboleAPI;
  }
  interface DocumentEventMap {
    "hyp-load": CustomEvent;
    "hyp-mouseenter": CustomEvent;
    "hyp-mouseleave": CustomEvent;
  }
}

export interface HyperboleAPI {
  runAction(target: HTMLElement, action: string, form?: FormData): Promise<void>
  action(con: string, ...params: any[]): string
  hyperView(viewId: ViewId): HyperView | undefined
  parseMetadata(input: string): Metadata
  socket: SocketConnection
}

window.Hyperbole =
{
  runAction,
  parseMetadata: parseMetadata,
  action: function(con, ...params: any[]) {
    return params.reduce((str, param) => str + " " + JSON.stringify(param), con);
  },
  hyperView: function(viewId) {
    let element = document.getElementById(viewId)
    if (!isHyperView(element)) {
      console.error("Element id=" + viewId + " was not a HyperView")
      return
    }
    return element
  },
  socket: sock
}
