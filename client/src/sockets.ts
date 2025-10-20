import { ActionMessage, renderActionMessage } from './action'
import { Response, FetchError, ResponseBody } from "./response"
import * as message from "./message"
import { Meta, SplitMessage, ViewId, RequestId, splitMessage, EncodedAction, metaValue, Metadata } from "./message"

const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
const defaultAddress = `${protocol}//${window.location.host}${window.location.pathname}`



export class SocketConnection extends EventTarget {
  socket: WebSocket

  hasEverConnected: Boolean = false
  isConnected: Boolean = false
  reconnectDelay: number = 0
  queue: ActionMessage[] = []

  constructor() { super() }

  connect(addr = defaultAddress) {
    const sock = new WebSocket(addr)
    this.socket = sock

    function onConnectError(ev: Event) {
      console.error("Connect Error", ev)
    }

    function onSocketError(ev: Event) {
      console.error("Socket Error", ev)
    }


    // initial connection errors
    sock.addEventListener('error', onConnectError)

    sock.addEventListener('open', (_event) => {
      console.log("Websocket Connected")

      if (this.hasEverConnected) {
        document.dispatchEvent(new Event("hyp-socket-reconnect"))
      }

      this.isConnected = true
      this.hasEverConnected = true
      this.reconnectDelay = 1000
      sock.removeEventListener('error', onConnectError)
      sock.addEventListener('error', onSocketError)

      document.dispatchEvent(new Event("hyp-socket-connect"))

      this.runQueue()
    })

    sock.addEventListener('close', _ => {
      if (this.isConnected) {
        document.dispatchEvent(new Event("hyp-socket-disconnect"))
      }

      this.isConnected = false
      sock.removeEventListener('error', onSocketError)

      // attempt to reconnect in 1s
      if (this.hasEverConnected) {
        console.log("Reconnecting in " + (this.reconnectDelay / 1000) + "s")
        setTimeout(() => this.connect(addr), this.reconnectDelay)
      }

      sock.removeEventListener('error', onSocketError)
    })

    sock.addEventListener('message', ev => this.onMessage(ev))
  }

  async sendAction(action: ActionMessage) {
    if (this.isConnected) {
      let msg = renderActionMessage(action)
      this.socket.send(msg)
    }
    else {
      this.queue.push(action)
    }
  }

  private runQueue() {
    // send all messages queued while disconnected 
    let next: ActionMessage | null = this.queue.pop()
    console.log("RUN QUEUE", next)
    if (next) {
      this.sendAction(next)
      this.runQueue()
    }
  }


  // full responses will never be sent over!
  private onMessage(event: MessageEvent) {
    let { command, metas, rest } = message.splitMessage(event.data)
    // console.log("MESSAGE", command, metas, rest)

    let requestId = requireMeta("RequestId")

    function requireMeta(key: string): string {
      let val = metaValue(key, metas)
      if (!val) throw new ProtocolError("Missing Required Metadata: " + key, event.data)
      return val
    }

    function parseUpdate(rest: string[]): Update {
      let viewId = requireMeta("ViewId")
      let action = requireMeta("Action")
      return {
        requestId,
        viewId,
        action,
        meta: message.toMetadata(metas),
        body: rest.join("\n"),
      }
    }

    function parseRedirect(rest: string[]): Redirect {
      let url = rest[0]
      return {
        requestId,
        url
      }
    }

    switch (command) {

      case "|UPDATE|":
        return this.dispatchEvent(new CustomEvent("update", { detail: parseUpdate(rest) }))

      case "|RESPONSE|":
        return this.dispatchEvent(new CustomEvent("response", { detail: parseUpdate(rest) }))

      case "|REDIRECT|":
        return this.dispatchEvent(new CustomEvent("redirect", { detail: parseRedirect(rest) }))
    }
  }


  // so what if they send remote events in the page? trigger, redirect, page title, etc...
  // we aren't connected yet on a page thing

  // private async waitMessage(reqId: RequestId, id: ViewId): Promise<ParsedResponse> {
  //   return new Promise((resolve, reject) => {
  //     const onMessage = (event: MessageEvent) => {
  //       let data: string = event.data
  //       let lines = data.split("\n").slice(1)  // drop the command line
  //
  //       let parsed = splitMetadata(lines)
  //       let metadata: Metadata = parsed.metadata
  //
  //       if (!metadata.requestId) {
  //         console.error("Missing RequestId!", metadata, event.data)
  //         return
  //       }
  //
  //       if (metadata.requestId != reqId) {
  //         // skip, it's not us!
  //         return
  //       }
  //
  //
  //       // We have found our message. Remove the listener
  //       this.socket.removeEventListener('message', onMessage)
  //
  //       // set the cookies. These happen automatically in http
  //       metadata.cookies.forEach((cookie: string) => {
  //         document.cookie = cookie
  //       })
  //
  //       if (metadata.error) {
  //         reject(new FetchError(id, metadata.error, parsed.rest.join('\n')))
  //         return
  //       }
  //
  //       resolve(parsed)
  //     }
  //
  //     this.socket.addEventListener('message', onMessage)
  //     this.socket.addEventListener('error', reject)
  //   })
  // }
  //
  // disconnect() {
  //   this.socket.close()
  // }
}


export type Update = {
  requestId: RequestId
  meta: Metadata
  viewId: ViewId
  action: EncodedAction
  body: ResponseBody
}

export type Redirect = {
  requestId: RequestId
  url: string
}

export type MessageType = string


// PARSING MESSAGE  ---------------------------------------

export class ProtocolError extends Error {
  constructor(description: string, body: string) {
    super(description + "\n" + body)
    this.name = "ProtocolError"
  }
}
