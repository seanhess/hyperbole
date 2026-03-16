import { ActionMessage, renderActionMessage } from './action'
import { dropWhile } from "./lib"
import { ResponseBody, Update, Redirect, Response, parseResponse, parseRedirect, requireMeta, parseUpdate } from "./response"
import * as message from "./message"
import { ViewId, RequestId, EncodedAction, metaValue, Metadata, Meta } from "./message"

const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
const defaultAddress = `${protocol}//${window.location.host}${window.location.pathname}`

interface SocketConnectionEventMap {
  "update": CustomEvent<Update>;
  "response": CustomEvent<Response>;
  "redirect": CustomEvent<Redirect>;
}

export class SocketConnection {
  socket: WebSocket

  hasEverConnected: Boolean = false
  isConnected: Boolean = false
  reconnectDelay: number = 0
  queue: ActionMessage[] = []
  events: EventTarget

  constructor(addr = defaultAddress) {
    this.events = new EventTarget()
    const sock = new WebSocket(addr)
    this.socket = sock
  }

  connect(addr = defaultAddress, createSocket = false) {
    const sock = createSocket ? new WebSocket(addr) : this.socket
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
      console.log("CLOSE SOCKET")
      if (this.isConnected) {
        document.dispatchEvent(new Event("hyp-socket-disconnect"))
      }

      this.isConnected = false
      sock.removeEventListener('error', onSocketError)

      // attempt to reconnect in 1s
      if (this.hasEverConnected) {
        console.log("Reconnecting in " + (this.reconnectDelay / 1000) + "s")
        setTimeout(() => this.connect(addr, true), this.reconnectDelay)
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
    let next: ActionMessage | undefined = this.queue.pop()
    if (next) {
      console.log("runQueue: ", next)
      this.sendAction(next)
      this.runQueue()
    }
  }


  // full responses will never be sent over!
  private onMessage(event: MessageEvent) {
    let { command, metas, rest } = splitMessage(event.data)
    // console.log("MESSAGE", command, metas, rest)
    //
    switch (command) {
      case "|UPDATE|":
        this.dispatchEvent(new CustomEvent<Update>("update", { detail: parseUpdate(metas, rest) }))
        return

      case "|RESPONSE|":
        this.dispatchEvent(new CustomEvent<Response>("response", { detail: parseResponse(metas, rest) }))
        return

      case "|REDIRECT|":
        this.dispatchEvent(new CustomEvent<Redirect>("redirect", { detail: parseRedirect(metas, rest) }))
        return
    }
  }

  addEventListener<K extends keyof SocketConnectionEventMap>(e: K, cb: (ev: SocketConnectionEventMap[K]) => void) {
    this.events.addEventListener(e,
      // @ts-ignore: HACK
      cb
    )
  }

  dispatchEvent<K extends keyof SocketConnectionEventMap>(e: SocketConnectionEventMap[K]) {
    this.events.dispatchEvent(e)
  }

  disconnect() {
    this.isConnected = false
    this.hasEverConnected = false
    this.socket.close()
  }
}



export type MessageType = string



export type SplitMessage = {
  command: string,
  metas: Meta[],
  rest: string[]
}


export function splitMessage(msg: string): SplitMessage {
  let lines = msg.split("\n")
  let command: string = lines[0]
  let metas: Meta[] = message.parseMetas(lines.slice(1))
  // console.log("Split Metadata", lines.length)
  // console.log(" [0]", lines[0])
  // console.log(" [1]", lines[1])
  let rest = dropWhile(l => l == "", lines.slice(metas.length + 1))

  return { command, metas, rest }
}
