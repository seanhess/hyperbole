import { ActionMessage, ViewId, RequestId } from './action'
import { Response, FetchError } from "./response"
import { Metadata, ParsedResponse, splitMetadata } from "./response"

const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
const defaultAddress = `${protocol}//${window.location.host}${window.location.pathname}`



export class SocketConnection {
  socket: WebSocket

  hasEverConnected: Boolean
  isConnected: Boolean
  reconnectDelay: number = 0

  constructor() {
  }

  // we need to faithfully transmit the 
  connect(addr = defaultAddress) {
    const sock = new WebSocket(addr)
    this.socket = sock

    function onConnectError(ev: Event) {
      console.log("Connection Error", ev)
    }

    sock.addEventListener('error', onConnectError)

    sock.addEventListener('open', (_event) => {
      console.log("Websocket Connected")

      if (this.hasEverConnected) {
        document.dispatchEvent(new Event("hyp-socket-reconnect"))
      }

      this.isConnected = true
      this.hasEverConnected = true
      this.reconnectDelay = 0
      this.socket.removeEventListener('error', onConnectError)
      document.dispatchEvent(new Event("hyp-socket-connect"))
    })

    sock.addEventListener('close', _ => {
      if (this.isConnected) {
        document.dispatchEvent(new Event("hyp-socket-disconnect"))
      }

      this.isConnected = false

      // attempt to reconnect in 1s
      if (this.hasEverConnected) {
        this.reconnectDelay += 1000
        console.log("Reconnecting in " + (this.reconnectDelay / 1000) + "s")
        setTimeout(() => this.connect(addr), this.reconnectDelay)
      }

    })
  }

  async sendAction(reqId: RequestId, action: ActionMessage): Promise<Response> {
    // console.log("SOCKET sendAction", action)
    let msg = [action.url.pathname + action.url.search
      , "Host: " + window.location.host
      , "Cookie: " + document.cookie
      , "Request-Id: " + reqId
      , action.form
    ].join("\n")
    let { metadata, rest } = await this.fetch(reqId, action.id, msg)

    return {
      requestId: metadata.requestId,
      location: metadata.redirect,
      query: metadata.query,
      body: rest.join('\n')
    }

  }

  async fetch(reqId: RequestId, id: ViewId, msg: string): Promise<ParsedResponse> {
    this.sendMessage(msg)
    let res = await this.waitMessage(reqId, id)
    return res
  }

  private sendMessage(msg: string) {
    this.socket.send(msg)
  }

  private async waitMessage(reqId: RequestId, id: ViewId): Promise<ParsedResponse> {
    return new Promise((resolve, reject) => {
      const onMessage = (event: MessageEvent) => {
        let data: string = event.data

        let parsed = splitMetadata(data.split("\n"))
        let metadata: Metadata = parsed.metadata

        if (!metadata.requestId) {
          console.error("Missing RequestId!", metadata, event.data)
          return
        }

        if (metadata.requestId != reqId) {
          // skip, it's not us!
          return
        }


        // We have found our message. Remove the listener
        this.socket.removeEventListener('message', onMessage)

        // set the cookies. These happen automatically in http
        metadata.cookies.forEach((cookie: string) => {
          document.cookie = cookie
        })

        if (metadata.error) {
          reject(new FetchError(id, metadata.error, parsed.rest.join('\n')))
          return
        }

        resolve(parsed)
      }

      this.socket.addEventListener('message', onMessage)
      this.socket.addEventListener('error', reject)
    })
  }

  disconnect() {
    this.socket.close()
  }
}






