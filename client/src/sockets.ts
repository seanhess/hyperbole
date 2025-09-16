import { ActionMessage, ViewId, RequestId, renderActionMessage } from './action'
import { Metadata, ParsedResponse, splitMetadata } from "./action"
import { Response, FetchError } from "./response"

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
      this.reconnectDelay = 1000
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
        console.log("Reconnecting in " + (this.reconnectDelay / 1000) + "s")
        setTimeout(() => this.connect(addr), this.reconnectDelay)
      }

    })
  }

  async sendAction(action: ActionMessage): Promise<Response> {
    let msg = renderActionMessage(action)
    let { metadata, rest } = await this.fetch(action.requestId, action.viewId, msg)

    return {
      meta: metadata,
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
        let lines = data.split("\n").slice(1)  // drop the command line

        let parsed = splitMetadata(lines)
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






