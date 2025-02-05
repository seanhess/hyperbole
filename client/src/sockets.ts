import { ActionMessage, ViewId, RequestId } from './action'
import { takeWhileMap, dropWhile } from "./lib"
import { Response, ResponseBody, fetchError } from "./response"

const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
const defaultAddress = `${protocol}//${window.location.host}${window.location.pathname}`



export class SocketConnection {


  socket: WebSocket

  hasEverConnected: Boolean
  isConnected: Boolean
  reconnectDelay: number = 0

  constructor() { }

  // we need to faithfully transmit the 
  connect(addr = defaultAddress) {
    const sock = new WebSocket(addr)
    this.socket = sock

    function onConnectError(ev: Event) {
      console.log("Connection Error", ev)
    }

    sock.addEventListener('error', onConnectError)

    sock.addEventListener('open', (event) => {
      console.log("Opened", event)
      this.isConnected = true
      this.hasEverConnected = true
      this.reconnectDelay = 0
      this.socket.removeEventListener('error', onConnectError)
    })

    // TODO: Don't reconnet if the socket server is OFF, only if we've successfully connected once
    sock.addEventListener('close', _ => {
      this.isConnected = false
      console.log("Closed")

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
    let { metadata, body } = await this.fetch(reqId, action.id, msg)

    return {
      requestId: metadata.requestId,
      location: metadata.redirect,
      query: metadata.query,
      body
    }

  }

  async fetch(reqId: RequestId, id: ViewId, msg: string): Promise<SocketResponse> {
    this.sendMessage(msg)
    let res = await this.waitMessage(reqId, id)
    return res
  }

  private sendMessage(msg: string) {
    this.socket.send(msg)
  }

  private async waitMessage(reqId: RequestId, id: ViewId): Promise<SocketResponse> {
    return new Promise((resolve, reject) => {
      const onMessage = (event: MessageEvent) => {
        let data = event.data

        let { metadata, body } = parseMetadataResponse(data)

        if (!metadata.requestId) {
          console.error("Missing RequestId!", metadata, event.data)
          return
        }

        if (metadata.requestId != reqId) {
          // skip, it's not us!
          return
        }

        if (metadata.error) {
          throw fetchError(metadata.error)
        }

        // We have found our message. Remove the listener
        this.socket.removeEventListener('message', onMessage)

        // set the cookies. These happen automatically in http
        metadata.cookies.forEach(cookie => {
          document.cookie = cookie
        })

        resolve({ metadata, body })
      }

      this.socket.addEventListener('message', onMessage)
      this.socket.addEventListener('error', reject)
    })
  }

  disconnect() {
    this.socket.close()
  }
}

// function socketError(inp: string): Error {
//   let error = new Error()
//   error.name = inp.substring(0, inp.indexOf(' '));
//   error.message = inp.substring(inp.indexOf(' ') + 1);
//   return error
// }






type SocketResponse = {
  metadata: Metadata,
  body: ResponseBody
}

type Metadata = {
  viewId?: ViewId
  cookies: string[]
  redirect?: string
  error?: string
  query?: string
  requestId?: string
}

type Meta = { key: string, value: string }


function parseMetadataResponse(ret: string): SocketResponse {
  let lines = ret.split("\n")
  let metas: Meta[] = takeWhileMap(parseMeta, lines)
  let rest = dropWhile(parseMeta, lines).join("\n")

  return {
    metadata: parseMetas(metas),
    body: rest
  }

  function parseMeta(line: string): Meta | undefined {
    let match = line.match(/^\|([A-Z\-]+)\|(.*)$/)
    if (match) {
      return {
        key: match[1],
        value: match[2]
      }
    }
  }
}

function parseMetas(meta: Meta[]): Metadata {

  let requestId = meta.find(m => m.key == "REQUEST-ID")?.value

  return {
    cookies: meta.filter(m => m.key == "COOKIE").map(m => m.value),
    redirect: meta.find(m => m.key == "REDIRECT")?.value,
    error: meta.find(m => m.key == "ERROR")?.value,
    viewId: meta.find(m => m.key == "VIEW-ID")?.value,
    query: meta.find(m => m.key == "QUERY")?.value,
    requestId
  }
}

