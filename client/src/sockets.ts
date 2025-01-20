import { ActionMessage, ViewId } from './action'
import { takeWhileMap, dropWhile } from "./lib"
import { setQuery } from "./browser"

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

  async sendAction(action: ActionMessage): Promise<string> {
    // console.log("SOCKET sendAction", action)
    let msg = [action.url.pathname + action.url.search
      , "Host: " + window.location.host
      , "Cookie: " + document.cookie
      , action.form
    ].join("\n")
    let { metadata, rest } = await this.fetch(action.id, msg)


    return rest
  }

  async fetch(id: ViewId, msg: string): Promise<SocketResponse> {
    this.sendMessage(msg)
    let res = await this.waitMessage(id)
    return res
  }

  private sendMessage(msg: string) {
    this.socket.send(msg)
  }

  private async waitMessage(id: ViewId): Promise<SocketResponse> {
    return new Promise((resolve, reject) => {
      const onMessage = (event: MessageEvent) => {
        let data = event.data

        let { metadata, rest } = parseMetadataResponse(data)

        // console.log("META", metadata)

        if (metadata.error) {
          throw socketError(metadata.error)
        }


        metadata.cookies.forEach(cookie => {
          document.cookie = cookie
        })

        if (metadata.redirect) {
          window.location.href = metadata.redirect
          return
        }

        setQuery(metadata.query)

        if (metadata.viewId != id) {
          // console.warn("Mismatched ids, ignoring", metadata.viewId, id)
          return
        }

        resolve({ metadata, rest })

        // we should only resolve if we have found our message
        this.socket.removeEventListener('message', onMessage)
      }

      this.socket.addEventListener('message', onMessage)
      this.socket.addEventListener('error', reject)
    })
  }

  disconnect() {
    this.socket.close()
  }
}

function socketError(inp: string): Error {
  let error = new Error()
  error.name = inp.substring(0, inp.indexOf(' '));
  error.message = inp.substring(inp.indexOf(' ') + 1);
  return error
}






type SocketResponse = {
  metadata: Metadata,
  rest: string
}

type Metadata = {
  viewId?: ViewId
  cookies: string[]
  redirect?: string
  error?: string
  query?: string
}

type Meta = { key: string, value: string }


function parseMetadataResponse(ret: string): SocketResponse {
  let lines = ret.split("\n")
  let metas: Meta[] = takeWhileMap(parseMeta, lines)
  let rest = dropWhile(parseMeta, lines).join("\n")

  return {
    metadata: parseMetas(metas),
    rest: rest
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
  return {
    cookies: meta.filter(m => m.key == "COOKIE").map(m => m.value),
    redirect: meta.find(m => m.key == "REDIRECT")?.value,
    error: meta.find(m => m.key == "ERROR")?.value,
    viewId: meta.find(m => m.key == "VIEW-ID")?.value,
    query: meta.find(m => m.key == "QUERY")?.value
  }
}
