import  { ActionMessage } from './action'
import { takeWhileMap, dropWhile } from "./lib"

const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
const defaultAddress = `${protocol}//${window.location.host}${window.location.pathname}`


export class SocketConnection {


  socket:WebSocket

  isConnected:Boolean

  constructor() {}

  // we need to faithfully transmit the 
  connect(addr = defaultAddress) {
    const sock = new WebSocket(addr)
    this.socket = sock

    function onConnectError(ev:Event) {
      console.log("Connection Error", ev)
    }

    sock.addEventListener('error', onConnectError)

    sock.addEventListener('open', (event) => {
      console.log("Opened", event)
      this.isConnected = true
      this.socket.removeEventListener('error', onConnectError)
    })

    // TODO: Don't reconnet if the socket server is OFF, only if we've successfully connected once
    sock.addEventListener('close', (event) => {
      this.isConnected = false
      console.log("Closed", event)

      // attempt to reconnect in 1s
      setTimeout(() => this.connect(addr), 1000)
    })
  }

  async sendAction(action:ActionMessage):Promise<string> {
    // console.log("SOCKET sendAction", action)
    let msg = [ action.url.pathname + action.url.search
              , "Host: " + window.location.host
              , "Cookie: " + document.cookie
              , action.form
              ].join("\n")
    let ret = await this.fetch(msg)
    let {metadata, rest} = parseMetadataResponse(ret)

    if (metadata.error) {
      throw socketError(metadata.error)
    }

    if (metadata.session) {
      // console.log("setting cookie", metadata.session)
      document.cookie = metadata.session
    }

    if (metadata.redirect) {
      window.location.href = metadata.redirect
    }

    return rest
  }

  async fetch(msg:string):Promise<string> {
    this.sendMessage(msg)
    let res = await this.waitMessage()
    return res
  }

  private sendMessage(msg:string) {
    this.socket.send(msg)
  }

  private async waitMessage():Promise<string> {
    return new Promise((resolve, reject) => {
      const onMessage = (event:MessageEvent) => {
        let data = event.data
        resolve(data)
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

function socketError(inp:string):Error {
    let error = new Error()
    error.name = inp.substring(0, inp.indexOf(' '));
    error.message = inp.substring(inp.indexOf(' ') + 1);
    return error
}


console.log("CONNECTING", window.location)




type SocketResponse = {
  metadata: Metadata,
  rest: string
}

type Metadata = {
  session?: string
  redirect?: string
  error?: string
}

type Meta = {key: string, value: string}


function parseMetadataResponse(ret:string):SocketResponse {
  let lines = ret.split("\n")
  let metas:Metadata = parseMetas(takeWhileMap(parseMeta, lines))
  let rest = dropWhile(parseMeta, lines).join("\n")

  return {
    metadata: metas,
    rest: rest
  }

  function parseMeta(line:string):Meta | undefined {
    let match = line.match(/^\|(\w+)\|(.*)$/)
    if (match) {
      return {
        key: match[1],
        value: match[2]
      }
    }
  }
}

function parseMetas(meta:Meta[]):Metadata {
  return {
    session: meta.find(m => m.key == "SESSION")?.value,
    redirect: meta.find(m => m.key == "REDIRECT")?.value,
    error: meta.find(m => m.key == "ERROR")?.value
  }
}
