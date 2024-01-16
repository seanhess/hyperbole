import  { ActionMessage } from './action'

const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
const defaultAddress = `${protocol}//${window.location.host}${window.location.pathname}`

export class SocketConnection {


  socket:WebSocket

  isConnected:Boolean

  constructor() {}

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

    sock.addEventListener('close', (event) => {
      this.isConnected = false
      console.log("Closed", event)

      // attempt to reconnect in 1s
      setTimeout(() => this.connect(addr), 1000)
    })
  }

  async sendAction(action:ActionMessage):Promise<string> {
    console.log("SOCKET sendAction", action)
    let msg = [action.url.pathname, action.url.search, action.form].join("\n")
    return await this.fetch(msg)
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


console.log("CONNECTING", window.location)




// Save a couple bytes, and make it easier to read
type Command<T> = {
  command: string,
  data: T
}

function parseCommand<T>(message:string): Command<T> {
  const match = message.match(/^(\w+)\s+(.*)$/)

  if (!match) console.error("Could not parse command: ", message)

  return {
    command: match[1],
    data: JSON.parse(match[2])
  }
}
