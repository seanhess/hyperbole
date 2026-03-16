import {
  type Meta,
  type ViewId,
  type RequestId,
  type ViewState,
  type EncodedAction,
} from "./message"
import * as message from "./message"

export type ActionMessage = {
  viewId: ViewId
  action: EncodedAction
  requestId: RequestId
  state?: ViewState
  meta: Meta[]
  body?: ActionBody
}

export type InputValue = string

export type ActionBody = URLSearchParams | InputValue

export function actionMessage(
  id: ViewId,
  action: EncodedAction,
  state: ViewState | undefined,
  reqId: RequestId,
  body?: ActionBody
): ActionMessage {
  let meta: Meta[] = [
    { key: "Cookie", value: decodeURI(document.cookie) },
    { key: "Query", value: window.location.search },
  ]

  return { viewId: id, action, state, requestId: reqId, meta, body: body }
}

export function toSearch(form?: FormData): URLSearchParams | undefined {
  if (!form) return undefined


  const params = new URLSearchParams()

  console.log("FORM DATA")
  form.forEach((value, key) => {
    console.log(" ", key, "=", value)
    params.append(key, value as string)
  })

  return params
}

export function renderActionMessage(msg: ActionMessage): string {
  let header = ["|ACTION|", "ViewId: " + msg.viewId, "Action: " + msg.action]

  if (msg.state) {
    header.push("State: " + msg.state)
  }

  header.push("RequestId: " + msg.requestId)

  return [
    header.join('\n'),
    message.renderMetas(msg.meta),
  ].join('\n')
  // Forms are submitted via fetch() + renderForm(msg.form)
}

export function renderForm(body?: ActionBody): string {
  if (!body) return ""
  return "\n\n" + body
}

let globalRequestId: RequestId = 0

export type Request = {
  requestId: RequestId
  isCancelled: boolean
}

export function newRequest(): Request {
  let requestId = ++globalRequestId
  return { requestId, isCancelled: false }
}
