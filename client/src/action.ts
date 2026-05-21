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
  form?: FormData
  value?: InputValue
}

export type InputValue = string

export type ActionBody = FormData | InputValue

export function actionMessage(
  id: ViewId,
  action: EncodedAction,
  state: ViewState | undefined,
  reqId: RequestId,
  body?: ActionBody,
): ActionMessage {
  let meta: Meta[] = [
    { key: "Cookie", value: decodeURI(document.cookie) },
    { key: "Query", value: window.location.search },
  ]

  let form = undefined
  let value = undefined
  if (body instanceof FormData) {
    form = body
  } else if (body) {
    value = body as string
  }

  return { viewId: id, action, state, requestId: reqId, meta, form, value }
}

export function renderActionMessage(msg: ActionMessage): string {
  let header = ["|ACTION|", "ViewId: " + msg.viewId, "Action: " + msg.action]

  if (msg.state) {
    header.push("State: " + msg.state)
  }

  header.push("RequestId: " + msg.requestId)

  return [header.join("\n"), message.renderMetas(msg.meta)].join("\n") + renderInput(msg.value)
  // Forms are submitted via fetch() + renderForm(msg.form)
}

export function renderInput(value?: InputValue): string {
  if (!value) return ""
  return "\n\n" + value
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
