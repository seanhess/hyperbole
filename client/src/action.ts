
import { takeWhileMap } from "./lib"
import { Meta, ViewId, RequestId, EncodedAction } from "./message"
import * as message from "./message"



export type ActionMessage = {
  viewId: ViewId
  action: EncodedAction
  requestId: RequestId
  meta: Meta[]
  form: URLSearchParams | undefined
}

export type ViewId = string
export type RequestId = number



export function actionMessage(id: ViewId, action: EncodedAction, reqId: RequestId, form?: FormData): ActionMessage {
  let meta: Meta[] = [
    { key: "Cookie", value: decodeURI(document.cookie) },
    { key: "Query", value: window.location.search }
  ]

  return { viewId: id, action, requestId: reqId, meta, form: toSearch(form) }
}

export function toSearch(form?: FormData): URLSearchParams | undefined {
  if (!form) return undefined

  const params = new URLSearchParams()

  form.forEach((value, key) => {
    params.append(key, value as string)
  })

  return params
}

export function renderActionMessage(msg: ActionMessage): string {
  let header = [
    "|ACTION|",
    "ViewId: " + msg.viewId,
    "Action: " + msg.action,
    "RequestId: " + msg.requestId
  ]


  return [
    header.join('\n'),
    message.renderMetas(msg.meta),
  ].join('\n') + renderForm(msg.form)
}


export function renderForm(form: URLSearchParams | undefined): string {
  if (!form) return ""
  return "\n\n" + form
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



// Sanitized Encoding ------------------------------------

export function encodedParam(action: string, param: string): string {
  return action + ' ' + sanitizeParam(param)
}

function sanitizeParam(param: string): string {
  if (param == "") {
    return "|"
  }

  return param.replace(/_/g, "\\_").replace(/\s+/g, "_")
}
