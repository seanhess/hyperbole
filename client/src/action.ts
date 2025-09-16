
import { takeWhileMap } from "./lib"


type EncodedAction = string

export type ActionMessage = {
  viewId: ViewId
  action: EncodedAction
  requestId: RequestId
  meta: Meta[]
  form: URLSearchParams | undefined
}

export type ViewId = string
export type RequestId = string



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
    renderMetadata(msg.meta),
  ].join('\n') + renderForm(msg.form)
}


export function renderForm(form: URLSearchParams | undefined): string {
  if (!form) return ""
  return "\n\n" + form
}


export function requestId(): RequestId {
  return Math.random().toString(36).substring(2, 8)
}


// Metadata -------------------------------------


type Meta = { key: string, value: string }
type RemoteEvent = { name: string, detail: any }

export type Metadata = {
  requestId: string
  cookies: string[]
  redirect?: string
  error?: string
  query?: string
  events?: RemoteEvent[]
  actions?: [ViewId, string][],
  pageTitle?: string
}


export type ParsedResponse = {
  metadata: Metadata,
  rest: string[]
}

export function renderMetadata(meta: Meta[]): string {
  return meta.map(m => m.key + ": " + m.value).join('\n')
}

export function parseMetas(meta: Meta[]): Metadata {

  let requestId = meta.find(m => m.key == "RequestId")?.value

  return {
    cookies: meta.filter(m => m.key == "Cookie").map(m => m.value),
    redirect: meta.find(m => m.key == "Redirect")?.value,
    error: meta.find(m => m.key == "Error")?.value,
    query: meta.find(m => m.key == "Query")?.value,
    events: meta.filter(m => m.key == "Event").map((m) => parseRemoteEvent(m.value)),
    actions: meta.filter(m => m.key == "Trigger").map((m) => parseAction(m.value)),
    pageTitle: meta.find(m => m.key == "PageTitle")?.value,
    requestId
  }
}
// viewId: meta.find(m => m.key == "VIEW-ID")?.value,

// decode entities
export function parseMetadata(input: string): Metadata {
  return splitMetadata(input.trim().split("\n")).metadata

}

export function splitMetadata(lines: string[]): ParsedResponse {
  let metas: Meta[] = takeWhileMap(parseMeta, lines)
  // console.log("Split Metadata", lines.length)
  // console.log(" [0]", lines[0])
  // console.log(" [1]", lines[1])
  let rest = lines.slice(metas.length)

  return {
    metadata: parseMetas(metas),
    rest: rest
  }

}


export function parseRemoteEvent(input: string): RemoteEvent {
  let [name, data] = breakNextSegment(input)
  return {
    name,
    detail: JSON.parse(data)
  }
}

export function parseAction(input: string): [ViewId, string] {
  let [viewId, action] = breakNextSegment(input)
  return [viewId, action]
}

function breakNextSegment(input: string): [string, string] | undefined {
  let ix = input.indexOf('|')
  if (ix === -1) {
    let err = new Error("Bad Encoding, Expected Segment")
    err.message = input
    throw err
  }
  return [input.slice(0, ix), input.slice(ix + 1)]
}

export function parseMeta(line: string): Meta | undefined {
  let match = line.match(/^(\w+)\: (.*)$/)
  if (match) {
    return {
      key: match[1],
      value: match[2]
    }
  }
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
