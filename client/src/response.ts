
import { ViewId, Metadata, Meta, metaValue, toMetadata, RequestId, EncodedAction } from './message'




export type ResponseBody = string


export function parseResponseDocument(res: ResponseBody): Document {
  const parser = new DOMParser()
  return parser.parseFromString(res, 'text/html')
}

export function documentUpdate(doc: Document): LiveUpdate {
  const css = doc.querySelector<HTMLStyleElement>("style")
  const content = doc.querySelector<HTMLElement>("div")

  return {
    content: content,
    css: css
  }
}

export type BaseResponse = {
  requestId: RequestId
  meta: Metadata
  viewId: ViewId
  targetViewId?: ViewId
  action: EncodedAction
  body: ResponseBody
}

export type Response = BaseResponse & {
  kind: 'response'
}

export type Update = BaseResponse & {
  kind: 'update'
  targetViewId: ViewId
}

export type Redirect = {
  kind: 'redirect'
  requestId: RequestId
  meta: Metadata
  url: string
}


export function parseUpdate(metas: Meta[], rest: string[]): Update {
  let { requestId, viewId, action, meta, body } = parseResponse(metas, rest)

  return {
    kind: 'update',
    targetViewId: requireMeta(rest, metas, "TargetViewId"),
    requestId,
    viewId,
    action,
    meta,
    body
  }
}

export function parseResponse(metas: Meta[], rest: string[]): Response {
  let viewId = requireMeta(rest, metas, "ViewId")
  let action = requireMeta(rest, metas, "Action")
  let requestId = parseInt(requireMeta(rest, metas, "RequestId"), 0)
  return {
    kind: 'response',
    requestId,
    targetViewId: undefined, // only set on push update
    viewId,
    action,
    meta: toMetadata(metas),
    body: rest.join("\n"),
  }
}

export function parseRedirect(metas: Meta[], rest: string[]): Redirect {
  let url = rest[0]
  let requestId = parseInt(requireMeta(rest, metas, "RequestId"), 0)
  return {
    kind: 'redirect',
    requestId,
    meta: toMetadata(metas),
    url
  }
}


export function requireMeta(lines: string[], metas: Meta[], key: string): string {
  let val = metaValue(key, metas)
  if (!val) throw new ProtocolError("Missing Required Metadata: " + key, lines.join('\n'))
  return val
}


export type LiveUpdate = {
  content: HTMLElement | null
  css: HTMLStyleElement | null
}


export class FetchError extends Error {
  viewId: ViewId
  body: string
  constructor(viewId: ViewId, msg: string, body: string) {
    super(msg)
    this.viewId = viewId
    this.name = "Fetch Error"
    this.body = body
  }
}

export class ProtocolError extends Error {
  constructor(description: string, body: string) {
    super(description + "\n" + body)
    this.name = "ProtocolError"
  }
}
