
import { ViewId } from './action'
import { takeWhileMap } from "./lib"



export type Response = {
  requestId: string
  location?: string
  query?: string
  body: ResponseBody
}

export type ResponseBody = string

export function parseResponse(res: ResponseBody): LiveUpdate {
  const parser = new DOMParser()
  const doc = parser.parseFromString(res, 'text/html')
  const css = doc.querySelector("style") as HTMLStyleElement
  const content = doc.querySelector("div") as HTMLElement

  return {
    content: content,
    css: css
  }
}

export type LiveUpdate = {
  content: HTMLElement
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

export type Metadata = {
  viewId?: ViewId
  cookies: string[]
  redirect?: string
  error?: string
  query?: string
  requestId?: string
}

type Meta = { key: string, value: string }



export function parseMetas(meta: Meta[]): Metadata {

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


export function parseMeta(line: string): Meta | undefined {
  let match = line.match(/^\|([A-Z\-]+)\|(.*)$/)
  if (match) {
    return {
      key: match[1],
      value: match[2]
    }
  }
}


export type ParsedResponse = {
  metadata: Metadata,
  rest: string[]
}
