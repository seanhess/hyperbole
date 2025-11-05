
import { takeWhileMap, dropWhile } from "./lib"



export type Meta = { key: string, value: string }
export type ViewId = string
export type RequestId = number
export type EncodedAction = string
export type ViewState = string

type RemoteEvent = { name: string, detail: any }


export function renderMetas(meta: Meta[]): string {
  return meta.map(m => m.key + ": " + m.value).join('\n')
}

export type Metadata = {
  cookies?: string[]
  // redirect?: string
  error?: string
  query?: string
  events?: RemoteEvent[]
  actions?: [ViewId, string][],
  pageTitle?: string
}


export function toMetadata(meta: Meta[]): Metadata {

  return {
    cookies: meta.filter(m => m.key == "Cookie").map(m => m.value),
    // redirect: metaValue("Redirect", meta),
    error: metaValue("Error", meta),
    query: metaValue("Query", meta),
    pageTitle: metaValue("PageTitle", meta),
    events: metaValuesAll("Event", meta).map(parseRemoteEvent),
    actions: metaValuesAll("Trigger", meta).map(parseAction),
  }
}

// viewId: meta.find(m => m.key == "VIEW-ID")?.value,

export function parseMetadata(input: string): Metadata {
  let metas = takeWhileMap(parseMeta, input.trim().split("\n"))
  return toMetadata(metas)
}


export function metaValue(key: string, metas: Meta[]): string | undefined {
  return metas.find(m => m.key == key)?.value
}

export function metaValuesAll(key: string, metas: Meta[]): string[] {
  return metas.filter(m => m.key == key).map(m => m.value)
}

export type SplitMessage = {
  command: string,
  metas: Meta[],
  rest: string[]
}


export function splitMessage(message: string): SplitMessage {
  let lines = message.split("\n")
  let command: string = lines[0]
  let metas: Meta[] = takeWhileMap(parseMeta, lines.slice(1))
  // console.log("Split Metadata", lines.length)
  // console.log(" [0]", lines[0])
  // console.log(" [1]", lines[1])
  let rest = dropWhile(l => l == "", lines.slice(metas.length + 1))

  return { command, metas, rest }
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

