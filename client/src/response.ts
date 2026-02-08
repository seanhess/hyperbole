
import { ViewId, Metadata } from './message'



export type Response = {
  meta: Metadata
  body: ResponseBody
}

export type ResponseBody = string

export function parseResponse(res: ResponseBody): LiveUpdate {
  const parser = new DOMParser()
  const doc = parser.parseFromString(res, 'text/html')
  const css = doc.querySelector<HTMLStyleElement>("style")
  const content = doc.querySelector<HTMLElement>("div")

  return {
    content: content,
    css: css
  }
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
