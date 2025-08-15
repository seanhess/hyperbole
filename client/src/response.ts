
import { ViewId } from './action'
import { takeWhileMap } from "./lib"
import { actionMessage, ActionMessage, Metadata } from "./action"



export type Response = {
  meta: Metadata
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
