



export type Response = {
  requestId: string
  location?: string
  query?: string
  error?: string
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
  css: HTMLStyleElement
}
