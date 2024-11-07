
export function actionUrl(id: ViewId, action: string): URL {
  let url = new URL(window.location.href)
  url.searchParams.append("id", id)
  url.searchParams.append("action", action)
  return url
}

export function toSearch(form?: FormData): URLSearchParams | undefined {
  if (!form) return undefined

  const params = new URLSearchParams()

  form.forEach((value, key) => {
    params.append(key, value as string)
  })

  return params
}

export function actionMessage(id: ViewId, action: string, form?: FormData): ActionMessage {
  let url = actionUrl(id, action)
  return { id, url, form: toSearch(form) }
}

export type ActionMessage = {
  id: ViewId
  url: URL
  form: URLSearchParams | undefined
}

export type ViewId = string

export type Trigger = { view: ViewId, action: string }

export type ActionResponse = {
  content: string
  triggers: Trigger[]
}


export type Response = {
  content: HTMLElement
  css: HTMLStyleElement
  triggers: Trigger[]
}

export function parseResponse(resp: ActionResponse): Response {
  const parser = new DOMParser()
  const doc = parser.parseFromString(resp.content, 'text/html')
  const css = doc.querySelector("style") as HTMLStyleElement
  const content = doc.querySelector("div") as HTMLElement

  return {
    content: content,
    css: css,
    triggers: resp.triggers
  }
}
