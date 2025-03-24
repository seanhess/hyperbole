export function actionUrl(id: ViewId, action: string): URL {
  let url = new URL(window.location.href)
  url.searchParams.append("hyp-id", id)
  url.searchParams.append("hyp-action", action)
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


export function inputToAction(action: string, value: string): string {
  return action + ' "' + sanitizeInput(value) + '"'
}

// WARNING: security flaw, unescaped output. no closing quotes allowed?
function sanitizeInput(input: string): string {
  // replace any escape characters: '/' etc
  // replace any quotes with escaped quotes
  return input.replace(/\\/g, "\\\\").replace(/"/g, '\\"')
}


export type RequestId = string

export function requestId(): RequestId {
  return Math.random().toString(36).substring(2, 8)
}
