export function actionUrl(id:string, action:string):URL {
  let url = new URL(window.location.href)
  url.searchParams.append("id", id)
  url.searchParams.append("action", action)
  return url
}

export function toSearch(form?:FormData):URLSearchParams | undefined {
  if (!form) return undefined
    
  const params = new URLSearchParams()

  form.forEach((value, key) => {
    params.append(key, value as string)
  })

  return params
}

export function actionMessage(id:string, action:string, form?:FormData):ActionMessage {
  let url = actionUrl(id, action)
  return { url, form: toSearch(form) }
}

export type ActionMessage = {
  url: URL
  form: URLSearchParams | undefined
}
