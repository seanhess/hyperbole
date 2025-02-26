import { ActionMessage, RequestId } from './action'
import { Response, fetchError } from "./response"

export async function sendActionHttp(reqId: RequestId, msg: ActionMessage): Promise<Response> {
  // console.log("HTTP sendAction", msg.url.toString())
  let res = await fetch(msg.url, {
    method: "POST",
    headers:
    {
      'Accept': 'text/html',
      'Content-Type': 'application/x-www-form-urlencoded',
      'Request-Id': reqId
    },
    body: msg.form,
    // we never want this to be redirected
    redirect: "manual"
  })

  let body = await res.text()

  if (!res.ok) {
    throw fetchError(body)
  }

  let response: Response = {
    requestId: res.headers.get("Request-Id"),
    location: res.headers.get("location"),
    query: res.headers.get("set-query"),
    body
  }

  return response
}


