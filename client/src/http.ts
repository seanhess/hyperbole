import { ActionMessage, RequestId } from './action'
import { Response, FetchError, Metadata, ParsedResponse, splitMetadata } from "./response"

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
  let { metadata, rest } = parseMetadataHttp(body)

  if (!res.ok) {
    throw new FetchError(msg.id, body, body)
  }

  let response: Response = {
    meta: metadata,
    body: rest.join('\n')
  }

  return response
}


export function parseMetadataHttp(inp: string): ParsedResponse {
  let lines = inp.split("\n")
  // drop the <script> start line
  let { metadata, rest } = splitMetadata(lines.slice(1))
  // drop the </script> end line and 2x whitespace
  return { metadata, rest: rest.slice(3) }
}


