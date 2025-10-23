import { ActionMessage, splitMetadata, ParsedResponse } from './action'
import { Response, FetchError } from "./response"

export async function sendActionHttp(msg: ActionMessage): Promise<Response> {
  // console.log("HTTP sendAction", msg.url.toString())
  let url = window.location.href
  let res = await fetch(url, {
    method: "POST",
    headers:
    {
      'Accept': 'text/html',
      'Content-Type': 'application/x-www-form-urlencoded',
      'Hyp-RequestId': msg.requestId.toString(),
      'Hyp-ViewId': msg.viewId,
      'Hyp-Action': msg.action
    },
    body: msg.form,
    // we never want this to be redirected
    redirect: "manual"
  })

  let body = await res.text()
  let { metadata, rest } = parseMetadataHttp(body)

  if (!res.ok) {
    throw new FetchError(msg.viewId, body, body)
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
  return { metadata, rest: rest.slice(2) }
}


