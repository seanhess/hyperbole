import { ActionMessage } from './action'
import { FetchError, Response, Redirect, parseResponseDocument, parseResponse } from "./response"
import { Metadata, parseMetadata, Meta, parseMetas } from "./message"

// Could be a redirect or response
export async function sendActionHttp(msg: ActionMessage): Promise<Response | Redirect> {
  // console.log("HTTP sendAction", msg.url.toString())
  let url = window.location.href

  let headers: any = {
    "Accept": "text/html",
    "Hyp-ViewId": msg.viewId,
    "Hyp-RequestId": msg.requestId,
    "Hyp-Action": msg.action,
    "Hyp-State": msg.state || "()"
  }

  let res = await fetch(url, {
    method: "POST",
    headers: headers,
    body: msg.form,
    // we never want this to be redirected
    redirect: "manual"
  })


  let body = await res.text()

  // TODO: handle redirects. Check status code and do it
  if (!res.ok) {
    throw new FetchError(msg.viewId, "Invalid Status:" + res.status, body)
  }

  const { metas, rest } = splitResponse(body)
  const response = parseResponse(metas, rest)
  return response
}

export function splitResponse(body: string): SplitResponse {
  let lines = body.split("\n")
  // drop the <script> start line
  let metas = parseMetas(lines.slice(1))
  let rest = lines.slice(2)
  // drop the </script> end line and 2x whitespace
  return { metas, rest }
}

export type SplitResponse = {
  metas: Meta[],
  rest: string[]
}


// export function documentScriptMeta(doc: Document): Metadata {
//   const meta = doc.querySelector<HTMLScriptElement>("script#hyp.metadata")
//   return parseMetadata(meta?.textContent || "")
// }
