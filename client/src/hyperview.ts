import { type Request } from "./action"

export interface HyperView extends HTMLElement {
  runAction(action: string): Promise<void>
  activeRequest?: Request
  cancelActiveRequest(): void
  concurrency: ConcurrencyMode
  _timeout?: number
}

export const isHyperView = (ele: any): ele is HyperView => {
  return ele?.runAction !== undefined
}

export type ConcurrencyMode = string

export function dispatchContent(node: HTMLElement): void {
  let event = new Event("hyp-content", { bubbles: true })
  node.dispatchEvent(event)
}

export function enrichHyperViews(
  node: HTMLElement,
  runAction: (target: HyperView, action: string, form?: FormData) => Promise<void>,
): void {
  // enrich all the hyperviews
  node.querySelectorAll<HyperView>("[id]").forEach((element) => {
    element.runAction = function (action: string) {
      return runAction(element, action)
    }

    element.concurrency = element.dataset.concurrency || "Drop"

    element.cancelActiveRequest = function () {
      if (element.activeRequest && !element.activeRequest?.isCancelled) {
        element.activeRequest.isCancelled = true
      }
    }

    dispatchContent(node)
  })
}
