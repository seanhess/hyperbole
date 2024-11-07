import { ViewId, Trigger } from './action'


export type Meta = { key: string, value: string }






export function parseMeta(line: string): Meta | undefined {
  let match = line.match(/^\|([A-Z\-]+)\|(.*)$/)
  if (match) {
    return {
      key: match[1],
      value: match[2]
    }
  }
}

export function isMeta(key: string) {
  return (meta: Meta) => meta.key == key
}

export function findMeta(key: string, metas: Meta[]): string | undefined {
  return metas.find(isMeta(key))?.value
}


export function parseMetas(metas: Meta[]): Metadata {
  return {
    session: findMeta("SESSION", metas),
    redirect: findMeta("REDIRECT", metas),
    error: findMeta("ERROR", metas),
    viewId: findMeta("VIEW-ID", metas),
    triggers: metas.filter(isMeta("TRIGGER"))?.map(m => parseTrigger(m.value))
  }
}

export function parseTrigger(value: string): Trigger {
  let [view, action] = value.split("|")
  return { view, action }
}

export type Metadata = {
  viewId?: ViewId
  session?: string
  redirect?: string
  error?: string
  triggers: Trigger[]
}
