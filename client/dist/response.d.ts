import { ViewId, Metadata, Meta, RequestId, EncodedAction } from './message';
export type ResponseBody = string;
export declare function parseResponseDocument(res: ResponseBody): Document;
export declare function documentUpdate(doc: Document): LiveUpdate;
export type BaseResponse = {
    requestId: RequestId;
    meta: Metadata;
    viewId: ViewId;
    targetViewId?: ViewId;
    action: EncodedAction;
    body: ResponseBody;
};
export type Response = BaseResponse & {
    kind: 'response';
};
export type Update = BaseResponse & {
    kind: 'update';
    targetViewId: ViewId;
};
export type Redirect = {
    kind: 'redirect';
    requestId: RequestId;
    meta: Metadata;
    url: string;
};
export declare function parseUpdate(metas: Meta[], rest: string[]): Update;
export declare function parseResponse(metas: Meta[], rest: string[]): Response;
export declare function parseRedirect(metas: Meta[], rest: string[]): Redirect;
export declare function requireMeta(lines: string[], metas: Meta[], key: string): string;
export type LiveUpdate = {
    content: HTMLElement | null;
    css: HTMLStyleElement | null;
};
export declare class FetchError extends Error {
    viewId: ViewId;
    body: string;
    constructor(viewId: ViewId, msg: string, body: string);
}
export declare class ProtocolError extends Error {
    constructor(description: string, body: string);
}
