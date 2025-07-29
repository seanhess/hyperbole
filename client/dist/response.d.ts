import { ViewId } from './action';
export type Response = {
    requestId: string;
    location?: string;
    query?: string;
    body: ResponseBody;
};
export type ResponseBody = string;
export declare function parseResponse(res: ResponseBody): LiveUpdate;
export type LiveUpdate = {
    content: HTMLElement;
    css: HTMLStyleElement | null;
};
export declare class FetchError extends Error {
    viewId: ViewId;
    body: string;
    constructor(viewId: ViewId, msg: string, body: string);
}
