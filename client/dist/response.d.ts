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
    css: HTMLStyleElement;
};
export declare function fetchError(msg: string): Error;
