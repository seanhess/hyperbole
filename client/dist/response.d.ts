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
export type Metadata = {
    viewId?: ViewId;
    cookies: string[];
    redirect?: string;
    error?: string;
    query?: string;
    requestId?: string;
};
type Meta = {
    key: string;
    value: string;
};
export declare function parseMetas(meta: Meta[]): Metadata;
export declare function parseMetadata(input: string): Metadata;
export declare function splitMetadata(lines: string[]): ParsedResponse;
export declare function parseMeta(line: string): Meta | undefined;
export type ParsedResponse = {
    metadata: Metadata;
    rest: string[];
};
export {};
