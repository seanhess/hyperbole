type EncodedAction = string;
export type ActionMessage = {
    viewId: ViewId;
    action: EncodedAction;
    requestId: RequestId;
    meta: Meta[];
    form: URLSearchParams | undefined;
};
export type ViewId = string;
export type RequestId = string;
export declare function actionMessage(id: ViewId, action: EncodedAction, reqId: RequestId, form?: FormData): ActionMessage;
export declare function toSearch(form?: FormData): URLSearchParams | undefined;
export declare function renderActionMessage(msg: ActionMessage): string;
export declare function renderForm(form: URLSearchParams | undefined): string;
export declare function requestId(): RequestId;
type Meta = {
    key: string;
    value: string;
};
type RemoteEvent = {
    name: string;
    detail: any;
};
export type Metadata = {
    requestId: string;
    cookies: string[];
    redirect?: string;
    error?: string;
    query?: string;
    events?: RemoteEvent[];
    actions?: [ViewId, string][];
    pageTitle?: string;
};
export type ParsedResponse = {
    metadata: Metadata;
    rest: string[];
};
export declare function renderMetadata(meta: Meta[]): string;
export declare function parseMetas(meta: Meta[]): Metadata;
export declare function parseMetadata(input: string): Metadata;
export declare function splitMetadata(lines: string[]): ParsedResponse;
export declare function parseRemoteEvent(input: string): RemoteEvent;
export declare function parseAction(input: string): [ViewId, string];
export declare function parseMeta(line: string): Meta | undefined;
export declare function encodedParam(action: string, param: string): string;
export {};
