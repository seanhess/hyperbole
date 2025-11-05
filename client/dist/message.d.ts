export type Meta = {
    key: string;
    value: string;
};
export type ViewId = string;
export type RequestId = number;
export type EncodedAction = string;
export type ViewState = string;
type RemoteEvent = {
    name: string;
    detail: any;
};
export declare function renderMetas(meta: Meta[]): string;
export type Metadata = {
    cookies?: string[];
    error?: string;
    query?: string;
    events?: RemoteEvent[];
    actions?: [ViewId, string][];
    pageTitle?: string;
};
export declare function toMetadata(meta: Meta[]): Metadata;
export declare function parseMetadata(input: string): Metadata;
export declare function metaValue(key: string, metas: Meta[]): string | undefined;
export declare function metaValuesAll(key: string, metas: Meta[]): string[];
export type SplitMessage = {
    command: string;
    metas: Meta[];
    rest: string[];
};
export declare function splitMessage(message: string): SplitMessage;
export declare function parseMeta(line: string): Meta | undefined;
export declare function parseRemoteEvent(input: string): RemoteEvent;
export declare function parseAction(input: string): [ViewId, string];
export {};
