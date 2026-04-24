import { ActionMessage } from './action';
import { ResponseBody } from "./response";
import { ViewId, RequestId, EncodedAction, Metadata, RemoteEvent } from "./message";
interface SocketConnectionEventMap {
    "update": CustomEvent<Update>;
    "response": CustomEvent<Update>;
    "redirect": CustomEvent<Redirect>;
    "trigger": CustomEvent<Trigger>;
    "event": CustomEvent<JSEvent>;
}
export declare class SocketConnection {
    socket: WebSocket;
    hasEverConnected: Boolean;
    isConnected: Boolean;
    reconnectDelay: number;
    queue: ActionMessage[];
    events: EventTarget;
    constructor(addr?: string);
    connect(addr?: string, createSocket?: boolean): void;
    sendAction(action: ActionMessage): Promise<void>;
    private runQueue;
    private onMessage;
    addEventListener<K extends keyof SocketConnectionEventMap>(e: K, cb: (ev: SocketConnectionEventMap[K]) => void): void;
    dispatchEvent<K extends keyof SocketConnectionEventMap>(e: SocketConnectionEventMap[K]): void;
    disconnect(): void;
}
export type Update = {
    requestId: RequestId;
    meta: Metadata;
    viewId: ViewId;
    targetViewId?: ViewId;
    action: EncodedAction;
    body: ResponseBody;
};
export type Redirect = {
    requestId: RequestId;
    meta: Metadata;
    url: string;
};
export type Trigger = {
    requestId: RequestId;
    meta: Metadata;
    viewId: ViewId;
    action: EncodedAction;
    targetViewId: ViewId;
    targetAction: string;
};
export type JSEvent = {
    requestId: RequestId;
    meta: Metadata;
    viewId: ViewId;
    action: EncodedAction;
    event: RemoteEvent;
};
export type MessageType = string;
export declare class ProtocolError extends Error {
    constructor(description: string, body: string);
}
export {};
