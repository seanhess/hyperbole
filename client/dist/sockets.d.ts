import { ActionMessage } from './action';
import { ResponseBody } from "./response";
import { ViewId, RequestId, EncodedAction, Metadata } from "./message";
export declare class SocketConnection {
    socket: WebSocket;
    hasEverConnected: Boolean;
    isConnected: Boolean;
    reconnectDelay: number;
    queue: ActionMessage[];
    events: EventTarget;
    constructor();
    connect(addr?: string): void;
    sendAction(action: ActionMessage): Promise<void>;
    private runQueue;
    private onMessage;
    addEventListener(e: string, cb: EventListenerOrEventListenerObject): void;
    dispatchEvent(e: Event): void;
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
    url: string;
};
export type MessageType = string;
export declare class ProtocolError extends Error {
    constructor(description: string, body: string);
}
