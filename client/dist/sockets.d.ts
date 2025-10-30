import { ActionMessage } from './action';
import { ResponseBody } from "./response";
import { ViewId, RequestId, EncodedAction, Metadata } from "./message";
export declare class SocketConnection extends EventTarget {
    socket: WebSocket;
    hasEverConnected: Boolean;
    isConnected: Boolean;
    reconnectDelay: number;
    queue: ActionMessage[];
    connect(addr?: string): void;
    sendAction(action: ActionMessage): Promise<void>;
    private runQueue;
    private onMessage;
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
