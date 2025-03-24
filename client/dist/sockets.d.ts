import { ActionMessage, ViewId, RequestId } from './action';
import { Response, ResponseBody } from "./response";
export declare class SocketConnection {
    socket: WebSocket;
    hasEverConnected: Boolean;
    isConnected: Boolean;
    reconnectDelay: number;
    constructor();
    connect(addr?: string): void;
    sendAction(reqId: RequestId, action: ActionMessage): Promise<Response>;
    fetch(reqId: RequestId, id: ViewId, msg: string): Promise<SocketResponse>;
    private sendMessage;
    private waitMessage;
    disconnect(): void;
}
type SocketResponse = {
    metadata: Metadata;
    body: ResponseBody;
};
type Metadata = {
    viewId?: ViewId;
    cookies: string[];
    redirect?: string;
    error?: string;
    query?: string;
    requestId?: string;
};
export {};
