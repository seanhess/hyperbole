import { ActionMessage, ViewId, RequestId } from './action';
import { Response } from "./response";
import { ParsedResponse } from "./response";
export declare class SocketConnection {
    socket: WebSocket;
    hasEverConnected: Boolean;
    isConnected: Boolean;
    reconnectDelay: number;
    constructor();
    connect(addr?: string): void;
    sendAction(reqId: RequestId, action: ActionMessage): Promise<Response>;
    fetch(reqId: RequestId, id: ViewId, msg: string): Promise<ParsedResponse>;
    private sendMessage;
    private waitMessage;
    disconnect(): void;
}
