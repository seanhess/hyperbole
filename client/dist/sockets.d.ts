import { ActionMessage } from './action';
import { Update, Redirect, Response } from "./response";
import { Meta } from "./message";
interface SocketConnectionEventMap {
    "update": CustomEvent<Update>;
    "response": CustomEvent<Response>;
    "redirect": CustomEvent<Redirect>;
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
export type MessageType = string;
export type SplitMessage = {
    command: string;
    metas: Meta[];
    rest: string[];
};
export declare function splitMessage(msg: string): SplitMessage;
export {};
