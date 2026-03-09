import { SocketConnection } from './sockets';
import { ViewId, Metadata } from './message';
import { HyperView } from "./hyperview";
declare global {
    interface Window {
        Hyperbole?: HyperboleAPI;
    }
    interface DocumentEventMap {
        "hyp-load": CustomEvent;
        "hyp-mouseenter": CustomEvent;
        "hyp-mouseleave": CustomEvent;
    }
}
export interface HyperboleAPI {
    runAction(target: HTMLElement, action: string, form?: FormData): Promise<void>;
    action(con: string, ...params: any[]): string;
    hyperView(viewId: ViewId): HyperView | undefined;
    parseMetadata(input: string): Metadata;
    socket: SocketConnection;
}
