import { ViewId } from './action';
import { Metadata } from './response';
declare global {
    interface Window {
        Hyperbole?: HyperboleAPI;
    }
}
export interface HyperboleAPI {
    runAction(target: HTMLElement, action: string, form?: FormData): Promise<void>;
    action(con: string, ...params: any[]): string;
    hyperView(viewId: ViewId): HyperView | undefined;
    parseMetadata(input: string): Metadata;
}
export interface HyperView extends HTMLElement {
    runAction(target: HTMLElement, action: string, form?: FormData): Promise<void>;
}
