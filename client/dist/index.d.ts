declare global {
    interface Window {
        Hyperbole?: HyperboleAPI;
    }
}
export interface HyperboleAPI {
    runAction(target: HTMLElement, action: string, form?: FormData): Promise<void>;
    action(con: string, params: [any]): string;
    hyperView(viewId: ViewId): HyperView | undefined;
}
export interface HyperView extends HTMLElement {
    runAction(target: HTMLElement, action: string, form?: FormData): Promise<void>;
}
export type ViewId = string;
