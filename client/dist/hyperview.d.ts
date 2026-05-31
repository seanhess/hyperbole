import { type Request, type ActionBody } from "./action";
export interface HyperView extends HTMLElement {
    runAction(action: string, body?: ActionBody): Promise<void>;
    activeRequest?: Request;
    cancelActiveRequest(): void;
    concurrency: ConcurrencyMode;
    _timeout?: number;
}
export declare const isHyperView: (ele: any) => ele is HyperView;
export type ConcurrencyMode = string;
export declare function dispatchContent(node: HTMLElement): void;
export declare function enrichHyperViews(node: HTMLElement, runAction: (target: HyperView, action: string, body: ActionBody) => Promise<void>): void;
