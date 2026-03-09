import { type Request } from "./action";
export interface HyperView extends HTMLElement {
    runAction(action: string): Promise<void>;
    activeRequest?: Request;
    cancelActiveRequest(): void;
    concurrency: ConcurrencyMode;
    _timeout?: number;
}
export declare const isHyperView: (ele: any) => ele is HyperView;
export type ConcurrencyMode = string;
export declare function dispatchContent(node: HTMLElement): void;
export declare function enrichHyperViews(node: HTMLElement, runAction: (target: HyperView, action: string, form?: FormData) => Promise<void>): void;
