export type UrlFragment = string;
export declare function listenKeydown(cb: (target: HTMLElement, action: string) => void): void;
export declare function listenKeyup(cb: (target: HTMLElement, action: string) => void): void;
export declare function listenKeyEvent(event: string, cb: (target: HTMLElement, action: string) => void): void;
export declare function listenClick(cb: (target: HTMLElement, action: string) => void): void;
export declare function listenDblClick(cb: (target: HTMLElement, action: string) => void): void;
export declare function listenLoadDocument(cb: (target: HTMLElement, action: string) => void): void;
export declare function listenLoad(node: HTMLElement): void;
export declare function listenChange(cb: (target: HTMLElement, action: string) => void): void;
export declare function listenInput(cb: (target: HTMLElement, action: string) => void): void;
export declare function listenFormSubmit(cb: (target: HTMLElement, action: string, form: FormData) => void): void;
