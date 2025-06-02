export declare function actionUrl(id: ViewId, action: string): URL;
export declare function toSearch(form?: FormData): URLSearchParams | undefined;
export declare function actionMessage(id: ViewId, action: string, form?: FormData): ActionMessage;
export type ActionMessage = {
    id: ViewId;
    url: URL;
    form: URLSearchParams | undefined;
};
export type ViewId = string;
export declare function encodedTextInput(action: string, value: string): string;
export declare function encodedJSONInput(action: string, value: string): string;
export type RequestId = string;
export declare function requestId(): RequestId;
