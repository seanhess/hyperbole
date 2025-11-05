import { Meta, ViewId, RequestId, EncodedAction, ViewState } from "./message";
export type ActionMessage = {
    viewId: ViewId;
    action: EncodedAction;
    requestId: RequestId;
    state?: ViewState;
    meta: Meta[];
    form: URLSearchParams | undefined;
};
export declare function actionMessage(id: ViewId, action: EncodedAction, state: ViewState | undefined, reqId: RequestId, form?: FormData): ActionMessage;
export declare function toSearch(form?: FormData): URLSearchParams | undefined;
export declare function renderActionMessage(msg: ActionMessage): string;
export declare function renderForm(form: URLSearchParams | undefined): string;
export type Request = {
    requestId: RequestId;
    isCancelled: boolean;
};
export declare function newRequest(): Request;
export declare function encodedParam(action: string, param: string): string;
