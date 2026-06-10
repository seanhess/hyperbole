import { type Meta, type ViewId, type RequestId, type ViewState, type EncodedAction } from "./message";
export type ActionMessage = {
    viewId: ViewId;
    action: EncodedAction;
    requestId: RequestId;
    state?: ViewState;
    meta: Meta[];
    form?: FormData;
    value?: InputValue;
};
export type InputValue = string;
export type ActionBody = FormData | InputValue;
export declare function actionMessage(id: ViewId, action: EncodedAction, state: ViewState | undefined, reqId: RequestId, body?: ActionBody): ActionMessage;
export declare function renderActionMessage(msg: ActionMessage): string;
export declare function renderInput(value?: InputValue): string;
export type Request = {
    requestId: RequestId;
    isCancelled: boolean;
};
export declare function newRequest(): Request;
