import { ActionMessage } from './action';
import { Response, Redirect } from "./response";
import { Meta } from "./message";
export declare function sendActionHttp(msg: ActionMessage): Promise<Response | Redirect>;
export declare function splitResponse(body: string): SplitResponse;
export type SplitResponse = {
    metas: Meta[];
    rest: string[];
};
