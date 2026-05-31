import { type ActionMessage } from "./action";
import { type Response, type Redirect } from "./response";
import { type Meta } from "./message";
export declare function sendActionHttp(msg: ActionMessage): Promise<Response | Redirect>;
export declare function splitResponse(body: string): SplitResponse;
export type SplitResponse = {
    metas: Meta[];
    rest: string[];
};
