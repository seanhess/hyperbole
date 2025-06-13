import { ActionMessage, RequestId } from './action';
import { Response } from "./response";
export declare function sendActionHttp(reqId: RequestId, msg: ActionMessage): Promise<Response>;
