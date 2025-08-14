import { ActionMessage, RequestId } from './action';
import { Response, ParsedResponse } from "./response";
export declare function sendActionHttp(reqId: RequestId, msg: ActionMessage): Promise<Response>;
export declare function parseMetadataHttp(inp: string): ParsedResponse;
