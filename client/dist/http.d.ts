import { ActionMessage, ParsedResponse } from './action';
import { Response } from "./response";
export declare function sendActionHttp(msg: ActionMessage): Promise<Response>;
export declare function parseMetadataHttp(inp: string): ParsedResponse;
