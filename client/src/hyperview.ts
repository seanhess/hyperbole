import { type Request } from "./action";

export interface HyperView extends HTMLElement {
  runAction(action: string): Promise<void>;
  activeRequest?: Request;
  cancelActiveRequest(): void;
  concurrency: ConcurrencyMode;
  _timeout?: number;
}

export const isHyperView = (ele: any): ele is HyperView => {
  return ele?.runAction !== undefined;
};

export type ConcurrencyMode = string;
