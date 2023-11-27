declare module 'omdomdom/lib/omdomdom.es.js' {
  export function create(node: any, ...args: any[]): any;
  export function patch(template: any, vNode: any, rootNode?: any): void;
  export function render(vNode: any, root: any): void;
}

