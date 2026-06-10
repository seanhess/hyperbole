TypeScript
=================

Hyperbole comes with a [JavaScript API](https://hyperbole.live/interactivity#javascript). All `*.js` sources are already included in Hyperbole's Haskell package.  

However, to get Hyperbole's TypeScript definition files (`*.d.ts`) into your Hyperbole project, two more steps are needed:

1. Install Hyperbole's `*.d.ts` files from its GitHub repository (Note: They are *not* published to `npm`).

```sh
# npm
npm install -D github:seanhess/hyperbole

# pnpm
pnpm add -D github:seanhess/hyperbole

# bun
bun add -D github:seanhess/hyperbole
```

2. Create a `globals.d.ts` file in the same folder as your client (TypeScript) sources are located. 

```ts
/// <reference types="hyperbole-client-types" />
```

That's all. Now you get full type safety for `Hyperbole`'s API within your TypeScript code.
