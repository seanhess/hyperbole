import { readFileSync } from "fs";
const pkg = JSON.parse(readFileSync("./package.json", "utf-8"));

/** @type {import('vite').UserConfig} */
export default {
  define: {
    __VERSION__: JSON.stringify(pkg.version),
  },
  build: {
    // https://vite.dev/config/build-options#build-lib
    lib: {
      entry: "src/index.ts",
      // Global variable to be accessible via `windows.Hyperbole`
      name: "Hyperbole",
      // Haskell expects `hyperbole.js`, not `Hyperbole.js` nor `hyperbole.iife.js` which Vite would do by default
      fileName: () => "hyperbole.js",
      formats: ["iife"],
    },
    outDir: "dist",
    sourcemap: true,
    minify: "oxc",
  },
};
