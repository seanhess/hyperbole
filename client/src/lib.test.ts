import { describe, expect, it } from "vitest";
import { takeWhileMap } from "./lib";

// This is just a single test to demonstrate how to run tests.
// TODO: add tests for all lib functions

describe("takeWhileMap", () => {
  it("collects even numbers until an odd one is found", () => {
    const result = takeWhileMap((n: number) => (n % 2 === 0 ? n : undefined), [2, 4, 6, 7, 8]);
    expect(result).toEqual([2, 4, 6]);
  });
});
