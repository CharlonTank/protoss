"use strict";

const assert = require("assert");
const { looksLikeHuman, planSyntaxSwitch, stripDashComment } = require("../extension.js");

// --- stripDashComment: only `--` is a comment marker, never inside strings ---

assert.strictEqual(stripDashComment("add a b = a -- comment"), "add a b = a ");
assert.strictEqual(stripDashComment("x = \"a--b\""), "x = \"a--b\"");
assert.strictEqual(stripDashComment("; sexp comment = kept"), "; sexp comment = kept");

// --- looksLikeHuman: mirror of Elm_syntax.looks_like ---

const kernelText = [
  "; a kernel file",
  "(def Nat.add",
  "  (lam a (lam b (foldNat a b (lam r (succ r))))))"
].join("\n");
assert.strictEqual(looksLikeHuman(kernelText), false, "kernel sexp is not human");

const humanText = [
  "-- a human file",
  "add : Nat -> Nat -> Nat",
  "add a b = a"
].join("\n");
assert.strictEqual(looksLikeHuman(humanText), true, "signature/value lines are human");

assert.strictEqual(looksLikeHuman("module Foo exposing Bar"), true, "module line is human");
assert.strictEqual(looksLikeHuman("-- add = 1"), false, "commented value line is not human");
assert.strictEqual(looksLikeHuman(""), false, "empty text is not human");
assert.strictEqual(
  looksLikeHuman("(def x (succ zero))\n(type T Nat)"),
  false,
  "pure sexp forms are not human"
);

// --- planSyntaxSwitch: the toggle restore planner ---

const K = "(def x (succ zero))\n";
const H = "x : Nat\nx = succ zero\n";
const handWrittenHuman = "x : Nat\n\n\nx   =   succ zero  -- hand layout\n";
const memory = { textA: handWrittenHuman, humanA: true, textB: K, humanB: false };

// The core promise: human -> kernel -> human restores the original bytes.
assert.strictEqual(planSyntaxSwitch(memory, K, true), handWrittenHuman, "opposite switch restores");

// Re-applying the stored direction is also served from memory (ping-pong).
assert.strictEqual(planSyntaxSwitch(memory, handWrittenHuman, false), K, "redo is served from memory");

// Same-direction request on the produced side falls through to the CLI.
assert.strictEqual(planSyntaxSwitch(memory, K, false), null, "same direction is not a restore");
assert.strictEqual(planSyntaxSwitch(memory, handWrittenHuman, true), null, "same direction is not a restore");

// Any edit in between expires the stash.
assert.strictEqual(planSyntaxSwitch(memory, K + "\n; edited", true), null, "edited buffer expires stash");

// No memory, or a degenerate no-op pair, never restores.
assert.strictEqual(planSyntaxSwitch(undefined, K, true), null, "no memory");
assert.strictEqual(
  planSyntaxSwitch({ textA: K, humanA: false, textB: K, humanB: false }, K, true),
  null,
  "no-op pair never restores"
);

// Kernel-canonicalization stash (non-canonical kernel -> canonical kernel):
// both sides are kernel, so a toHuman request must not restore either side.
const roughKernel = "(def x   (succ zero))\n";
const kernelPair = { textA: roughKernel, humanA: false, textB: K, humanB: false };
assert.strictEqual(planSyntaxSwitch(kernelPair, K, true), null, "kernel pair never serves toHuman");
assert.strictEqual(planSyntaxSwitch(kernelPair, K, false), null, "original side was not requested syntax");

console.log("protoss switch tests ok");
