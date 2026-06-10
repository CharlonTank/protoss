"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const {
  findDefinitionInText,
  scanDefinitions,
  stripLineComment,
  symbolAtTextOffset
} = require("../extension");

const elmLike = `import "../stdlib/prelude.protoss"

type alias Model =
    { count : Nat
    }

add a b =
    a + b

total : Nat
total =
    add 2 5

view model =
    text model.count
`;

const sexp = `(def Nat.add (-> Nat (-> Nat Nat))
  (lambda (a Nat)
    (lambda (b Nat)
      (foldNat a b (lambda (acc Nat) (succ acc))))))

(record Box (value Nat))

(def total Nat ((Nat.add 2) 5))
`;

const elmPipeline = `double : Nat -> Nat
double x =
    x |> Nat.add x

main : Nat
main =
    2
        |> double
        |> succ

withLet : Nat
withLet =
    let
        base = 2
        bump value = value |> double
    in
        bumped |> succ

user =
    { name = "-- not comment"
    , active = true
    }
`;

assert.deepStrictEqual(
  scanDefinitions(elmLike).map((def) => def.name),
  ["Model", "add", "total", "view"]
);
assert.strictEqual(findDefinitionInText(elmLike, "add").line, 6);
assert.strictEqual(findDefinitionInText(elmLike, "total").line, 9);
assert.strictEqual(findDefinitionInText(elmLike, "Model").kind, "type");
assert.strictEqual(findDefinitionInText(sexp, "Nat.add").line, 0);
assert.strictEqual(findDefinitionInText(sexp, "Box").kind, "type");
assert.strictEqual(stripLineComment("text \"-- not comment\" -- comment").trim(), "text \"-- not comment\"");
assert.strictEqual(stripLineComment("text \"; not comment\" ; comment").trim(), "text \"; not comment\"");
assert.strictEqual(stripLineComment("(def a Nat 1) ; comment").trim(), "(def a Nat 1)");
assert.strictEqual(symbolAtTextOffset("    inferredAdd 2 5", 8), "inferredAdd");
assert.deepStrictEqual(
  scanDefinitions(elmPipeline).map((def) => def.name),
  ["double", "main", "withLet", "user"]
);
assert.strictEqual(findDefinitionInText(elmPipeline, "double").line, 0);
assert.strictEqual(symbolAtTextOffset("        |> double", 12), "double");
assert.strictEqual(symbolAtTextOffset("        |> succ", 12), "succ");

const builtins = fs.readFileSync(path.join(__dirname, "..", "builtins.protoss"), "utf8");
assert.strictEqual(findDefinitionInText(builtins, "succ").line > 0, true);
assert.strictEqual(findDefinitionInText(builtins, "foldNat").kind, "function");

const grammar = JSON.parse(
  fs.readFileSync(path.join(__dirname, "..", "syntaxes", "protoss.tmLanguage.json"), "utf8")
);
assert.strictEqual(grammar.scopeName, "source.protoss");
assert.ok(JSON.stringify(grammar).includes("keyword.operator.pipeline.protoss"));

console.log("protoss definition tests ok");
