"use strict";

const assert = require("assert");
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
assert.strictEqual(stripLineComment("(def a Nat 1) ; comment").trim(), "(def a Nat 1)");
assert.strictEqual(symbolAtTextOffset("    inferredAdd 2 5", 8), "inferredAdd");

console.log("protoss definition tests ok");
