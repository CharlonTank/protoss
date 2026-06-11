"use strict";
/*
 * Standalone Node harness for the web runtime's VDOM diff/patch.
 *
 * No browser is available, so this:
 *   (a) extracts the `runtime_js` string literal from lib/web.ml,
 *   (b) evaluates its IIFE body under a minimal mock `document`/`window`/
 *       `localStorage`, capturing the internal `renderView` and `patch`
 *       functions,
 *   (c) asserts that patch reuses existing DOM nodes (the key property: an
 *       unchanged <input> stays the *same* DOM object across renders, so real
 *       focus/caret would be preserved), patches text/attributes in place,
 *       replaces on tag change, and handles child add/remove.
 *
 * Run: node test/vdom_patch_harness.js
 */

const fs = require("fs");
const path = require("path");
const vm = require("vm");

// ---------------------------------------------------------------------------
// 1. Extract the runtime_js literal from lib/web.ml.
// ---------------------------------------------------------------------------
const webMlPath = path.join(__dirname, "..", "lib", "web.ml");
const webMl = fs.readFileSync(webMlPath, "utf8");
const marker = "let runtime_js =";
const startIdx = webMl.indexOf(marker);
if (startIdx === -1) throw new Error("could not find runtime_js in web.ml");
const open = webMl.indexOf("{|", startIdx);
const close = webMl.indexOf("|}", open);
if (open === -1 || close === -1) throw new Error("could not delimit runtime_js literal");
let runtimeSource = webMl.slice(open + 2, close);

// Strip the outer IIFE wrapper "(function () { ... })();" so the inner function
// declarations land in our sandbox scope where we can read them back.
runtimeSource = runtimeSource.replace(/^\s*\(function \(\) \{/, "");
runtimeSource = runtimeSource.replace(/\}\)\(\);\s*$/, "");
// Append a hook that exports the internals we want to test.
runtimeSource += "\n;globalThis.__exports = { renderView: renderView, patch: patch, text: text, keyOf: keyOf, childrenAreKeyed: childrenAreKeyed, reconcileKeyed: reconcileKeyed };\n";

// ---------------------------------------------------------------------------
// 2. Minimal mock DOM. Elements track childNodes, attributes, listeners,
//    textContent/value; createTextNode-style text uses the runtime's own
//    text() helper (a <span>), so we only need element semantics here.
// ---------------------------------------------------------------------------
let nodeSeq = 0;

function MockElement(tag) {
  this.__id = ++nodeSeq;
  this.nodeType = 1;
  this.tagName = String(tag).toUpperCase();
  this.childNodes = [];
  this.attributes = {};
  this.listeners = []; // { event, handler }
  this._textContent = "";
  this._className = "";
  this.value = "";
  this.src = "";
  this.alt = "";
  this.loading = "";
}
Object.defineProperty(MockElement.prototype, "className", {
  get() { return this._className; },
  set(v) { this._className = String(v); },
});
Object.defineProperty(MockElement.prototype, "textContent", {
  get() { return this._textContent; },
  set(v) {
    // Setting textContent replaces all children with a single text value (DOM
    // semantics). Mirror that so text nodes don't accumulate stale children.
    this._textContent = String(v);
    this.childNodes = [];
  },
});
Object.defineProperty(MockElement.prototype, "firstChild", {
  get() { return this.childNodes.length ? this.childNodes[0] : null; },
});
MockElement.prototype.appendChild = function (child) {
  this.childNodes.push(child);
  child.parentNode = this;
  return child;
};
MockElement.prototype.removeChild = function (child) {
  const i = this.childNodes.indexOf(child);
  if (i === -1) throw new Error("removeChild: not a child");
  this.childNodes.splice(i, 1);
  child.parentNode = null;
  return child;
};
MockElement.prototype.replaceChild = function (next, old) {
  const i = this.childNodes.indexOf(old);
  if (i === -1) throw new Error("replaceChild: old not a child");
  this.childNodes[i] = next;
  next.parentNode = this;
  old.parentNode = null;
  return old;
};
// Faithful DOM insertBefore: if `node` is already in this element it is first
// detached from its current position (DOM "move" semantics — the same object is
// repositioned, not cloned), then inserted before `ref`. A null `ref` appends.
// insertBefore(node, node) is a no-op (matching the spec) since after removing
// node, `ref` is gone, so we must guard that case explicitly.
MockElement.prototype.insertBefore = function (node, ref) {
  if (node === ref) return node; // no-op move onto itself
  const cur = this.childNodes.indexOf(node);
  if (cur !== -1) this.childNodes.splice(cur, 1); // detach from old slot
  if (ref == null) {
    this.childNodes.push(node);
  } else {
    const ri = this.childNodes.indexOf(ref);
    if (ri === -1) throw new Error("insertBefore: ref not a child");
    this.childNodes.splice(ri, 0, node);
  }
  node.parentNode = this;
  return node;
};
MockElement.prototype.setAttribute = function (name, value) {
  this.attributes[name] = String(value);
};
MockElement.prototype.removeAttribute = function (name) {
  delete this.attributes[name];
};
MockElement.prototype.getAttribute = function (name) {
  return Object.prototype.hasOwnProperty.call(this.attributes, name)
    ? this.attributes[name]
    : null;
};
MockElement.prototype.addEventListener = function (event, handler) {
  this.listeners.push({ event, handler });
};
MockElement.prototype.removeEventListener = function (event, handler) {
  for (let i = 0; i < this.listeners.length; i++) {
    if (this.listeners[i].event === event && this.listeners[i].handler === handler) {
      this.listeners.splice(i, 1);
      return;
    }
  }
};
// Test helper: count listeners bound for a given event.
MockElement.prototype._listenerCount = function (event) {
  return this.listeners.filter((l) => l.event === event).length;
};
// Test helper: fire the first listener for an event.
MockElement.prototype._fire = function (event) {
  const l = this.listeners.find((x) => x.event === event);
  if (!l) throw new Error("no listener for " + event);
  l.handler();
};

const mockDocument = {
  createElement(tag) { return new MockElement(tag); },
  createTextNode(t) {
    const n = new MockElement("#text");
    n.nodeType = 3;
    n._textContent = String(t);
    return n;
  },
  getElementById() { return new MockElement("div"); },
};

const sandbox = {
  document: mockDocument,
  window: {},
  localStorage: { getItem() { return null; }, setItem() {}, },
  console,
  JSON,
  Math,
  Object,
  String,
  Array,
  CustomEvent: function () {},
};
sandbox.globalThis = sandbox;
sandbox.window = sandbox; // runtime assigns window.ProtossRuntime = ...

vm.createContext(sandbox);
vm.runInContext(runtimeSource, sandbox, { filename: "runtime_js (extracted)" });

const { renderView, patch, keyOf, childrenAreKeyed, reconcileKeyed } = sandbox.__exports;
if (typeof renderView !== "function") throw new Error("renderView not captured");
if (typeof patch !== "function") throw new Error("patch not captured");
if (typeof keyOf !== "function") throw new Error("keyOf not captured");
if (typeof childrenAreKeyed !== "function") throw new Error("childrenAreKeyed not captured");
if (typeof reconcileKeyed !== "function") throw new Error("reconcileKeyed not captured");

// ---------------------------------------------------------------------------
// 3. Assertions.
// ---------------------------------------------------------------------------
let passed = 0;
const failures = [];
function check(name, cond) {
  if (cond) { passed++; console.log("  ok  - " + name); }
  else { failures.push(name); console.log("  FAIL- " + name); }
}
function eq(name, a, b) {
  if (a === b) { passed++; console.log("  ok  - " + name); }
  else { failures.push(name + " (got " + JSON.stringify(a) + " want " + JSON.stringify(b) + ")");
         console.log("  FAIL- " + name + " (got " + JSON.stringify(a) + " want " + JSON.stringify(b) + ")"); }
}

function newMount() { return new MockElement("div"); }

// vnode builders matching the shapes produced by machine.view / renderView.
const vText = (t) => ({ kind: "text", text: t });
const vInput = (value, handler) => ({ kind: "input", value, handler });
const vButton = (label, message) => ({ kind: "button", label, message });
const vColumn = (...children) => ({ kind: "column", children });
const vRow = (...children) => ({ kind: "row", children });
const vNode = (tag, attributes, children) => ({ kind: "node", tag, attributes: attributes || [], children: children || [] });
// A keyed "node" vnode: carries {kind:"attr", name:"key", value:key}. Extra
// attributes/children can be appended. This is the ONLY way a child opts into
// keyed reconciliation (keyOf reads the "key" attr of a "node" kind only).
const vKeyed = (key, tag, children, extraAttrs) =>
  ({ kind: "node", tag: tag || "div",
     attributes: [{ kind: "attr", name: "key", value: key }].concat(extraAttrs || []),
     children: children || [] });
// A keyed <input>: a "node" with tag "input" so it both carries a key AND
// renders to a real INPUT element whose object identity (focus/caret) we track.
const vKeyedInput = (key, value) =>
  vKeyed(key, "input", [], [{ kind: "attr", name: "value", value: value || "" }]);

let dispatched = [];
const dispatch = (m) => { dispatched.push(m); };

console.log("== Test 1: first render builds the DOM (prevVNode=null) ==");
{
  const mount = newMount();
  const tree = vColumn(vInput("hello", "H1"), vButton("Add", "ADD"));
  patch(mount, mount.firstChild, null, tree, dispatch);
  eq("mount has one child", mount.childNodes.length, 1);
  const col = mount.firstChild;
  eq("root is a column div", col.className, "protoss-column");
  eq("column has 2 children", col.childNodes.length, 2);
  eq("first child is input", col.childNodes[0].tagName, "INPUT");
  eq("input value rendered", col.childNodes[0].value, "hello");
  eq("second child is button", col.childNodes[1].tagName, "BUTTON");
  eq("button label rendered", col.childNodes[1].textContent, "Add");
}

console.log("== Test 2 (KEY): re-render with changed text reuses the input DOM node ==");
{
  const mount = newMount();
  const t1 = vColumn(vInput("a", "H"), vText("count: 0"));
  patch(mount, mount.firstChild, null, t1, dispatch);
  const col = mount.firstChild;
  const inputBefore = col.childNodes[0];
  const textSpanBefore = col.childNodes[1];
  const inputId = inputBefore.__id;

  // Simulate the user having typed/focused: the live DOM value diverges from
  // the model only via user input; here the model just bumped the counter.
  const t2 = vColumn(vInput("a", "H"), vText("count: 1"));
  patch(mount, mount.firstChild, t1, t2, dispatch);
  const colAfter = mount.firstChild;

  check("column is the SAME object", colAfter === col);
  check("INPUT is the SAME DOM object (focus/caret would survive)", colAfter.childNodes[0] === inputBefore);
  eq("input id unchanged", colAfter.childNodes[0].__id, inputId);
  check("text span is the SAME object", colAfter.childNodes[1] === textSpanBefore);
  eq("text content updated in place", textSpanBefore.textContent, "count: 1");
  eq("input still has exactly one input listener (rebind not duplicate)", inputBefore._listenerCount("input"), 1);
}

console.log("== Test 3: changing an attribute patches it in place (same node) ==");
{
  const mount = newMount();
  const a1 = vNode("div", [{ kind: "attr", name: "class", value: "x" }, { kind: "attr", name: "title", value: "old" }], [vText("hi")]);
  patch(mount, mount.firstChild, null, a1, dispatch);
  const el = mount.firstChild;
  const elId = el.__id;
  eq("attr class set", el.getAttribute("class"), "x");
  eq("attr title set", el.getAttribute("title"), "old");

  const a2 = vNode("div", [{ kind: "attr", name: "class", value: "y" }], [vText("hi")]);
  patch(mount, mount.firstChild, a1, a2, dispatch);
  const elAfter = mount.firstChild;
  check("node is the SAME object after attr patch", elAfter === el);
  eq("node id unchanged", elAfter.__id, elId);
  eq("attr class updated in place", elAfter.getAttribute("class"), "y");
  eq("removed attr title is gone", elAfter.getAttribute("title"), null);
  eq("child text preserved", elAfter.childNodes[0].textContent, "hi");
}

console.log("== Test 4: changing tag/kind replaces the node ==");
{
  const mount = newMount();
  const n1 = vNode("div", [], [vText("a")]);
  patch(mount, mount.firstChild, null, n1, dispatch);
  const before = mount.firstChild;
  eq("starts as div", before.tagName, "DIV");

  const n2 = vNode("section", [], [vText("a")]);
  patch(mount, mount.firstChild, n1, n2, dispatch);
  const after = mount.firstChild;
  check("node was REPLACED (different object)", after !== before);
  eq("now a section", after.tagName, "SECTION");
  eq("still exactly one mount child", mount.childNodes.length, 1);

  // kind change input -> button also replaces.
  const k1 = vColumn(vInput("v", "H"));
  patch(mount, mount.firstChild, n2, k1, dispatch);
  const inputNode = mount.firstChild.childNodes[0];
  const k2 = vColumn(vButton("B", "M"));
  patch(mount, mount.firstChild, k1, k2, dispatch);
  const buttonNode = mount.firstChild.childNodes[0];
  check("input replaced by button on kind change", buttonNode !== inputNode);
  eq("button rendered after kind change", buttonNode.tagName, "BUTTON");
}

console.log("== Test 5: child added and child removed ==");
{
  const mount = newMount();
  const c1 = vColumn(vText("one"));
  patch(mount, mount.firstChild, null, c1, dispatch);
  const col = mount.firstChild;
  const firstChildBefore = col.childNodes[0];
  eq("starts with 1 child", col.childNodes.length, 1);

  // Add a child.
  const c2 = vColumn(vText("one"), vText("two"));
  patch(mount, mount.firstChild, c1, c2, dispatch);
  eq("now 2 children after add", col.childNodes.length, 2);
  check("existing first child reused on add", col.childNodes[0] === firstChildBefore);
  eq("first child unchanged text", col.childNodes[0].textContent, "one");
  eq("appended child text", col.childNodes[1].textContent, "two");

  // Remove a child.
  const c3 = vColumn(vText("one"));
  patch(mount, mount.firstChild, c2, c3, dispatch);
  eq("back to 1 child after remove", col.childNodes.length, 1);
  check("surviving child still the same object", col.childNodes[0] === firstChildBefore);
}

console.log("== Test 6: button message rebinds when it changes (dispatch correctness) ==");
{
  const mount = newMount();
  const b1 = vButton("Go", { tag: "MsgA" });
  patch(mount, mount.firstChild, null, b1, dispatch);
  const btn = mount.firstChild;
  dispatched = [];
  btn._fire("click");
  eq("dispatch fired MsgA", dispatched.length && dispatched[0].tag, "MsgA");

  const b2 = vButton("Go", { tag: "MsgB" });
  patch(mount, mount.firstChild, b1, b2, dispatch);
  check("button reused (same object) across message change", mount.firstChild === btn);
  eq("button still has one click listener", btn._listenerCount("click"), 1);
  dispatched = [];
  btn._fire("click");
  eq("dispatch now fires MsgB after rebind", dispatched.length && dispatched[0].tag, "MsgB");
}

console.log("== Test 7: input live value not clobbered when model value unchanged ==");
{
  const mount = newMount();
  const i1 = vColumn(vInput("", "H"));
  patch(mount, mount.firstChild, null, i1, dispatch);
  const input = mount.firstChild.childNodes[0];
  // User types: live DOM value diverges from the (unchanged) model value "".
  input.value = "user typing";
  // A re-render unrelated to this input (model value for it is still "").
  const i2 = vColumn(vInput("", "H"));
  patch(mount, mount.firstChild, i1, i2, dispatch);
  check("input reused", mount.firstChild.childNodes[0] === input);
  eq("user's in-progress value preserved (not reset)", input.value, "user typing");

  // Now the model genuinely sets a new value -> it should be applied.
  const i3 = vColumn(vInput("model set", "H"));
  patch(mount, mount.firstChild, i2, i3, dispatch);
  eq("model-driven value change applied", input.value, "model set");
}

console.log("== Test 8: inline on* attributes stay blocked; {kind:'on'} binds ==");
{
  const mount = newMount();
  const n = vNode("div",
    [ { kind: "attr", name: "onclick", value: "alert(1)" },
      { kind: "attr", name: "data-x", value: "1" },
      { kind: "on", event: "click", message: { tag: "Clicked" } } ],
    []);
  patch(mount, mount.firstChild, null, n, dispatch);
  const el = mount.firstChild;
  eq("inline onclick attribute blocked", el.getAttribute("onclick"), null);
  eq("normal data attr kept", el.getAttribute("data-x"), "1");
  eq("one real click listener from {kind:on}", el._listenerCount("click"), 1);
  dispatched = [];
  el._fire("click");
  eq("on-listener dispatches its message", dispatched.length && dispatched[0].tag, "Clicked");

  // Drop the {kind:on} -> listener removed on patch.
  const n2 = vNode("div", [{ kind: "attr", name: "data-x", value: "1" }], []);
  patch(mount, mount.firstChild, n, n2, dispatch);
  eq("click listener removed when {kind:on} dropped", el._listenerCount("click"), 0);
}

// ===========================================================================
// KEYED reconciliation tests. A child opts in by carrying a "key" attribute on
// a "node" vnode. When EVERY child of a list (old and new) has a distinct key,
// patchChildren reconciles by key — moving existing DOM nodes — instead of the
// positional diff. Tests below prove identity-preserving reorder/insert/remove
// and that an unkeyed (or partially-keyed) list keeps the legacy positional path.
// ===========================================================================

console.log("== Test 9: keyOf / childrenAreKeyed opt-in semantics ==");
{
  eq("keyOf reads the key attr of a node", keyOf(vKeyed("X", "div")), "X");
  eq("keyOf is null for a node without a key", keyOf(vNode("div", [{ kind: "attr", name: "id", value: "z" }], [])), null);
  eq("keyOf is null for non-node kinds (text)", keyOf(vText("hi")), null);
  eq("keyOf is null for non-node kinds (input)", keyOf(vInput("v", "H")), null);
  check("all-distinct-keyed list opts in", childrenAreKeyed([vKeyed("a", "div"), vKeyed("b", "div")]) === true);
  check("empty list does NOT opt in", childrenAreKeyed([]) === false);
  check("a single missing key disqualifies the whole list", childrenAreKeyed([vKeyed("a", "div"), vNode("div", [], [])]) === false);
  check("duplicate keys disqualify the whole list", childrenAreKeyed([vKeyed("a", "div"), vKeyed("a", "div")]) === false);
}

console.log("== Test 10 (KEY): reorder [A,B,C] -> [C,A,B] reuses the SAME 3 DOM nodes, reordered ==");
{
  const mount = newMount();
  const t1 = vColumn(vKeyed("A", "div", [vText("a")]), vKeyed("B", "div", [vText("b")]), vKeyed("C", "div", [vText("c")]));
  patch(mount, mount.firstChild, null, t1, dispatch);
  const col = mount.firstChild;
  const domA = col.childNodes[0], domB = col.childNodes[1], domC = col.childNodes[2];
  eq("A rendered with its text", domA.childNodes[0].textContent, "a");
  eq("B rendered with its text", domB.childNodes[0].textContent, "b");
  eq("C rendered with its text", domC.childNodes[0].textContent, "c");

  const t2 = vColumn(vKeyed("C", "div", [vText("c")]), vKeyed("A", "div", [vText("a")]), vKeyed("B", "div", [vText("b")]));
  patch(mount, mount.firstChild, t1, t2, dispatch);

  eq("still exactly 3 children", col.childNodes.length, 3);
  check("slot 0 is the SAME object as old C (moved, not recreated)", col.childNodes[0] === domC);
  check("slot 1 is the SAME object as old A", col.childNodes[1] === domA);
  check("slot 2 is the SAME object as old B", col.childNodes[2] === domB);
  eq("new order matches [C,A,B] by content", col.childNodes.map((n) => n.childNodes[0].textContent).join(","), "c,a,b");
}

console.log("== Test 11 (KEY, THE PROOF): a keyed <input> moved middle->front stays the SAME DOM object (focus/caret survive) ==");
{
  const mount = newMount();
  const t1 = vColumn(vKeyedInput("A", "av"), vKeyedInput("FOCUSED", "typed-by-user"), vKeyedInput("C", "cv"));
  patch(mount, mount.firstChild, null, t1, dispatch);
  const col = mount.firstChild;
  const inputBefore = col.childNodes[1]; // the middle, "focused" input
  const inputId = inputBefore.__id;
  eq("middle child is an INPUT element", inputBefore.tagName, "INPUT");
  // The user's live caret/selection state lives on the very DOM object; here we
  // stand in for it with a property only this object carries.
  inputBefore.__caret = 7;

  // Reorder so FOCUSED moves to the front. Value unchanged in the model.
  const t2 = vColumn(vKeyedInput("FOCUSED", "typed-by-user"), vKeyedInput("A", "av"), vKeyedInput("C", "cv"));
  patch(mount, mount.firstChild, t1, t2, dispatch);

  check("the focused input is now at slot 0", col.childNodes[0] === inputBefore);
  eq("it is the SAME DOM object (id unchanged) => focus/caret preserved", col.childNodes[0].__id, inputId);
  eq("its live caret/selection state survived the move", col.childNodes[0].__caret, 7);
  eq("its value survived the move", col.childNodes[0].getAttribute("value"), "typed-by-user");
  eq("three inputs total still", col.childNodes.length, 3);
  eq("order is [FOCUSED,A,C]", col.childNodes.map((n) => n.getAttribute("value")).join(","), "typed-by-user,av,cv");
}

console.log("== Test 12 (KEY): inserting a key in the middle reuses existing nodes, only the new one is created ==");
{
  const mount = newMount();
  const t1 = vColumn(vKeyed("A", "div", [vText("a")]), vKeyed("C", "div", [vText("c")]));
  patch(mount, mount.firstChild, null, t1, dispatch);
  const col = mount.firstChild;
  const domA = col.childNodes[0], domC = col.childNodes[1];

  const t2 = vColumn(vKeyed("A", "div", [vText("a")]), vKeyed("B", "div", [vText("b")]), vKeyed("C", "div", [vText("c")]));
  patch(mount, mount.firstChild, t1, t2, dispatch);

  eq("now 3 children", col.childNodes.length, 3);
  check("A reused (same object)", col.childNodes[0] === domA);
  check("C reused (same object)", col.childNodes[2] === domC);
  check("B is a brand-new object (not A or C)", col.childNodes[1] !== domA && col.childNodes[1] !== domC);
  eq("B rendered between A and C", col.childNodes[1].childNodes[0].textContent, "b");
  eq("final order is [A,B,C]", col.childNodes.map((n) => n.childNodes[0].textContent).join(","), "a,b,c");
}

console.log("== Test 13 (KEY): removing a key in the middle removes the right node, reuses the others ==");
{
  const mount = newMount();
  const t1 = vColumn(vKeyed("A", "div", [vText("a")]), vKeyed("B", "div", [vText("b")]), vKeyed("C", "div", [vText("c")]));
  patch(mount, mount.firstChild, null, t1, dispatch);
  const col = mount.firstChild;
  const domA = col.childNodes[0], domB = col.childNodes[1], domC = col.childNodes[2];
  // Give B a listener so we can prove it was clearDeep'd (detached) on removal.
  domB.addEventListener("custom", function () {});
  eq("B has a listener before removal", domB.listeners.length >= 1, true);

  const t2 = vColumn(vKeyed("A", "div", [vText("a")]), vKeyed("C", "div", [vText("c")]));
  patch(mount, mount.firstChild, t1, t2, dispatch);

  eq("now 2 children", col.childNodes.length, 2);
  check("A reused", col.childNodes[0] === domA);
  check("C reused", col.childNodes[1] === domC);
  check("B is no longer a child (removed)", col.childNodes.indexOf(domB) === -1);
  eq("removed B was detached (parentNode null)", domB.parentNode, null);
  eq("final order is [A,C]", col.childNodes.map((n) => n.childNodes[0].textContent).join(","), "a,c");
}

console.log("== Test 14 (KEY): a moved keyed node keeps its listeners (NOT clearDeep'd while reused) ==");
{
  const mount = newMount();
  const withClick = (key) => vKeyed(key, "div", [], [{ kind: "on", event: "click", message: { tag: "Hit-" + key } }]);
  const t1 = vColumn(withClick("A"), withClick("B"));
  patch(mount, mount.firstChild, null, t1, dispatch);
  const col = mount.firstChild;
  const domA = col.childNodes[0], domB = col.childNodes[1];
  eq("A has one click listener initially", domA._listenerCount("click"), 1);

  // Swap order: B then A.
  const t2 = vColumn(withClick("B"), withClick("A"));
  patch(mount, mount.firstChild, t1, t2, dispatch);

  check("A moved to slot 1 (same object)", col.childNodes[1] === domA);
  check("B moved to slot 0 (same object)", col.childNodes[0] === domB);
  eq("moved A still has exactly one click listener (rebind, not duplicate, not lost)", domA._listenerCount("click"), 1);
  dispatched = [];
  domA._fire("click");
  eq("moved A still dispatches its message", dispatched.length && dispatched[0].tag, "Hit-A");
}

console.log("== Test 15 (REGRESSION): an UNKEYED list keeps the positional diff (legacy behaviour unchanged) ==");
{
  const mount = newMount();
  // No child carries a key -> childrenAreKeyed is false -> positional path.
  const t1 = vColumn(vText("one"), vText("two"));
  patch(mount, mount.firstChild, null, t1, dispatch);
  const col = mount.firstChild;
  const slot0 = col.childNodes[0], slot1 = col.childNodes[1];

  // "Prepend" zero: positionally, slot0 is re-mutated to "zero" in place (NOT
  // moved), slot1 re-mutated to "one", and a third node appended for "two".
  const t2 = vColumn(vText("zero"), vText("one"), vText("two"));
  patch(mount, mount.firstChild, t1, t2, dispatch);

  eq("now 3 children", col.childNodes.length, 3);
  check("slot 0 is the SAME object as before, re-mutated in place (positional)", col.childNodes[0] === slot0);
  check("slot 1 is the SAME object as before, re-mutated in place (positional)", col.childNodes[1] === slot1);
  eq("slot 0 text positionally updated to 'zero'", col.childNodes[0].textContent, "zero");
  eq("slot 1 text positionally updated to 'one'", col.childNodes[1].textContent, "one");
  eq("slot 2 appended as 'two'", col.childNodes[2].textContent, "two");
}

console.log("== Test 16 (REGRESSION): a PARTIALLY-keyed list falls back to the positional diff ==");
{
  const mount = newMount();
  // One child keyed, one not -> NOT fully keyed -> positional path.
  const t1 = vColumn(vKeyed("A", "div", [vText("a")]), vNode("div", [], [vText("b")]));
  patch(mount, mount.firstChild, null, t1, dispatch);
  const col = mount.firstChild;
  const slot0 = col.childNodes[0];

  // Swap the vnode order. Under positional diff both slots have the same
  // structural signature ("node:div"), so each is updated in place by index —
  // the DOM objects are NOT moved, proving we did NOT take the keyed path.
  const t2 = vColumn(vNode("div", [], [vText("b")]), vKeyed("A", "div", [vText("a")]));
  patch(mount, mount.firstChild, t1, t2, dispatch);

  eq("still 2 children", col.childNodes.length, 2);
  check("slot 0 is the SAME object (positional in-place update, not a keyed move)", col.childNodes[0] === slot0);
  eq("slot 0 updated in place to the new child's content 'b'", col.childNodes[0].childNodes[0].textContent, "b");
  eq("slot 0 lost its key attr (overwritten in place)", col.childNodes[0].getAttribute("key"), null);
}

// ---------------------------------------------------------------------------
// Summary.
// ---------------------------------------------------------------------------
console.log("\n----------------------------------------");
console.log("passed: " + passed + "   failed: " + failures.length);
if (failures.length) {
  console.log("FAILURES:");
  failures.forEach((f) => console.log("  - " + f));
  process.exit(1);
}
console.log("ALL VDOM PATCH ASSERTIONS PASSED");
process.exit(0);
