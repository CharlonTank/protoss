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
runtimeSource += "\n;globalThis.__exports = { renderView: renderView, patch: patch, text: text };\n";

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

const { renderView, patch } = sandbox.__exports;
if (typeof renderView !== "function") throw new Error("renderView not captured");
if (typeof patch !== "function") throw new Error("patch not captured");

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
