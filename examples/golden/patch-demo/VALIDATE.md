# patch-demo — validation

A tiny project plus a structured JSON patch (`patches/add_total.json`,
single `AddDef` op) that adds a `total` definition depending on `base`.
The scenario goes through the full content-addressed mutation path:
build → patch review → patch check → patch apply → audit → re-verification
(store listing + evaluation of the patched definition from the stored graph
object).

Run every command from the repository root, **in this order** (the scenario
is one-shot over a fresh store: step 0 is mandatory on re-runs, because the
applied `AddDef` makes a second `patch check`/`patch apply` of the same file
fail with `PATCH001 … AddDef target already exists: total` — that is the
atomicity contract, not a bug). The `env PROTOSS_GLOBAL_STORE=` prefix only
disables global-object interning; it changes no output. `<REPO>` stands for
the absolute repository root. Every `p2:` value is deterministic.

0. Reset (mandatory for re-runs; `.protoss/` is git-ignored):

   ```sh
   rm -rf examples/golden/patch-demo/.protoss
   ```

1. Build the store — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project build examples/golden/patch-demo
   ```

   ```
   Build p2:11010968b8570735a85dbcb4fc55073c34141cf37fbcef943a6731d4c007f59b
   UniverseRoot p2:199fb4342c933cae406203eeb111675fbc635164d1e5a37ce799972b443c57aa
   Store <REPO>/examples/golden/patch-demo/.protoss/store
   ```

2. Human-readable patch review — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe patch review examples/golden/patch-demo/patches/add_total.json
   ```

   ```
   Patch review
   ops: 1
   op 1: AddDef
     name: total
     deps: [base]
     capabilities: []
     type: Nat
     expr:
       (succ base)
   ```

3. Validate the patch against the store (no mutation) — expect exit 0,
   stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe patch check examples/golden/patch-demo/.protoss/store examples/golden/patch-demo/patches/add_total.json
   ```

   ```
   Patch valid p2:bb87817ece787c88ae2b9578ba95790ecd3e9f2dd0a8d2e78f357550cc50f6a1
   ```

4. Apply the patch (atomic, audited) — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe patch apply examples/golden/patch-demo/.protoss/store examples/golden/patch-demo/patches/add_total.json
   ```

   ```
   Patch accepted p2:2f1183f04d08a51b3934168b9478cf1a202b1e1aaf5a986f1ee502daac76d0a0
   ```

5. Verify the audit chain — expect exit 0. Key lines (the full output also
   embeds the patch source bytes):

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe patch audit examples/golden/patch-demo/.protoss/store
   ```

   ```
   Patch audit OK p2:2f1183f04d08a51b3934168b9478cf1a202b1e1aaf5a986f1ee502daac76d0a0
   protoss-patch-audit-v1
   previous-ref=none
   previous-root=p2:199fb4342c933cae406203eeb111675fbc635164d1e5a37ce799972b443c57aa
   root-ref=p2:fa8564ff7e2b3661ca233ab570cdff59664e755016f0361a2ef723fc94f67163
   program-hash=p2:cbccabe746dae87e6e6ae8d7323d7b16a94cc77c7871819d277c1d7d6f6c1eba
   result=p2:bb87817ece787c88ae2b9578ba95790ecd3e9f2dd0a8d2e78f357550cc50f6a1
   ops=1
   op=1 kind=AddDef name=total target=total result=p2:bb87817ece787c88ae2b9578ba95790ecd3e9f2dd0a8d2e78f357550cc50f6a1
   ```

6. Re-verify the store contents: the patched definition is present with the
   declared dependency — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe store list examples/golden/patch-demo
   ```

   ```
   base p2:31f69a651d436f48c7b3066c0dba6aa91e4a887ce663ebf4fb58b32a4c1bc00d Nat deps=[]
   label p2:cfffd9543a5bbaad20240fbfdf58a8a682d16fa569375b9f34582d537712e2f1 String deps=[]
   total p2:ccd0be0b2e0a43d9230d11236070427dca21fa4540b72d7ef11c70bdb8620f78 Nat deps=[base]
   ```

7. Re-verify by evaluating the patched program from its content-addressed
   graph object (`store graphs` lists two objects: the pre-patch graph
   `p2:5a092d25…` and the post-patch graph below) — expect exit 0, stdout
   exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe eval --store-graph examples/golden/patch-demo p2:e2cbaf88897644a1b0f6d1d1fa463c63c66310692f476e0e4f719f3d79077e78 --entry total
   ```

   ```
   total = 3
   ```

8. Project-level audit still verifies the patched store — expect exit 0:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe audit examples/golden/patch-demo
   ```

   ```
   Audit OK
   ```
