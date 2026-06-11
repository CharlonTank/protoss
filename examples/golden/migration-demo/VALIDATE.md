# migration-demo — validation

A schema evolution driven by a structured patch. The source ships version 1
of a record schema (`(Record (title String))`); the patch
`patches/add_done_field.json` evolves it to `(Record (done Bool) (title
String))` in one atomic batch:

- op 1 `MigrateType`: adds the pure migration `migrate_v1_v2 : (-> (Record
  (title String)) (Record (done Bool) (title String)))` (copies `title`,
  defaults `done` to `false`);
- op 2 `ReplaceDef initial`: the new value is literally the migrated old
  value, `(migrate_v1_v2 (record (title "first item")))`;
- op 3 `ReplaceDef describe`: updated to consume the new record shape;
- `headline` is intentionally NOT in the patch: whole-program revalidation
  must keep it typechecking against the replaced definitions.

Run every command from the repository root, **in this order** (one-shot over
a fresh store: step 0 is mandatory on re-runs — re-applying the patch fails
with `PATCH001 … MigrateType target already exists`-style conflicts by
design). The `env PROTOSS_GLOBAL_STORE=` prefix only disables global-object
interning; it changes no output. `<REPO>` stands for the absolute repository
root. Every `p2:` value is deterministic.

0. Reset (mandatory for re-runs; `.protoss/` is git-ignored):

   ```sh
   rm -rf examples/golden/migration-demo/.protoss
   ```

1. Build the v1 store — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project build examples/golden/migration-demo
   ```

   ```
   Build p2:a8353feddc610c058997f1afb48da4707e79b2999b75c1b6d575585a54ec1781
   UniverseRoot p2:97765dcce995e8df1e1f0191c50af4626d16b1fdf8a42dd164355674c4a8d4a1
   Store <REPO>/examples/golden/migration-demo/.protoss/store
   ```

2. Review the migration patch — expect exit 0; the review must list the
   three ops in order (`MigrateType migrate_v1_v2`, `ReplaceDef initial`,
   `ReplaceDef describe`):

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe patch review examples/golden/migration-demo/patches/add_done_field.json
   ```

3. Validate the migration against the v1 store (no mutation) — expect
   exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe patch check examples/golden/migration-demo/.protoss/store examples/golden/migration-demo/patches/add_done_field.json
   ```

   ```
   Patch valid p2:346bf6a339048c61aaba0ab77c6d3166bf89bf1829cbe60719bb5c6baab93575
   ```

4. Apply the migration (atomic batch, audited) — expect exit 0, stdout
   exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe patch apply examples/golden/migration-demo/.protoss/store examples/golden/migration-demo/patches/add_done_field.json
   ```

   ```
   Patch accepted p2:db684cd5ccafac9f466b21f32aa4298b2ffe4ad924f335cb7b36d6f6245cac1b
   ```

5. Verify the audit chain — expect exit 0. Key lines:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe patch audit examples/golden/migration-demo/.protoss/store
   ```

   ```
   Patch audit OK p2:db684cd5ccafac9f466b21f32aa4298b2ffe4ad924f335cb7b36d6f6245cac1b
   protoss-patch-audit-v1
   previous-ref=none
   previous-root=p2:97765dcce995e8df1e1f0191c50af4626d16b1fdf8a42dd164355674c4a8d4a1
   root-ref=p2:6d0d72253cf34c96addec1c89de865fa57006086df46b6e1b9896f3bf3f9b657
   program-hash=p2:346bf6a339048c61aaba0ab77c6d3166bf89bf1829cbe60719bb5c6baab93575
   result=p2:346bf6a339048c61aaba0ab77c6d3166bf89bf1829cbe60719bb5c6baab93575
   ops=3
   op=1 kind=MigrateType name=migrate_v1_v2 target=migrate_v1_v2 result=p2:e4515bb40c8e5b5f43491a91981014b6aea222a758ceb8148517cf89bb8cfed6
   op=2 kind=ReplaceDef name=initial target=initial result=p2:6d7be452d0105dc4bf66ab39099acc45ec0daa3cffaec3e25e078acf266da3e6
   op=3 kind=ReplaceDef name=describe target=describe result=p2:4b6eede4030f52268e2d988043f49d256ced142f2728a61a0336809886ad5553
   ```

6. Re-verify the store: types evolved, `headline` untouched but still
   present and depending on the replaced defs — expect exit 0, stdout
   exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe store list examples/golden/migration-demo
   ```

   ```
   describe p2:f9fe54c37c8bb7052e823ec738313568ec7b52c5d1a921ca51a53cda0ecae38f (-> (Record (done Bool) (title String)) String) deps=[]
   headline p2:99d427903799816323a3f33293935c32b7c4a5d1dae2b18403a3db31b6f1edc7 String deps=[describe,initial]
   initial p2:79c826272630947aa5f353c35801ac8536f8f74a62af253469344c2fcb06a1fd (Record (done Bool) (title String)) deps=[migrate_v1_v2]
   migrate_v1_v2 p2:2b1be7e71689c598672aa0a23ee60c38456cefce71a01265bbd3c5a5b7649442 (-> (Record (title String)) (Record (done Bool) (title String))) deps=[]
   ```

7. Re-verify by evaluating the migrated value from the post-patch graph
   object (`store graphs` lists the pre-patch graph `p2:27b134a5…` and the
   post-patch graph below): the migrated record carries the defaulted new
   field — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe eval --store-graph examples/golden/migration-demo p2:b034ad0ad777d67574e35570cc07d032fd258860fc5f3994cff3fae4327ca735 --entry initial
   ```

   ```
   initial = {done = false, title = "first item"}
   ```

8. The untouched dependent still evaluates after the migration — expect
   exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe eval --store-graph examples/golden/migration-demo p2:b034ad0ad777d67574e35570cc07d032fd258860fc5f3994cff3fae4327ca735 --entry headline
   ```

   ```
   headline = "first item"
   ```
