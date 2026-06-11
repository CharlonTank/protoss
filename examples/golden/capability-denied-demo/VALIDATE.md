# capability-denied-demo — validation (expected FAILURE)

This project is a negative golden: it MUST be rejected. The source performs
`(Http.get …)` but never declares the `Http.get` capability — there is no
top-level `(capabilities …)` form and `protoss.toml` has `capabilities = []`.
Every check path refuses it before writing any canonical graph or store
(no `.protoss/` directory is created by the failing commands).

Run every command from the repository root. The `env PROTOSS_GLOBAL_STORE=`
prefix only disables global-object interning; it changes no output. `<REPO>`
stands for the absolute repository root.

1. Isolated file check — expect **exit 1** with the public taxonomy code
   `CAP001` (CapabilityDenied) and a `path:line:column` location, stderr
   exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe check examples/golden/capability-denied-demo/src/main.protoss
   ```

   ```
   CAP001 load error: <REPO>/examples/golden/capability-denied-demo/src/main.protoss:9:34: definition fetchData: missing capability: Http.get, expression (Http.get "https://example.invalid/data")
   ```

2. Project check — expect **exit 1**. The workspace path wraps the same
   failure under the workspace catalog code `WORKSPACE001` (not `CAP001`);
   the message still names the missing capability, stderr exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project check examples/golden/capability-denied-demo
   ```

   ```
   WORKSPACE001 workspace error: definition fetchData: missing capability: Http.get, expression (Http.get "https://example.invalid/data")
   ```

3. Project build — expect **exit 1**, same `WORKSPACE001` message as step 2,
   and no `.protoss/` store may be created:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project build examples/golden/capability-denied-demo
   ```

4. The code is in the public catalog — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe explain CAP001
   ```

   ```
   An effect requires a capability that is not declared in scope.
   ```

Doctor integration note: assert exit 1 AND match on
`missing capability: Http.get`; match the code prefix `CAP001` on the
isolated `check` path (step 1) — the project-level wrapper deliberately
reports `WORKSPACE001` today.
