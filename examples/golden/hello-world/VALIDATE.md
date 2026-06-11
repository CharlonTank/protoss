# hello-world — validation

Smallest valid Protoss project: one pure `main` definition, no capabilities,
`stdlib = "none"`.

Run every command from the repository root. The `env PROTOSS_GLOBAL_STORE=`
prefix only disables global-object interning (hermetic runs); it changes no
output. `<REPO>` stands for the absolute repository root. Every `p2:` value
below is content-addressed and deterministic: byte-identical sources must
reproduce it exactly (verified by rebuilding from a deleted store).

0. Reset (optional, only for re-runs; `.protoss/` is git-ignored):

   ```sh
   rm -rf examples/golden/hello-world/.protoss
   ```

1. Project check — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project check examples/golden/hello-world
   ```

   ```
   Project OK golden-hello-world
   ```

2. Project build — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project build examples/golden/hello-world
   ```

   ```
   Build p2:35fdec2f5537ec599157a5aeb7e56ffa6331469fe538f9d76207ecc91105da67
   UniverseRoot p2:e130ca930c6e1ea56067a854e93751f312f6de165015487867b91cb9f7bee3a1
   Store <REPO>/examples/golden/hello-world/.protoss/store
   ```

3. Project audit (store must verify) — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe audit examples/golden/hello-world
   ```

   ```
   Audit OK
   ```

4. Evaluate the entrypoint (file-level) — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe eval examples/golden/hello-world/src/main.protoss --entry main
   ```

   ```
   main = "hello, world"
   ```
