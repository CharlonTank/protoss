# process-clock — validation

A typed `Process` program that reads the host clock. The `Clock.read`
capability is declared both at source level (`(capabilities Clock.read)`)
and in `protoss.toml` (`capabilities = ["Clock.read"]`). `now` is the bare
`(Process String)` definition; `readTime` pins the same effect with an
explicit `defcap` scope.

Run every command from the repository root. The `env PROTOSS_GLOBAL_STORE=`
prefix only disables global-object interning; it changes no output. `<REPO>`
stands for the absolute repository root. Every `p2:` value is deterministic.

0. Reset (optional, only for re-runs; `.protoss/` is git-ignored):

   ```sh
   rm -rf examples/golden/process-clock/.protoss
   ```

1. Project check — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project check examples/golden/process-clock
   ```

   ```
   Project OK golden-process-clock
   ```

2. Project build — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project build examples/golden/process-clock
   ```

   ```
   Build p2:f72d69db64f578b2195204012166581b5b85283e3d7aa1b0bc5b1558b59f435b
   UniverseRoot p2:4e58a4a1453f5c4e47557a59bc55e7d30eabbd861e29501a68ac4a61cdfc3b42
   Store <REPO>/examples/golden/process-clock/.protoss/store
   ```

3. Capability report — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe capabilities --project examples/golden/process-clock
   ```

   ```
   program-hash=p2:f72d69db64f578b2195204012166581b5b85283e3d7aa1b0bc5b1558b59f435b
   program-caps=[Clock.read]
   defs=
   now cap-scope-ref=p2:408d834b5736aa42a931cf5f9ba1b640b87aa6ba0d2e2e1b443949c59a5a1440 caps=[Clock.read]
   readTime cap-scope-ref=p2:408d834b5736aa42a931cf5f9ba1b640b87aa6ba0d2e2e1b443949c59a5a1440 caps=[Clock.read]
   risks=
   none
   ```

4. Run the process — it must suspend as a typed `ReadClock` request
   (deterministic refs, no wall clock is read). Expect exit 0; key lines:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe run examples/golden/process-clock/src/main.protoss --entry now
   ```

   ```
   ProcessEvalKey p2:be9d35b5a68b091668157b2dfc20bfe9db25b444cf49e524e4056d191122b4dc
   Request ReadClock
   CapScope Clock.read
   CapScopeRef p2:408d834b5736aa42a931cf5f9ba1b640b87aa6ba0d2e2e1b443949c59a5a1440
   ```

   (Full output also includes deterministic `WorldRef`, `RequestId`,
   `ContinuationId`, `CapabilityRef`, `RequestSignatureRef`, `Event`,
   `NextWorldRef` lines.)
