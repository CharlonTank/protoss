# pure-library — validation

A small pure library: polymorphic defs (`identity`, `const`), Nat helpers,
a named record (`Point`), a named variant (`Step`) with its eliminator, and
worked examples. No capabilities, `stdlib = "none"`. Full project lifecycle:
check, build, lock, package, interface.

Run every command from the repository root. The `env PROTOSS_GLOBAL_STORE=`
prefix only disables global-object interning; it changes no output. `<REPO>`
stands for the absolute repository root. Every `p2:` value is deterministic.

0. Reset (optional, only for re-runs; `.protoss/` is git-ignored):

   ```sh
   rm -rf examples/golden/pure-library/.protoss
   ```

1. Project check — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project check examples/golden/pure-library
   ```

   ```
   Project OK golden-pure-library
   ```

2. Project build — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project build examples/golden/pure-library
   ```

   ```
   Build p2:a84b0d16255d9e70d4757f74758cbb1cae80b3ed72ad660072dafda723e0841a
   UniverseRoot p2:f0875b1d6b97571342b4a041e350ac4544cd978a9f5a049f0ddf2f651656f8ce
   Store <REPO>/examples/golden/pure-library/.protoss/store
   ```

3. Write the lockfile — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project lock examples/golden/pure-library
   ```

   ```
   Lock p2:ef1542a8a37c10d05a5b01314b95c314a313cc022ca97f9d5c1e33b3192c0b4c
   Path <REPO>/examples/golden/pure-library/.protoss/lock
   ```

4. Verify the lockfile against the current build — expect exit 0:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project lock examples/golden/pure-library --check
   ```

   ```
   Lock OK p2:ef1542a8a37c10d05a5b01314b95c314a313cc022ca97f9d5c1e33b3192c0b4c
   Path <REPO>/examples/golden/pure-library/.protoss/lock
   ```

5. Write the package descriptor + interface — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project package examples/golden/pure-library
   ```

   ```
   Package p2:6e91e02b9cbadc5a079e74dd692ba47f602f34e14ae147d12c2a976a17e1dac4
   Path <REPO>/examples/golden/pure-library/.protoss/packages/p2_6e91e02b9cbadc5a079e74dd692ba47f602f34e14ae147d12c2a976a17e1dac4.package
   Interface p2:a9afa6fcc897eb2940f85454cd2396e6c33fe3938038a5526148d355f794d303
   InterfacePath <REPO>/examples/golden/pure-library/.protoss/interfaces/p2_a9afa6fcc897eb2940f85454cd2396e6c33fe3938038a5526148d355f794d303.interface.json
   Contract p2:b6bd8194fb8c56825bc229b831adb5de58f447684a782259ef61d5abe9963f80
   Lock p2:ef1542a8a37c10d05a5b01314b95c314a313cc022ca97f9d5c1e33b3192c0b4c
   Build p2:a84b0d16255d9e70d4757f74758cbb1cae80b3ed72ad660072dafda723e0841a
   UniverseRoot p2:f0875b1d6b97571342b4a041e350ac4544cd978a9f5a049f0ddf2f651656f8ce
   Store <REPO>/examples/golden/pure-library/.protoss/store
   ```

6. Verify the package — expect exit 0; same fields as step 5 but first line
   `Package OK p2:6e91e02b…`:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project package examples/golden/pure-library --check
   ```

7. Print the verified public interface — expect exit 0. Key lines
   (deterministic; `project=` carries the absolute path):

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project interface examples/golden/pure-library
   ```

   ```
   PackageInterface OK
   package=golden-pure-library
   version=1.0.0
   package_ref=p2:6e91e02b9cbadc5a079e74dd692ba47f602f34e14ae147d12c2a976a17e1dac4
   interface_hash=p2:5104cf4e65dedc0d76425358e59cfb76f27a7423861fd0416919e6f175e64c78
   contract_hash=p2:b6bd8194fb8c56825bc229b831adb5de58f447684a782259ef61d5abe9963f80
   imports=0
   exports=15
   export type Point params=- type=(Record (x Nat) (y Nat)) type_hash=p2:5d2bf61b23c3d71a102c66c6617d0e00d34ac28fa101be7e87552348ca663723
   export type Step params=- type=(Variant (Move Nat) (Stay Unit)) type_hash=p2:7726dfaf81c3f488ea1085b5d9ee9bd0bf5a755d73496adbb99f719681a66d57
   ```

   (13 more `export def …` lines follow; all deterministic.)

   Note: `project interface` requires step 5 first — on a store without a
   package pointer it fails with
   `WORKSPACE001 workspace error: missing package pointer: …/.protoss/package`.

8. Evaluate a worked example — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe eval examples/golden/pure-library/src/lib.protoss --entry seven
   ```

   ```
   seven = 7
   ```
