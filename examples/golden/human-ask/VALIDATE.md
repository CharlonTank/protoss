# human-ask — validation

A typed `Process` program that asks the human a question. The `Human.ask`
capability is declared both at source level (`(capabilities Human.ask)`)
and in `protoss.toml` (`capabilities = ["Human.ask"]`). `askName` is the
bare `(Process String)` definition; `askTwice` chains two questions under an
explicit `defcap` scope.

Run every command from the repository root. The `env PROTOSS_GLOBAL_STORE=`
prefix only disables global-object interning; it changes no output. `<REPO>`
stands for the absolute repository root. Every `p2:` value is deterministic.

0. Reset (optional, only for re-runs; `.protoss/` is git-ignored):

   ```sh
   rm -rf examples/golden/human-ask/.protoss
   ```

1. Project check — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project check examples/golden/human-ask
   ```

   ```
   Project OK golden-human-ask
   ```

2. Project build — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe project build examples/golden/human-ask
   ```

   ```
   Build p2:5d26eb91830e9fffb556ab46cfbbbab37d82821df7bb42ca7b13d08ccc0a567c
   UniverseRoot p2:7a21d938a172c0663ebb8beb1bef2265106c42414b1ad1230f6f4a5af41e1b92
   Store <REPO>/examples/golden/human-ask/.protoss/store
   ```

3. Capability report — expect exit 0, stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe capabilities --project examples/golden/human-ask
   ```

   ```
   program-hash=p2:5d26eb91830e9fffb556ab46cfbbbab37d82821df7bb42ca7b13d08ccc0a567c
   program-caps=[Human.ask]
   defs=
   askName cap-scope-ref=p2:accc3b1ee6ae4b9b3c454d792b1f305ee8fed9a8e50632a5b825a1b51a84af66 caps=[Human.ask]
   askTwice cap-scope-ref=p2:accc3b1ee6ae4b9b3c454d792b1f305ee8fed9a8e50632a5b825a1b51a84af66 caps=[Human.ask]
   risks=
   none
   ```

4. Typed suspend/resume invariant — suspends as `AskHuman`, resumes with a
   typed `String` response, completes with `Done "Ada"`. Expect exit 0,
   stdout exactly:

   ```sh
   env PROTOSS_GLOBAL_STORE= _build/default/bin/main.exe invariants process examples/golden/human-ask/src/main.protoss --entry askName --response String:Ada
   ```

   ```
   Invariants OK
   kind=process
   source=examples/golden/human-ask/src/main.protoss
   entry=askName
   program_hash=p2:5d26eb91830e9fffb556ab46cfbbbab37d82821df7bb42ca7b13d08ccc0a567c
   request_id=p2:29081582b8fa2d5d8901d648463042d3c72160af73c69c9004042da4fe94dd77
   continuation_id=p2:3c42ed6dc25c4120fe0124e047c9e053cd720ae3e578ac3fb82e97e1e211a74a
   result=Done "Ada"
   ```
