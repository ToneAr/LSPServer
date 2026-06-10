# Stage 2: Per-Edit Speed Wins — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Cut the dominant per-edit main-thread cost (`runIndexUpdate`, ~2.0 s on a 3 k-line file) by removing accidental per-node work in `walkASTForDefinitions`, without changing extraction output.

**Architecture:** `walkASTForDefinitions` is recursive over the whole AST. Today every visit — including every `LeafNode` — rebuilds two `AssociationMap` name-set associations and two Module-local closures before matching anything. Fix: (1) fast-path `LeafNode` dispatch, (2) precompute the name sets once in `extractDefinitions` and pass them inside `structuredContexts`, (3) replace the per-node closures with top-level helpers, (4) stop computing `structuredPackageMetadata` twice per `UpdateFileIndex`. Stage 2 of `docs/superpowers/specs/2026-06-09-off-main-thread-architecture-design.md`.

**Tech Stack:** Wolfram Language (LSPServer paclet), MUnit `.wlt` tests.

---

## Profiling evidence (2026-06-10, Hover.wl 2966 lines, this machine)

Per-edit main-thread total **3667 ms**: parse pipeline 739 ms, fast tokens 190 ms,
**runIndexUpdate 2008 ms**, runScopingData 730 ms. Inside runIndexUpdate:
`extractDefinitions` 1764 ms, of which `walkASTForDefinitions` ≈ 1600 ms
(per-node `AssociationMap`s + closure definitions over ~5 k list-body nodes plus
tens of thousands of leaf visits); `structuredPackageMetadata` ~60–95 ms is
computed twice per update. Parse reuse from `$OpenFilesMap` already works.

## Critical workflow notes (project conventions)

- Tests load the paclet from `build/paclet/`. After editing `LSPServer/Kernel/*.wl`, copy to `build/paclet/` before running tests; never edit `build/paclet/` directly. Commit build copies with `git add -f`.
- Baselines (do not regress): `Tests/ServerInternals.wlt` 76 pass / 7 fail; `Tests/WorkerKernel.wlt` 12/12 (hermetic); `Tests/hover/DocComment.wlt` 4 pass / 15 fail; `Tests/hover/DocCommentDiag.wlt` 5 pass / 5 fail.
- Sync command:
```bash
cp LSPServer/Kernel/PacletIndex.wl build/paclet/LSPServer/Kernel/PacletIndex.wl
```

## File Structure

- **Modify** `LSPServer/Kernel/PacletIndex.wl`:
  - `extractDefinitions[ast, cst, uri]` (~line 2932) and `extractDefinitions[ast, uri]` (~line 2955): add precomputed `"ExportedNameSet"`/`"ScopedNameSet"` keys.
  - New top-level helpers `walkDefinitionContextFor`, `walkDefinitionVisibilityFor` just above `walkASTForDefinitions` (~line 3779).
  - `walkASTForDefinitions`: `LeafNode` fast-path overload; drop per-node name-set/closure construction.
  - `UpdateFileIndex` (~line 4616): pass its `structuredMetadata` into `extractDefinitions` instead of recomputing.
- **Create** `Tests/Performance.wlt`: opt-in (env `LSP_PERF=1`) per-edit index perf gate.

---

## Task 1: Golden baseline + failing perf test

**Files:**
- Create: `Tests/Performance.wlt`
- Create (untracked artifact): `/tmp/defs_golden.wxf`

- [ ] **Step 1: Capture the golden extraction output with the CURRENT build**

```bash
wolframscript -code '
PacletDirectoryLoad[AbsoluteFileName["build/paclet"]];
Needs["LSPServer`"]; LSPServer`LoadAllFeatureModules[]; Needs["CodeParser`"];
text = Import["LSPServer/Kernel/Hover.wl", "Text"] <> "\n(* edit *)\nzzqExtra = 1;\n";
uri = "file:///profile.wl";
cst = CodeParser`CodeConcreteParse[text, "FileFormat" -> "Package"]; cst[[1]] = File;
agg = CodeParser`Abstract`Aggregate[cst]; ast = CodeParser`Abstract`Abstract[agg];
LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text, "CST" -> cst, "Agg" -> agg, "AST" -> ast, "LastChange" -> Now|>|>;
LSPServer`PacletIndex`UpdateFileIndex[uri, text];
defs = LSPServer`PacletIndex`$PacletIndex["Files"][uri]["Definitions"];
Export["/tmp/defs_golden.wxf", defs];
Print["golden defs: ", Length[defs]];'
```
Expected: prints a definition count (≈90+). Keep `/tmp/defs_golden.wxf`.

- [ ] **Step 2: Create the opt-in perf test**

`Tests/Performance.wlt`:
```wolfram
(* Load LSPServer from the repository's build/paclet so in-tree changes are picked up. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "build", "paclet"}]]];
<<LSPServer`
LSPServer`LoadAllFeatureModules[];
Needs["CodeParser`"];

(* OPT-IN perf gate: set LSP_PERF=1 to run. Asserts UpdateFileIndex on a large
   file (with parse artifacts already cached, as in the real didChange flow)
   stays under 1200 ms. Pre-Stage-2 baseline: ~2000 ms; post: ~450 ms. *)
VerificationTest[
  If[Environment["LSP_PERF"] === "1",
    Module[{text, uri, cst, agg, ast, t},
      text = Import[FileNameJoin[{DirectoryName[$TestFileName], "..",
        "LSPServer", "Kernel", "Hover.wl"}], "Text"];
      uri = "file:///perf.wl";
      cst = CodeParser`CodeConcreteParse[text, "FileFormat" -> "Package"];
      cst[[1]] = File;
      agg = CodeParser`Abstract`Aggregate[cst];
      ast = CodeParser`Abstract`Abstract[agg];
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text, "CST" -> cst,
        "Agg" -> agg, "AST" -> ast, "LastChange" -> Now|>|>;
      (* warm caches once, then measure *)
      LSPServer`PacletIndex`UpdateFileIndex[uri, text];
      t = First[AbsoluteTiming[LSPServer`PacletIndex`UpdateFileIndex[uri, text]]];
      Print["UpdateFileIndex warm: ", Round[1000 t], " ms"];
      t < 1.2
    ],
    True
  ],
  True,
  TestID -> "perf-update-file-index-under-budget"
]
```

- [ ] **Step 3: Run to verify it FAILS (red)**

```bash
LSP_PERF=1 wolframscript -code 'Needs["MUnit`"]; r = TestReport["Tests/Performance.wlt"]; Print["Passed: ", r["TestsSucceededCount"], " Failed: ", r["TestsFailedCount"]]'
```
Expected: `Passed: 0 Failed: 1` (current warm time ≈ 2000 ms > 1200 ms).

- [ ] **Step 4: Commit the test**

```bash
git add Tests/Performance.wlt
git commit -m "stage2: opt-in perf gate for UpdateFileIndex (red at ~2s)"
```

---

## Task 2: `LeafNode` fast-path + hoisted name sets/helpers

**Files:**
- Modify: `LSPServer/Kernel/PacletIndex.wl:2932-2971` (both `extractDefinitions` overloads)
- Modify: `LSPServer/Kernel/PacletIndex.wl:3779+` (`walkASTForDefinitions` + new helpers)

- [ ] **Step 1: Add precomputed name sets in both `extractDefinitions` overloads**

In BOTH overloads, the existing
```wolfram
  structuredContexts = Join[structuredContexts, <|
    "ExportedSymbols" -> Replace[Lookup[exportedDeclaredSymbols, "name", {}], _Missing -> {}],
    "ScopedSymbols" -> Replace[Lookup[scopedDeclaredSymbols, "name", {}], _Missing -> {}]
  |>];
```
becomes
```wolfram
  structuredContexts = Join[structuredContexts, <|
    "ExportedSymbols" -> Replace[Lookup[exportedDeclaredSymbols, "name", {}], _Missing -> {}],
    "ScopedSymbols" -> Replace[Lookup[scopedDeclaredSymbols, "name", {}], _Missing -> {}]
  |>];
  (* Precomputed once per file; walkASTForDefinitions must not rebuild these per node. *)
  structuredContexts = Join[structuredContexts, <|
    "ExportedNameSet" -> AssociationMap[True &, structuredContexts["ExportedSymbols"]],
    "ScopedNameSet" -> AssociationMap[True &, structuredContexts["ScopedSymbols"]]
  |>];
```

- [ ] **Step 2: Add the top-level helpers above `walkASTForDefinitions`**

```wolfram
(*
Context/visibility resolution for a definition name, using the name sets
precomputed by extractDefinitions ("ExportedNameSet"/"ScopedNameSet").
Falls back to building the set from the symbol lists if the precomputed keys
are absent (defensive; only pays at definition nodes, not per AST node).
*)
walkNameSet[structuredContexts_, setKey_String, listKey_String] :=
  Lookup[structuredContexts, setKey,
    AssociationMap[True &, Lookup[structuredContexts, listKey, {}]]]

walkDefinitionContextFor[name_String, structuredContexts_, currentContext_] :=
Which[
  TrueQ[Lookup[walkNameSet[structuredContexts, "ExportedNameSet", "ExportedSymbols"], name, False]] &&
      StringQ[Lookup[structuredContexts, "PackageContext", None]],
    Lookup[structuredContexts, "PackageContext", None],
  TrueQ[Lookup[walkNameSet[structuredContexts, "ScopedNameSet", "ScopedSymbols"], name, False]] &&
      StringQ[Lookup[structuredContexts, "PackageScopeContext", None]],
    Lookup[structuredContexts, "PackageScopeContext", None],
  True, currentContext
]

walkDefinitionVisibilityFor[name_String, structuredContexts_, inPrivate_] :=
Which[
  TrueQ[Lookup[walkNameSet[structuredContexts, "ExportedNameSet", "ExportedSymbols"], name, False]], "public",
  TrueQ[Lookup[walkNameSet[structuredContexts, "ScopedNameSet", "ScopedSymbols"], name, False]], "package",
  inPrivate, "private",
  True, "public"
]


(*
Leaf fast-path: a LeafNode can match no definition/context pattern and has no
children — visiting it must cost nothing. The bulk of AST nodes are leaves.
*)
walkASTForDefinitions[_, LeafNode[_, _, _], _, _, _, _, _] := Null
```

- [ ] **Step 3: Strip the per-node construction from the main definition**

In the main `walkASTForDefinitions` body:
- Module locals become `{newContext, contextStrings, newInPrivate, structuredPrivateContext}` (drop `structuredPackageContext`, `structuredPackageScopeContext`, `exportedNames`, `scopedNames`, `definitionContextFor`, `definitionVisibilityFor`).
- Delete the `structuredPackageContext = …`, `structuredPackageScopeContext = …`, `exportedNames = …`, `scopedNames = …` assignments and both local `definitionContextFor[…] := …` / `definitionVisibilityFor[…] := …` definitions. Keep `structuredPrivateContext = Lookup[structuredContexts, "PrivateContext", None];`.
- Replace every call `definitionContextFor[X]` with `walkDefinitionContextFor[X, structuredContexts, newContext]` and every `definitionVisibilityFor[X]` with `walkDefinitionVisibilityFor[X, structuredContexts, newInPrivate]` (sites: function-def branch ×2, TagSet branch ×2, constant branch ×2, Options branch ×2, Attributes branch ×2).

- [ ] **Step 4: Sync + golden equality check**

```bash
cp LSPServer/Kernel/PacletIndex.wl build/paclet/LSPServer/Kernel/PacletIndex.wl
wolframscript -code '
PacletDirectoryLoad[AbsoluteFileName["build/paclet"]];
Needs["LSPServer`"]; LSPServer`LoadAllFeatureModules[]; Needs["CodeParser`"];
text = Import["LSPServer/Kernel/Hover.wl", "Text"] <> "\n(* edit *)\nzzqExtra = 1;\n";
uri = "file:///profile.wl";
cst = CodeParser`CodeConcreteParse[text, "FileFormat" -> "Package"]; cst[[1]] = File;
agg = CodeParser`Abstract`Aggregate[cst]; ast = CodeParser`Abstract`Abstract[agg];
LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text, "CST" -> cst, "Agg" -> agg, "AST" -> ast, "LastChange" -> Now|>|>;
LSPServer`PacletIndex`UpdateFileIndex[uri, text];
defs = LSPServer`PacletIndex`$PacletIndex["Files"][uri]["Definitions"];
golden = Import["/tmp/defs_golden.wxf"];
Print["identical: ", defs === golden];'
```
Expected: `identical: True`. If False, diff the structures before proceeding — output equality is the correctness gate.

- [ ] **Step 5: Run the perf gate (green)**

```bash
LSP_PERF=1 wolframscript -code 'Needs["MUnit`"]; r = TestReport["Tests/Performance.wlt"]; Print["Passed: ", r["TestsSucceededCount"], " Failed: ", r["TestsFailedCount"]]'
```
Expected: `Passed: 1 Failed: 0`, printed warm time well under 1200 ms.

- [ ] **Step 6: Commit**

```bash
git add LSPServer/Kernel/PacletIndex.wl
git add -f build/paclet/LSPServer/Kernel/PacletIndex.wl
git commit -m "stage2: hoist per-node work out of walkASTForDefinitions"
```

---

## Task 3: Single `structuredPackageMetadata` per UpdateFileIndex

**Files:**
- Modify: `LSPServer/Kernel/PacletIndex.wl` (`extractDefinitions[ast, cst, uri]` ~2932; `UpdateFileIndex` ~4718-4728)

- [ ] **Step 1: Extend the 3-arg `extractDefinitions` with an optional metadata arg**

```wolfram
extractDefinitions[ast_, cst_, uri_] :=
  extractDefinitions[ast, cst, uri, Automatic]

extractDefinitions[ast_, cst_, uri_, structuredMetadataIn_] :=
Module[{…existing locals…},
  docComments = ExtractDocComments[cst];
  structuredContexts = If[structuredMetadataIn === Automatic,
    structuredPackageMetadata[uriPath[uri], ast],
    structuredMetadataIn
  ];
  …rest unchanged…
]
```

- [ ] **Step 2: Reorder `UpdateFileIndex` to compute metadata first and pass it**

In `UpdateFileIndex`, move the existing
`structuredMetadata = structuredPackageMetadata[filePath, ast];` line so it runs
BEFORE `definitions = extractDefinitions[ast, cst, uri];`, and change that call to
`definitions = extractDefinitions[ast, cst, uri, structuredMetadata];`.
(`filePath` and `uriPath[uri]` are the same path string — `uriPath` strips the
`file://` scheme exactly as `UpdateFileIndex` does at the top.) Verify this
equivalence by reading `uriPath` before committing; if they differ, pass
`Automatic` instead and skip this task.

- [ ] **Step 3: Sync + golden equality + perf gate again**

Same commands as Task 2 Steps 4–5. Expected: `identical: True`, perf still green.

- [ ] **Step 4: Commit**

```bash
git add LSPServer/Kernel/PacletIndex.wl
git add -f build/paclet/LSPServer/Kernel/PacletIndex.wl
git commit -m "stage2: compute structuredPackageMetadata once per UpdateFileIndex"
```

---

## Task 4: Regression suites + full-cycle re-profile + installed-paclet sync

- [ ] **Step 1: Run the regression suites; compare to baselines**

```bash
wolframscript -code 'Needs["MUnit`"]; Scan[Module[{r = TestReport[#]}, Print[#, " -> Passed: ", r["TestsSucceededCount"], " Failed: ", r["TestsFailedCount"]]]&, {"Tests/ServerInternals.wlt", "Tests/WorkerKernel.wlt", "Tests/hover/DocComment.wlt", "Tests/hover/DocCommentDiag.wlt"}]'
```
Expected: ServerInternals 76/7, WorkerKernel 12/0, DocComment 4/15, DocCommentDiag 5/5 — identical to baselines.

- [ ] **Step 2: Re-profile the full edit cycle**

```bash
wolframscript -file profile_edit_cycle.wls LSPServer/Kernel/Hover.wl
```
Expected: `D runIndexUpdate` ≈ 400–600 ms (was 2008 ms); per-edit total ≈ 2.0–2.2 s (was 3667 ms). Record actual numbers in the commit message.

- [ ] **Step 3: Sync the installed paclet**

```bash
INSTALLED=$(ls -d ~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel | head -1)
cp LSPServer/Kernel/PacletIndex.wl "$INSTALLED/PacletIndex.wl"
echo "Synced to $INSTALLED"
```

- [ ] **Step 4: Commit any remaining artifacts + record results**

```bash
git add docs/superpowers/plans/2026-06-10-stage2-speed-wins.md
git commit -m "stage2: re-profile results after speed wins"
```

---

## Out of scope (deferred to Stage 3)

- `runScopingData` (~730 ms): inherent CodeParser `ScopingData` + full token
  recompute; moves to the worker in Stage 3.
- Parse pipeline (~740 ms) and fast token pass (~190 ms): interactive-feedback
  path; offload/incrementalize in Stage 3.

## Self-Review notes (applied)

- Spec coverage: "parse once" already implemented (artifact reuse verified by
  profiling); "throttle/decouple re-index" — the 0.4 s debounce already exists,
  and after Task 2 the residual index cost (~0.5 s) fits the existing
  yield-point structure; "tokenize once" investigated: fast pass is the
  immediate-feedback path, full recompute happens once scoping exists — no
  redundant compute found beyond design intent, so no task.
- Type consistency: helper names `walkNameSet`, `walkDefinitionContextFor`,
  `walkDefinitionVisibilityFor` used identically in Tasks 2; `extractDefinitions`
  4-arg form only consumed in Task 3.
- No placeholders: every step has runnable code/commands.
