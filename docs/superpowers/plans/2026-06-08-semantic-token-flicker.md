# Semantic-Token Coloring Flicker Fix — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stop semantic-token syntax coloring from flickering/blanking after edits, across VSCode, Zed, and other LSP clients.

**Architecture:** "Never-blank" token caching. Preserve the last-good token set across edits, serve it during the recompute window instead of blanking, stop the `workspace/semanticTokens/refresh` handler from dropping caches, and deliver freshly-computed tokens via direct fencepost recovery or a single coalesced (cache-preserving) refresh. All changes are localized to `SemanticTokens.wl` and `LSPServer.wl`.

**Tech Stack:** Wolfram Language (LSPServer paclet), MUnit test framework (`.wlt` files via `TestReport`), CodeParser.

---

## Spec

This plan implements `docs/superpowers/specs/2026-06-08-semantic-token-flicker-design.md`. Read it first.

## Critical workflow notes (from project memory)

- **Tests load the paclet from `build/paclet/`** via `PacletDirectoryLoad`. Editing source under `LSPServer/Kernel/` has **no effect on tests** until you copy it into `build/paclet/`. Therefore every "run the test" step below first syncs source → `build/paclet/`.
- **Do NOT edit files under `build/paclet/` directly.** Edit `LSPServer/Kernel/*.wl`, then `cp` into `build/paclet/`.
- Installed-paclet sync (`~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel/`) is only needed for **manual editor verification** (final task), not for `.wlt` tests.
- `VerificationTest` compares with `SameQ` (exact match, no wildcards).

**Sync command (used throughout):**

```bash
cp LSPServer/Kernel/LSPServer.wl build/paclet/LSPServer/Kernel/LSPServer.wl
cp LSPServer/Kernel/SemanticTokens.wl build/paclet/LSPServer/Kernel/SemanticTokens.wl
```

**Test-run command (used throughout):**

```bash
wolframscript -code 'Needs["MUnit`"]; r = TestReport["Tests/SemanticTokensFlicker.wlt"]; Print["Passed: ", r["TestsSucceededCount"], " Failed: ", r["TestsFailedCount"]]; Print["Failed IDs: ", #["TestID"]& /@ Values[r["TestsFailed"]]]'
```

Run all from the repository root.

## File Structure

- **Modify:** `LSPServer/Kernel/SemanticTokens.wl` — token compute/serve path: clear stale flag on fresh compute, gate cache-hit on freshness, serve stale instead of blank, rework `runScopingData` delivery.
- **Modify:** `LSPServer/Kernel/LSPServer.wl` — `didChangeFencepost` (preserve tokens + stale flag), `workspace/semanticTokens/refresh` handler (stop dropping caches), `runIndexUpdate` (coalesced delivery), stuck-refresh timeout.
- **Create:** `Tests/SemanticTokensFlicker.wlt` — targeted regression tests for all of the above.

---

## Task 1: Create the test file scaffold

**Files:**
- Create: `Tests/SemanticTokensFlicker.wlt`

- [ ] **Step 1: Create the test file with the standard loader header**

```wolfram
(* Load LSPServer from the repository's build/paclet so in-tree changes are picked up. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "build", "paclet"}]]];
<<LSPServer`
LSPServer`LoadAllFeatureModules[];
Needs["CodeParser`"];

(* Semantic-token flicker regression tests. Each test sets up global server
   state (LSPServer`$OpenFilesMap, LSPServer`$ContentQueue, etc.), calls a
   handler directly, and inspects the result or resulting state. *)

VerificationTest[
  True,
  True,
  TestID -> "SemanticTokensFlicker-loader-smoke"
]
```

- [ ] **Step 2: Sync and run to confirm the harness loads**

Run the sync command, then the test-run command.
Expected: `Passed: 1 Failed: 0`.

- [ ] **Step 3: Commit**

```bash
git add Tests/SemanticTokensFlicker.wlt
git commit -m "test: scaffold semantic-token flicker regression suite"
```

---

## Task 2: Fresh token computation clears the stale flag

This establishes the invariant the rest of the plan relies on: **any fresh token compute clears `SemanticTokensStale`.** Two compute sites set `entry["SemanticTokens"]`: `computeAndCacheSemanticTokens` and the `fullFencepost` handler's own compute path.

**Files:**
- Modify: `LSPServer/Kernel/SemanticTokens.wl:807-809` (computeAndCacheSemanticTokens)
- Modify: `LSPServer/Kernel/SemanticTokens.wl:1104-1110` (fullFencepost compute path)
- Test: `Tests/SemanticTokensFlicker.wlt`

- [ ] **Step 1: Write the failing test**

Append to `Tests/SemanticTokensFlicker.wlt`:

```wolfram
(* A fresh compute via computeAndCacheSemanticTokens must clear any stale flag. *)
VerificationTest[
  Module[{uri = "file:///clearflag.wl", cst, agg, ast, entry},
    cst = CodeParser`CodeConcreteParse["f[x_] := x\n", "FileFormat" -> "Package"];
    cst[[1]] = File;
    agg = CodeParser`Abstract`Aggregate[cst];
    ast = CodeParser`Abstract`Abstract[agg];
    LSPServer`$OpenFilesMap = <|
      uri -> <|
        "Text" -> "f[x_] := x\n",
        "CST" -> cst,
        "AST" -> ast,
        "SemanticTokens" -> {0, 0, 1, 2, 0},
        "SemanticTokensStale" -> True
      |>
    |>;
    LSPServer`SemanticTokens`computeAndCacheSemanticTokens[uri];
    entry = LSPServer`$OpenFilesMap[uri];
    TrueQ[Lookup[entry, "SemanticTokensStale", False]]
  ],
  False,
  TestID -> "computeAndCache-clears-stale-flag"
]
```

- [ ] **Step 2: Run test to verify it fails**

Sync + run.
Expected: this test FAILS (returns `True` — flag not cleared yet).

- [ ] **Step 3: Implement — clear the flag in `computeAndCacheSemanticTokens`**

In `LSPServer/Kernel/SemanticTokens.wl`, find (lines ~807-809):

```wolfram
  semanticTokens = transformed;
  entry["SemanticTokens"] = semanticTokens;
  entry = KeyDrop[entry, "SemanticTokensIncomplete"];
  $OpenFilesMap[uri] = entry;
```

Replace the `KeyDrop` line so it also drops the stale flag:

```wolfram
  semanticTokens = transformed;
  entry["SemanticTokens"] = semanticTokens;
  entry = KeyDrop[entry, {"SemanticTokensIncomplete", "SemanticTokensStale"}];
  $OpenFilesMap[uri] = entry;
```

- [ ] **Step 4: Implement — clear the flag in the `fullFencepost` compute path**

In `LSPServer/Kernel/SemanticTokens.wl`, find (lines ~1104-1110):

```wolfram
  entry["SemanticTokens"] = semanticTokens;
  If[needsScopingFollowupQ,
    entry["SemanticTokensIncomplete"] = True,
    entry = KeyDrop[entry, "SemanticTokensIncomplete"]
  ];

  $OpenFilesMap[uri] = entry;
```

Replace with (drop the stale flag in both branches — the tokens just computed are fresh for the current text):

```wolfram
  entry["SemanticTokens"] = semanticTokens;
  entry = KeyDrop[entry, "SemanticTokensStale"];
  If[needsScopingFollowupQ,
    entry["SemanticTokensIncomplete"] = True,
    entry = KeyDrop[entry, "SemanticTokensIncomplete"]
  ];

  $OpenFilesMap[uri] = entry;
```

- [ ] **Step 5: Run test to verify it passes**

Sync + run.
Expected: `computeAndCache-clears-stale-flag` PASSES; total `Passed: 2 Failed: 0`.

- [ ] **Step 6: Commit**

```bash
git add LSPServer/Kernel/SemanticTokens.wl build/paclet/LSPServer/Kernel/SemanticTokens.wl Tests/SemanticTokensFlicker.wlt
git commit -m "fix: clear SemanticTokensStale flag whenever tokens are freshly computed"
```

---

## Task 3: `didChangeFencepost` preserves last-good tokens

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl:3418-3432` (didChangeFencepost entry construction)
- Test: `Tests/SemanticTokensFlicker.wlt`

- [ ] **Step 1: Write the failing test**

Append to `Tests/SemanticTokensFlicker.wlt`:

```wolfram
(* An edit must NOT discard the cached tokens; it preserves them and marks them stale. *)
VerificationTest[
  Module[{uri = "file:///edit.wl", entry},
    LSPServer`$ContentQueue = {};
    LSPServer`$OpenFilesMap = <|
      uri -> <|
        "Text" -> "f[x_] := x\n",
        "SemanticTokens" -> {0, 0, 3, 2, 0},
        "AST" -> Null
      |>
    |>;
    LSPServer`handleContent[<|
      "method" -> "textDocument/didChangeFencepost",
      "params" -> <|
        "textDocument" -> <|"uri" -> uri|>,
        "contentChanges" -> {<|"text" -> "g[y_] := y\n"|>}
      |>
    |>];
    entry = LSPServer`$OpenFilesMap[uri];
    {Lookup[entry, "SemanticTokens", Missing["x"]], TrueQ[Lookup[entry, "SemanticTokensStale", False]]}
  ],
  {{0, 0, 3, 2, 0}, True},
  TestID -> "didChangeFencepost-preserves-stale-tokens"
]
```

- [ ] **Step 2: Run test to verify it fails**

Sync + run.
Expected: FAILS (entry is rebuilt without `SemanticTokens`, so first element is `Missing["x"]` and second is `False`).

- [ ] **Step 3: Implement — carry tokens forward in `didChangeFencepost`**

In `LSPServer/Kernel/LSPServer.wl`, find (lines ~3418-3432):

```wolfram
  entry = <|
    "Text" -> text,
    "LastChange" -> Now,
    "ScheduledJobs" -> $didChangeScheduledJobs,
    "IndexUpdatePending" -> True,
    "PreviousAST" -> Lookup[oldEntry, "PreviousAST", Lookup[oldEntry, "AST", Missing["NotAvailable"]]],
    "PreviousUserSymbols" -> Lookup[oldEntry, "PreviousUserSymbols", Lookup[oldEntry, "UserSymbols", Missing["NotAvailable"]]]
  |>;

  (* Pre-process .ipwl files so the parse handlers use annotation-free source *)
  If[StringEndsQ[uri, ".ipwl"],
    entry["PreprocessedText"] = LSPServer`TypeWL`PreprocessIPWL[text][[1]]
  ];
```

Replace with (add the token-preservation block between the entry literal and the `.ipwl` block):

```wolfram
  entry = <|
    "Text" -> text,
    "LastChange" -> Now,
    "ScheduledJobs" -> $didChangeScheduledJobs,
    "IndexUpdatePending" -> True,
    "PreviousAST" -> Lookup[oldEntry, "PreviousAST", Lookup[oldEntry, "AST", Missing["NotAvailable"]]],
    "PreviousUserSymbols" -> Lookup[oldEntry, "PreviousUserSymbols", Lookup[oldEntry, "UserSymbols", Missing["NotAvailable"]]]
  |>;

  (* Never-blank: carry the last-good semantic tokens across the edit and mark
     them stale. The serve path will display these (rather than nothing) until
     fresh tokens are computed, so coloring never goes monochrome on a keystroke. *)
  With[{oldTokens = Lookup[oldEntry, "SemanticTokens", Null]},
    If[oldTokens =!= Null,
      entry["SemanticTokens"] = oldTokens;
      entry["SemanticTokensStale"] = True
    ]
  ];

  (* Pre-process .ipwl files so the parse handlers use annotation-free source *)
  If[StringEndsQ[uri, ".ipwl"],
    entry["PreprocessedText"] = LSPServer`TypeWL`PreprocessIPWL[text][[1]]
  ];
```

- [ ] **Step 4: Run test to verify it passes**

Sync + run.
Expected: `didChangeFencepost-preserves-stale-tokens` PASSES; `Passed: 3 Failed: 0`.

- [ ] **Step 5: Commit**

```bash
git add LSPServer/Kernel/LSPServer.wl build/paclet/LSPServer/Kernel/LSPServer.wl Tests/SemanticTokensFlicker.wlt
git commit -m "fix: preserve last-good semantic tokens across edits (mark stale)"
```

---

## Task 4: `fullFencepost` gates cache-hit on freshness and serves stale instead of blanking

Two sub-changes: (a) a stale cache must not be served as a final fresh hit (otherwise it's never recomputed); (b) when fresh data is not yet available, serve the stale tokens rather than returning `{}`/`Null`.

**Files:**
- Modify: `LSPServer/Kernel/SemanticTokens.wl:842-845` (Module locals), `:889-903` (didchange-stale branch), `:918-929` (cache-hit + waiting-for-reindex branches)
- Test: `Tests/SemanticTokensFlicker.wlt`

- [ ] **Step 1: Write the failing tests**

Append to `Tests/SemanticTokensFlicker.wlt`:

```wolfram
(* When the entry is awaiting reindex but stale tokens exist, the fencepost
   serves the stale tokens (non-empty) rather than blanking. *)
VerificationTest[
  Module[{uri = "file:///reindex.wl"},
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|
        "SemanticTokens" -> {0, 0, 1, 2, 0},
        "SemanticTokensStale" -> True,
        "IndexUpdatePending" -> True
      |>
    |>;
    LSPServer`handleContent[<|
      "method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> 42,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>]
  ],
  {<|"jsonrpc" -> "2.0", "id" -> 42, "result" -> <|"data" -> {0, 0, 1, 2, 0}|>|>},
  TestID -> "fullFencepost-serves-stale-when-reindex-pending"
]

(* A stale cache must NOT short-circuit as a fresh cache-hit when the entry is
   otherwise ready to recompute. Here CST/AST are present and not stale-flagged
   data is fresh: a present-but-fresh cache is still served directly. *)
VerificationTest[
  Module[{uri = "file:///freshhit.wl"},
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|"SemanticTokens" -> {0, 0, 2, 2, 0}|>
    |>;
    LSPServer`handleContent[<|
      "method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> 7,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>]
  ],
  {<|"jsonrpc" -> "2.0", "id" -> 7, "result" -> <|"data" -> {0, 0, 2, 2, 0}|>|>},
  TestID -> "fullFencepost-fresh-cache-hit-still-served"
]
```

- [ ] **Step 2: Run tests to verify the first fails**

Sync + run.
Expected: `fullFencepost-serves-stale-when-reindex-pending` FAILS (current code returns `{}` for the waiting-for-reindex branch). `fullFencepost-fresh-cache-hit-still-served` should PASS already.

- [ ] **Step 3: Implement — add `staleQ` local**

In `LSPServer/Kernel/SemanticTokens.wl`, find the `fullFencepost` handler's Module locals (lines ~842-845):

```wolfram
Module[{id, params, doc, uri, entry, semanticTokens, scopingData, cst, allSymbols,
  scopedSources, globalSymbolTokens, stringTemplateTokens, localTokens, transformed,
  line, char, oldLine, oldChar, needsScopingFollowupQ, scopingEligibleQ,
  fastOnlyQ, classifySymbol},
```

Replace with (only `staleQ` is added to the end of the variable list — the `Module[{` and trailing `},` are unchanged):

```wolfram
Module[{id, params, doc, uri, entry, semanticTokens, scopingData, cst, allSymbols,
  scopedSources, globalSymbolTokens, stringTemplateTokens, localTokens, transformed,
  line, char, oldLine, oldChar, needsScopingFollowupQ, scopingEligibleQ,
  fastOnlyQ, classifySymbol, staleQ},
```

- [ ] **Step 4: Implement — gate the cache-hit on freshness**

In `LSPServer/Kernel/SemanticTokens.wl`, find (lines ~918-924):

```wolfram
  semanticTokens = Lookup[entry, "SemanticTokens", Null];

  If[semanticTokens =!= Null,
    clearPending[];
    log[0, "DBG-ST fencepost: CACHE HIT id=", id, " tokens=", Length[semanticTokens], " uri=", uri];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}]
  ];
```

Replace with:

```wolfram
  semanticTokens = Lookup[entry, "SemanticTokens", Null];
  staleQ = TrueQ[Lookup[entry, "SemanticTokensStale", False]];

  (* Only treat the cache as a final fresh hit when it is NOT stale. A stale
     cache falls through so fresh tokens are recomputed, but is still served as
     a non-blank fallback below if recompute is not yet possible. *)
  If[semanticTokens =!= Null && !staleQ,
    clearPending[];
    log[0, "DBG-ST fencepost: CACHE HIT id=", id, " tokens=", Length[semanticTokens], " uri=", uri];
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}]
  ];
```

- [ ] **Step 5: Implement — serve stale in the waiting-for-reindex branch**

In `LSPServer/Kernel/SemanticTokens.wl`, find (lines ~926-929):

```wolfram
  If[semanticTokensEntryWaitingForReindexQ[entry],
    log[0, "DBG-ST fencepost: WAITING FOR REINDEX id=", id, " uri=", uri];
    Throw[{}]
  ];
```

Replace with:

```wolfram
  If[semanticTokensEntryWaitingForReindexQ[entry],
    If[semanticTokens =!= Null,
      clearPending[];
      log[0, "DBG-ST fencepost: SERVE STALE (reindex pending) id=", id, " tokens=", Length[semanticTokens], " uri=", uri];
      Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}]
    ];
    log[0, "DBG-ST fencepost: WAITING FOR REINDEX id=", id, " uri=", uri];
    Throw[{}]
  ];
```

- [ ] **Step 6: Implement — serve stale in the didChange-stale branch**

In `LSPServer/Kernel/SemanticTokens.wl`, find (lines ~901-903, inside the `If[isStale[$ContentQueue, uri], ...]` block, after the didClose-stale sub-branch):

```wolfram
    log[0, "DBG-ST fencepost: DIDCHANGE-STALE waiting for recovery id=", id, " uri=", uri];
    Throw[{}]
  ];
```

Replace with (serve the last-good tokens during rapid typing instead of blanking):

```wolfram
    log[0, "DBG-ST fencepost: DIDCHANGE-STALE id=", id, " uri=", uri];
    Module[{staleEntry = Lookup[$OpenFilesMap, uri, Null], staleToks},
      staleToks = If[AssociationQ[staleEntry], Lookup[staleEntry, "SemanticTokens", Null], Null];
      If[staleToks =!= Null,
        clearPending[];
        log[0, "DBG-ST fencepost: SERVE STALE (didChange-stale) id=", id, " tokens=", Length[staleToks], " uri=", uri];
        Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> staleToks |> |>}]
      ]
    ];
    Throw[{}]
  ];
```

- [ ] **Step 7: Run tests to verify they pass**

Sync + run.
Expected: both Task-4 tests PASS; `Passed: 5 Failed: 0`.

- [ ] **Step 8: Commit**

```bash
git add LSPServer/Kernel/SemanticTokens.wl build/paclet/LSPServer/Kernel/SemanticTokens.wl Tests/SemanticTokensFlicker.wlt
git commit -m "fix: serve stale semantic tokens instead of blanking; gate cache-hit on freshness"
```

---

## Task 5: `workspace/semanticTokens/refresh` handler stops dropping caches

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl:2613-2619` (refresh handler invalidation block)
- Test: `Tests/SemanticTokensFlicker.wlt`

- [ ] **Step 1: Write the failing test**

Append to `Tests/SemanticTokensFlicker.wlt`:

```wolfram
(* The refresh handler must NOT drop cached tokens. Tokens are fresh in cache by
   the time a refresh is emitted; keeping them makes the re-fetch an instant
   cache-hit with no blank gap. *)
VerificationTest[
  Module[{uri = "file:///refresh.wl"},
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|"SemanticTokens" -> {1, 2, 3, 4, 5}|>
    |>;
    LSPServer`handleContent[<|"method" -> "workspace/semanticTokens/refresh"|>];
    Lookup[LSPServer`$OpenFilesMap[uri], "SemanticTokens", Missing["x"]]
  ],
  {1, 2, 3, 4, 5},
  TestID -> "refresh-handler-keeps-cache"
]
```

- [ ] **Step 2: Run test to verify it fails**

Sync + run.
Expected: FAILS — current handler `KeyDrop`s `SemanticTokens`, so the lookup returns `Missing["x"]`.

- [ ] **Step 3: Implement — remove the cache drop**

In `LSPServer/Kernel/LSPServer.wl`, find (lines ~2613-2619):

```wolfram
      Module[{entry = Lookup[$OpenFilesMap, uri, Null], recoveredCount = 0},
        If[AssociationQ[entry],
          If[KeyExistsQ[entry, "SemanticTokens"],
            (* Clear cached tokens so VS Code fetches fresh ones. *)
            $OpenFilesMap[uri] = KeyDrop[entry, "SemanticTokens"];
            invalidated += 1
          ];
```

Replace with:

```wolfram
      Module[{entry = Lookup[$OpenFilesMap, uri, Null], recoveredCount = 0},
        If[AssociationQ[entry],
          (* Do NOT drop cached tokens. They are already fresh by the time a
             refresh is emitted, so keeping them makes the client's re-fetch an
             instant cache-hit with no blank gap. *)
          If[KeyExistsQ[entry, "SemanticTokens"],
            invalidated += 1
          ];
```

> The rest of the `Module` body (the `recoveredCount = queuePendingSemanticTokenFenceposts[...]` call and `recovered += recoveredCount`) is unchanged.

- [ ] **Step 4: Run test to verify it passes**

Sync + run.
Expected: `refresh-handler-keeps-cache` PASSES; `Passed: 6 Failed: 0`.

- [ ] **Step 5: Commit**

```bash
git add LSPServer/Kernel/LSPServer.wl build/paclet/LSPServer/Kernel/LSPServer.wl Tests/SemanticTokensFlicker.wlt
git commit -m "fix: semanticTokens/refresh handler no longer drops the token cache"
```

---

## Task 6: `runScopingData` stops dropping cache + eager refresh; recomputes and lets delivery handle it

The full-scoping background pass currently drops the token cache and fires an eager global refresh on essentially every edit. Instead: keep the cache, recompute fresh full tokens, and deliver via the unified path (recover pending fenceposts, else one coalesced refresh).

**Files:**
- Modify: `LSPServer/Kernel/SemanticTokens.wl:1242-1253` (already-cached scoping branch), `:1270-1286` (just-computed scoping branch)
- Test: `Tests/SemanticTokensFlicker.wlt`

- [ ] **Step 1: Write the failing test**

Append to `Tests/SemanticTokensFlicker.wlt`:

```wolfram
(* After runScopingData processes an entry whose tokens were stale, the cache
   must remain populated (not dropped). *)
VerificationTest[
  Module[{uri = "file:///scoping.wl", cst, agg, ast, entry},
    cst = CodeParser`CodeConcreteParse["f[x_] := Module[{a}, a]\n", "FileFormat" -> "Package"];
    cst[[1]] = File;
    agg = CodeParser`Abstract`Aggregate[cst];
    ast = CodeParser`Abstract`Abstract[agg];
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PreExpandContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|
        "Text" -> "f[x_] := Module[{a}, a]\n",
        "CST" -> cst,
        "AST" -> ast,
        "SemanticTokens" -> {0, 0, 1, 2, 0},
        "SemanticTokensStale" -> True,
        "SemanticTokensIncomplete" -> True
      |>
    |>;
    LSPServer`handleContent[<|
      "method" -> "textDocument/runScopingData",
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>];
    entry = LSPServer`$OpenFilesMap[uri];
    KeyExistsQ[entry, "SemanticTokens"]
  ],
  True,
  TestID -> "runScopingData-keeps-cache-populated"
]
```

- [ ] **Step 2: Run test to verify it fails**

Sync + run.
Expected: FAILS — current code `KeyDrop`s `SemanticTokens` in the incomplete branch, so `KeyExistsQ` is `False`.

- [ ] **Step 3: Implement — already-cached scoping branch**

In `LSPServer/Kernel/SemanticTokens.wl`, find (lines ~1242-1253):

```wolfram
  If[ListQ[scopingData],
    If[TrueQ[Lookup[entry, "SemanticTokensIncomplete", False]],
      warmSemanticTokenClassifierCaches[];
      entry = KeyDrop[entry, {"SemanticTokens", "SemanticTokensIncomplete"}];
      $OpenFilesMap[uri] = entry;
      LSPServer`Private`queueSemanticTokensRefresh[
        "DBG-ST runScopingData: cached scoping ready; queueing semantic-tokens refresh for " <> uri
      ]
    ];
    log[0, "DBG-ST runScopingData: ALREADY CACHED uri=", uri];
    Throw[{}]
  ];
```

Replace with (recompute full tokens into cache, then deliver without dropping; capture staleness *before* the recompute clears the flag):

```wolfram
  If[ListQ[scopingData],
    If[TrueQ[Lookup[entry, "SemanticTokensIncomplete", False]],
      warmSemanticTokenClassifierCaches[];
      (* Recompute full (scoping-aware) tokens into the cache instead of dropping
         it, then deliver. computeAndCacheSemanticTokens clears the stale and
         incomplete flags, so capture staleness first. *)
      With[{wasStaleBefore = TrueQ[Lookup[entry, "SemanticTokensStale", False]]},
        LSPServer`SemanticTokens`computeAndCacheSemanticTokens[uri];
        LSPServer`Private`deliverFreshSemanticTokens[uri,
          "DBG-ST runScopingData: cached scoping ready; delivering fresh tokens for " <> uri,
          wasStaleBefore
        ]
      ]
    ];
    log[0, "DBG-ST runScopingData: ALREADY CACHED uri=", uri];
    Throw[{}]
  ];
```

- [ ] **Step 4: Implement — just-computed scoping branch**

In `LSPServer/Kernel/SemanticTokens.wl`, find (lines ~1270-1286):

```wolfram
  entry["ScopingData"] = scopingData;
  If[TrueQ[Lookup[entry, "SemanticTokensIncomplete", False]],
    If[scopingTimedOut,
      entry = KeyDrop[entry, "SemanticTokensIncomplete"];
      $OpenFilesMap[uri] = entry;
      log[0, "DBG-ST runScopingData: timed out; keeping fast semantic tokens uri=", uri]
    ,
      warmSemanticTokenClassifierCaches[];
      entry = KeyDrop[entry, {"SemanticTokens", "SemanticTokensIncomplete"}];
      $OpenFilesMap[uri] = entry;
      LSPServer`Private`queueSemanticTokensRefresh[
        "DBG-ST runScopingData: computed scoping; queueing semantic-tokens refresh for " <> uri
      ]
    ]
  ,
    $OpenFilesMap[uri] = entry
  ];
```

Replace with:

```wolfram
  entry["ScopingData"] = scopingData;
  If[TrueQ[Lookup[entry, "SemanticTokensIncomplete", False]],
    If[scopingTimedOut,
      entry = KeyDrop[entry, {"SemanticTokensIncomplete", "SemanticTokensStale"}];
      $OpenFilesMap[uri] = entry;
      log[0, "DBG-ST runScopingData: timed out; keeping fast semantic tokens uri=", uri]
    ,
      $OpenFilesMap[uri] = entry;
      warmSemanticTokenClassifierCaches[];
      (* Recompute full tokens into cache (clears stale + incomplete) and deliver.
         Capture staleness before the recompute clears the flag. *)
      With[{wasStaleBefore = TrueQ[Lookup[entry, "SemanticTokensStale", False]]},
        LSPServer`SemanticTokens`computeAndCacheSemanticTokens[uri];
        LSPServer`Private`deliverFreshSemanticTokens[uri,
          "DBG-ST runScopingData: computed scoping; delivering fresh tokens for " <> uri,
          wasStaleBefore
        ]
      ]
    ]
  ,
    $OpenFilesMap[uri] = entry
  ];
```

> NOTE: `deliverFreshSemanticTokens` is defined in Task 7. This task's test only asserts the cache stays populated, which holds because `computeAndCacheSemanticTokens` repopulates `SemanticTokens`. The call must compile, so a stub is added in Step 5 and replaced in Task 7.

- [ ] **Step 5: Define a temporary stub so this task is runnable in isolation, then run**

To keep tasks independently runnable under TDD, add a minimal stub for the helper near the other private helpers in `LSPServer/Kernel/LSPServer.wl` (immediately above `queueSemanticTokensRefresh[reason_String:""] :=` at ~line 2586). Task 7 replaces this stub with the real implementation. The `args___` form accepts any arity so the stub matches the 3-argument call sites above.

```wolfram
(* Stub — real implementation lands in the fresh-token delivery task. *)
deliverFreshSemanticTokens[args___] := Null
```

Sync + run.
Expected: `runScopingData-keeps-cache-populated` PASSES; all prior tests still pass.

- [ ] **Step 6: Commit**

```bash
git add LSPServer/Kernel/SemanticTokens.wl build/paclet/LSPServer/Kernel/SemanticTokens.wl LSPServer/Kernel/LSPServer.wl build/paclet/LSPServer/Kernel/LSPServer.wl Tests/SemanticTokensFlicker.wlt
git commit -m "fix: runScopingData recomputes tokens in-place instead of dropping cache + eager refresh"
```

---

## Task 7: Implement `deliverFreshSemanticTokens` and wire it into `runIndexUpdate`

The unified delivery helper: after fresh tokens exist in cache, recover any pending (unanswered) fencepost requests directly; if there are none and the displayed tokens were stale, queue exactly one coalesced refresh (cache-preserving, so the re-fetch is instant).

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (replace the Task-6 stub with the real helper, ~line 2586)
- Modify: `LSPServer/Kernel/LSPServer.wl:3540-3545` (runIndexUpdate delivery)
- Test: `Tests/SemanticTokensFlicker.wlt`

- [ ] **Step 1: Write the failing tests**

Append to `Tests/SemanticTokensFlicker.wlt`:

```wolfram
(* deliverFreshSemanticTokens: no pending request + wasStale=True =>
   exactly one coalesced workspace/semanticTokens/refresh is queued. *)
VerificationTest[
  Module[{uri = "file:///deliver1.wl", methods},
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$PendingTokenRefresh = False;
    LSPServer`$OpenFilesMap = <|
      uri -> <|"SemanticTokens" -> {0, 0, 1, 2, 0}|>
    |>;
    LSPServer`Private`deliverFreshSemanticTokens[uri, "", True];
    methods = Lookup[#, "method", None]& /@ LSPServer`$ContentQueue;
    Count[methods, "workspace/semanticTokens/refresh"]
  ],
  1,
  TestID -> "deliverFresh-queues-one-refresh-when-no-pending"
]

(* deliverFreshSemanticTokens: a pending (unanswered, not-yet-queued) request is
   recovered directly, so NO refresh is queued even when wasStale=True. *)
VerificationTest[
  Module[{uri = "file:///deliver2.wl", methods},
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingTokenRefresh = False;
    LSPServer`$PendingSemanticTokenRequests = <|uri -> {99}|>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|"SemanticTokens" -> {0, 0, 1, 2, 0}|>
    |>;
    LSPServer`Private`deliverFreshSemanticTokens[uri, "", True];
    methods = Lookup[#, "method", None]& /@ LSPServer`$ContentQueue;
    {Count[methods, "workspace/semanticTokens/refresh"],
     Count[methods, "textDocument/semanticTokens/fullFencepost"]}
  ],
  {0, 1},
  TestID -> "deliverFresh-recovers-pending-without-refresh"
]

(* deliverFreshSemanticTokens: wasStale=False and no pending => nothing queued
   (tokens already current; no client churn). *)
VerificationTest[
  Module[{uri = "file:///deliver3.wl"},
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$PendingTokenRefresh = False;
    LSPServer`$OpenFilesMap = <|uri -> <|"SemanticTokens" -> {0, 0, 1, 2, 0}|>|>;
    LSPServer`Private`deliverFreshSemanticTokens[uri, "", False];
    Length[LSPServer`$ContentQueue]
  ],
  0,
  TestID -> "deliverFresh-no-op-when-not-stale-and-no-pending"
]
```

- [ ] **Step 2: Run tests to verify they fail**

Sync + run.
Expected: `deliverFresh-queues-one-refresh-when-no-pending` FAILS (stub returns `0`, not `1`); `deliverFresh-recovers-pending-without-refresh` FAILS (stub returns `{0, 0}`, not `{0, 1}`); `deliverFresh-no-op-when-not-stale-and-no-pending` PASSES already (stub queues nothing).

- [ ] **Step 3: Implement — replace the stub with the real helper**

In `LSPServer/Kernel/LSPServer.wl`, find the Task-6 stub (just above `queueSemanticTokensRefresh[reason_String:""] :=`):

```wolfram
(* Stub — real implementation lands in the fresh-token delivery task. *)
deliverFreshSemanticTokens[args___] := Null
```

Replace with the single authoritative definition (staleness is always passed explicitly by callers, because the upstream `computeAndCacheSemanticTokens` clears the flag before delivery):

```wolfram
(*
deliverFreshSemanticTokens[uri, reason, wasStale]

Called after fresh semantic tokens have been written to $OpenFilesMap[uri].
`wasStale` says whether the tokens the client is currently displaying were the
stale, carried-across-the-edit set (callers capture this BEFORE the recompute,
which clears the SemanticTokensStale flag). Delivery is gap-free:
  1. If pending (unanswered, not-yet-queued) fencepost requests exist for uri,
     recover them directly with the fresh tokens (no global churn).
  2. Otherwise, if the displayed tokens were stale, queue ONE coalesced refresh
     so the client re-fetches. Because the refresh handler no longer drops the
     cache, the re-fetch is an instant cache-hit.
*)
deliverFreshSemanticTokens[uri_String, reason_String:"", wasStale_:False] :=
Module[{recovered},
  If[!TrueQ[$SemanticTokens],
    Return[Null]
  ];

  recovered = queuePendingSemanticTokenFenceposts[uri, reason];

  If[recovered == 0 && TrueQ[wasStale],
    queueSemanticTokensRefresh[reason]
  ];

  Null
]
```

> The Task 6 `runScopingData` call sites already use this 3-argument form (capturing `wasStaleBefore` before the compute), so no further changes there are needed.

- [ ] **Step 4: Implement — wire delivery into `runIndexUpdate`**

In `LSPServer/Kernel/LSPServer.wl`, find (lines ~3540-3545):

```wolfram
      If[$SemanticTokens,
        queuePendingSemanticTokenFenceposts[
          uri,
          "DBG-ST: didChange indexed; queuing pending semantic-token fenceposts"
        ]
      ];
```

Replace with (capture staleness before any recompute, then deliver):

```wolfram
      If[$SemanticTokens,
        Module[{wasStaleBefore},
          wasStaleBefore = TrueQ[Lookup[
            Lookup[$OpenFilesMap, uri, <||>], "SemanticTokensStale", False]];
          deliverFreshSemanticTokens[
            uri,
            "DBG-ST: didChange indexed; delivering fresh semantic tokens",
            wasStaleBefore
          ]
        ]
      ];
```

> Rationale: `runIndexUpdate` repopulates `CST`/`AST` but the actual token recompute happens when the recovered/refetched `fullFencepost` runs. The displayed tokens at this point are still the stale set carried across the edit, so `wasStaleBefore` is `True` on a normal edit; with no pending request that yields one coalesced refresh, which triggers the client re-fetch that recomputes fresh tokens.

- [ ] **Step 5: Run tests to verify they pass**

Sync + run.
Expected: both Task-7 tests PASS; all prior tests still pass.

- [ ] **Step 6: Commit**

```bash
git add LSPServer/Kernel/LSPServer.wl build/paclet/LSPServer/Kernel/LSPServer.wl LSPServer/Kernel/SemanticTokens.wl build/paclet/LSPServer/Kernel/SemanticTokens.wl Tests/SemanticTokensFlicker.wlt
git commit -m "feat: gap-free fresh semantic-token delivery (recover pending or one coalesced refresh)"
```

---

## Task 8: Shorten the stuck-refresh safety timeout

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl:1605-1611`
- Test: `Tests/SemanticTokensFlicker.wlt`

- [ ] **Step 1: Write the failing test**

Append to `Tests/SemanticTokensFlicker.wlt`:

```wolfram
(* The stuck-refresh recovery window is 3 seconds (was 10). We assert via the
   ProcessScheduledJobs behavior: a refresh pending for >3s is reset. *)
VerificationTest[
  Module[{},
    LSPServer`$SemanticTokens = True;
    LSPServer`$ServerState = "running";
    LSPServer`$ContentQueue = {};
    LSPServer`$OpenFilesMap = <||>;
    LSPServer`$PendingTokenRefresh = True;
    LSPServer`$PendingTokenRefreshTime = AbsoluteTime[] - 5;
    LSPServer`ProcessScheduledJobs[];
    TrueQ[LSPServer`$PendingTokenRefresh]
  ],
  False,
  TestID -> "stuck-refresh-resets-after-3s"
]
```

- [ ] **Step 2: Run test to verify it fails**

Sync + run.
Expected: FAILS — with the 10s threshold, a 5s-old pending refresh is NOT reset, so the result is `True`.

> If `ProcessScheduledJobs[]` errors in the test due to unrelated state, set the minimal globals it reads at the top of the test: `LSPServer`$DiagnosticsKernel = None; LSPServer`$HoverTask = None; LSPServer`$DiagnosticsTask = None; LSPServer`$WorkspaceBootstrapAfter = None; LSPServer`$WorkspaceDiagnosticsSweepURIs = {}; LSPServer`$QueueLastNonEmptyTime = AbsoluteTime[];` before calling it.

- [ ] **Step 3: Implement — change the threshold to 3 seconds**

In `LSPServer/Kernel/LSPServer.wl`, find (lines ~1605-1611):

```wolfram
  If[TrueQ[$PendingTokenRefresh] &&
     NumberQ[$PendingTokenRefreshTime] &&
     AbsoluteTime[] - $PendingTokenRefreshTime > 10,
    log[0, "DBG-ST: workspace/semanticTokens/refresh ack not received within 10s; resetting $PendingTokenRefresh"];
    $PendingTokenRefresh = False;
    $PendingTokenRefreshTime = None
  ];
```

Replace with:

```wolfram
  If[TrueQ[$PendingTokenRefresh] &&
     NumberQ[$PendingTokenRefreshTime] &&
     AbsoluteTime[] - $PendingTokenRefreshTime > 3,
    log[0, "DBG-ST: workspace/semanticTokens/refresh ack not received within 3s; resetting $PendingTokenRefresh"];
    $PendingTokenRefresh = False;
    $PendingTokenRefreshTime = None
  ];
```

- [ ] **Step 4: Run test to verify it passes**

Sync + run.
Expected: `stuck-refresh-resets-after-3s` PASSES; all prior tests pass.

- [ ] **Step 5: Commit**

```bash
git add LSPServer/Kernel/LSPServer.wl build/paclet/LSPServer/Kernel/LSPServer.wl Tests/SemanticTokensFlicker.wlt
git commit -m "fix: shorten stuck semanticTokens/refresh recovery window 10s -> 3s"
```

---

## Task 9: Full regression run + manual editor verification

**Files:** none (verification only).

- [ ] **Step 1: Sync all sources to build/paclet**

```bash
cp LSPServer/Kernel/LSPServer.wl build/paclet/LSPServer/Kernel/LSPServer.wl
cp LSPServer/Kernel/SemanticTokens.wl build/paclet/LSPServer/Kernel/SemanticTokens.wl
```

- [ ] **Step 2: Run the new flicker suite — all green**

```bash
wolframscript -code 'Needs["MUnit`"]; r = TestReport["Tests/SemanticTokensFlicker.wlt"]; Print["Passed: ", r["TestsSucceededCount"], " Failed: ", r["TestsFailedCount"]]'
```

Expected: `Passed: 11 Failed: 0` (loader smoke + 10 behavior tests).

- [ ] **Step 3: Run the existing internals suite — no regressions**

```bash
wolframscript -code 'Needs["MUnit`"]; r = TestReport["Tests/ServerInternals.wlt"]; Print["Passed: ", r["TestsSucceededCount"], " Failed: ", r["TestsFailedCount"]]; Print["Failed IDs: ", #["TestID"]& /@ Values[r["TestsFailed"]]]'
```

Expected: failures count is **no greater than the pre-change baseline**. (If unsure of the baseline, `git stash` the working tree, run once, record the number, `git stash pop`.)

- [ ] **Step 4: Sync to the installed paclet for live editor testing**

```bash
INSTALLED=$(ls -d ~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel | head -1)
cp LSPServer/Kernel/LSPServer.wl "$INSTALLED/LSPServer.wl"
cp LSPServer/Kernel/SemanticTokens.wl "$INSTALLED/SemanticTokens.wl"
echo "Synced to $INSTALLED"
```

- [ ] **Step 5: Manual verification in VSCode**

Open a `.wl` file with a mix of local variables (`Module[{a,b}, ...]`), parameters (`f[x_]`), system symbols, and undefined symbols. Type rapidly and continuously for several seconds.

Expected: coloring **never goes monochrome**; local variables and parameters do **not** visibly recolor to a different category and back between passes.

- [ ] **Step 6: Manual verification in Zed**

Repeat Step 5 in Zed (the `.wolfram-lsp.zed.log` client). Confirm the same: no monochrome flash on rapid typing.

- [ ] **Step 7: Final commit (if any working-tree drift)**

```bash
git status
# If only the intended files changed and all are committed, nothing to do.
```

---

## Self-Review notes (already applied)

- **Spec coverage:** Section 1 (data model) → Task 3. Section 2 (serve path) → Tasks 2 & 4. Section 3 (recompute & delivery) → Tasks 5, 6, 7. Section 4 (stuck-timeout) → Task 8. Testing → all tasks + Task 9.
- **Helper naming consistency:** `deliverFreshSemanticTokens` (3-arg form is authoritative), `computeAndCacheSemanticTokens`, `queuePendingSemanticTokenFenceposts`, `queueSemanticTokensRefresh` — names match their definitions in `LSPServer.wl` / `SemanticTokens.wl`.
- **Ordering dependency:** Task 6 introduces a stub for `deliverFreshSemanticTokens`; Task 7 replaces it with the real implementation. Do Tasks in order.
