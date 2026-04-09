# LSP Parallelism & Performance Design

**Date:** 2026-04-09
**Branch:** tonya/wl-doc-experiments

## Problem

LSP refresh is unacceptably slow on large files. The root causes:

1. The entire pipeline (11 sub-messages) is serial. Each sub-message burns a 0.1s poll-delay slot → minimum 1.1s overhead before any lints appear.
2. `runWorkspaceDiagnostics` performs 7+ independent full-AST traversals sequentially. This is the dominant compute cost for large files.
3. Workspace indexing at startup processes cache-miss files one-at-a-time in 20-file cooperative batches — bottlenecked by the single event-loop tick rate.

## Goals

- **Primary:** Partial diagnostic results (syntax/scope lints) visible to user within ~200–500ms of a keypress.
- **Secondary:** Full workspace diagnostics follow asynchronously, ~1–3s later.
- **Tertiary:** Workspace indexing startup speed improved via parallelism.

## Non-goals

- Real-time diagnostics on every keystroke (debounce stays at 0.4s).
- Parallelising the fast tier itself (it's already fast enough once the queue overhead is removed).

## Architecture

Three concurrent tracks after a `didChange` event:

```
didChange
    |
    ▼
[FAST TIER — main kernel, synchronous, one event-loop iteration]
  parse (CST → Agg → AST, using cached artifacts if available)
  + concreteLints + aggLints + astLints + scopingLints
  → publishDiagnostics (partial)         ← user sees in ~200-500ms

    |
    ▼
[SLOW TIER — $DiagnosticsKernel, background ParallelSubmit]
  runWorkspaceDiagnosticsWorker[uri, ast, ...]
  (fused single-pass AST traversal)
  → polled by ProcessScheduledJobs
  → publishDiagnostics (full merge)      ← user sees in ~1-3s

[STARTUP — pool of $ProcessorCount kernels]
  ParallelMap[indexFilePure, cacheMisses]
  → results merged into $PacletIndex
  → kernels released
```

**New global state (LSPServer.wl / Diagnostics.wl):**

| Symbol | Purpose |
|---|---|
| `$DiagnosticsKernel` | Single reserved background kernel, launched at server start |
| `$DiagnosticsTask` | In-flight `ParallelSubmit` result object, or `None` |
| `$DiagnosticsTaskURI` | URI the in-flight task is computing for |

## Section 1: Fast Tier

### What changes

`expandContent["textDocument/runDiagnostics"]` (Diagnostics.wl line 107) currently expands to 11 sub-messages. Replace with a single `"textDocument/runFastDiagnostics"` message.

`handleContent[_, "textDocument/runFastDiagnostics", _]` runs synchronously in one event-loop iteration:

1. Read `entry = $OpenFilesMap[uri]`
2. Use cached `CST`/`Agg`/`AST` from `UpdateFileIndex` if present; otherwise parse now (explicit fallback)
3. Run `suppressedRegions` + `parseIgnoreComments` (cheap, inline)
4. `cstLints    = CodeInspectCST[cst, ...]`
5. `aggLints    = CodeInspectAgg[agg, ...]`
6. `astLints    = CodeInspectAST[ast, ...]`
7. `scopingData = ScopingData[ast]; scopingLints = convertScopingLints[scopingData]`
8. Write all results back to `$OpenFilesMap[uri]`
9. Call `publishPartialDiagnostics[id, uri, Join[cstLints, aggLints, astLints, scopingLints]]` — sends `textDocument/publishDiagnostics` immediately
10. Call `dispatchWorkspaceDiagnostics[uri]` — fires slow tier

### publishPartialDiagnostics

Same filtering/sorting/limit logic as the current `publishDiagnostics`, but called with only the fast-tier lints. Marks the notification with a tag so the client knows more lints are coming (optional — LSP doesn't require this; a second publish will replace it).

## Section 2: Slow Tier

### Kernel lifecycle

On server start (`startServer` / `InitializeKernel`), launch one dedicated kernel:

```wolfram
$DiagnosticsKernel = First[LaunchKernels[1]];
$DiagnosticsTask   = None;
```

On server shutdown, `CloseKernels[$DiagnosticsKernel]`.

### dispatchWorkspaceDiagnostics[uri]

```wolfram
dispatchWorkspaceDiagnostics[uri_String] := Module[{ast, snapshot},
  (* Cancel any in-flight task for a different URI or stale content *)
  If[$DiagnosticsTask =!= None, TaskRemove[$DiagnosticsTask]];
  ast      = $OpenFilesMap[uri]["AST"];
  snapshot = buildWorkerSnapshot[uri];   (* see below *)
  $DiagnosticsTaskURI = uri;
  $DiagnosticsTask    = ParallelSubmit[{$DiagnosticsKernel},
    runWorkspaceDiagnosticsWorker[uri, ast, snapshot]
  ]
]
```

`buildWorkerSnapshot[uri]` constructs a serialisable `Association` containing everything the worker needs. This keeps the worker pure — no shared mutable globals. Contents:
- `"PacletIndex"` — deep copy of `$PacletIndex` entries for: (a) all symbols defined in the current file, (b) all symbols referenced by the current file (needed for cross-file type lookup). Use `$PacletIndex["Files"][uri]["ReferencedSymbols"]` to enumerate (b).
- `"BuiltinPatterns"` — `$BuiltinPatterns` (already a constant Association)
- `"WorkspaceRootPath"` — `$WorkspaceRootPath`
- `"LocalDefMap"` — the per-file function definition map (computed inline before dispatch or passed from fast-tier results)
- `"Config"` — relevant per-file lint configuration flags

### ProcessScheduledJobs polling

Add a new polling job in `ProcessScheduledJobs` (StdIO.wl):

```wolfram
If[$DiagnosticsTask =!= None && TaskStatus[$DiagnosticsTask] === "Finished",
  Module[{result = TaskResult[$DiagnosticsTask]},
    $DiagnosticsTask = None;
    If[AssociationQ[result] && result["URI"] === $DiagnosticsTaskURI,
      mergeAndPublishWorkspaceLints[result]
    ]
  ]
]
```

`mergeAndPublishWorkspaceLints` reads the cached fast-tier lints from `$OpenFilesMap`, joins with the workspace lints from the task result, applies the same filter/sort/limit, and sends a second `textDocument/publishDiagnostics` replacing the partial one.

### Worker: runWorkspaceDiagnosticsWorker

This is a pure function that runs in the background kernel. It receives the serialised snapshot and returns `<|"URI" -> uri, "Lints" -> {...}|>` or `$Failed`.

It implements the same logic as the current `runWorkspaceDiagnostics` giant module, with two changes:

1. **Traversal fusion** (see Section 3)
2. All global lookups replaced by snapshot lookups

### Stale-result handling

When a new `didChange` arrives:
- `handleContent[didChangeFencepost]` immediately cancels any in-flight slow-tier task: `If[$DiagnosticsTask =!= None, TaskRemove[$DiagnosticsTask]; $DiagnosticsTask = None]`. This prevents a stale slow-tier result from being published in the window between the new change arriving and the next `runFastDiagnostics` firing.
- After the debounce, the new `runFastDiagnostics` fires and calls `dispatchWorkspaceDiagnostics`, submitting a fresh task.
- If the old task had already finished and been `TaskRemove`d, the polling guard (`=== "Finished"`) skips it safely.

## Section 3: Traversal Fusion

The current `localVarMap` construction in `runWorkspaceDiagnosticsWorker` makes 7+ separate `Cases[ast, ..., Infinity]` traversals. Replace with a **single combined traversal**:

```wolfram
{rawNodes, closureNodes, branchNodes, callNodes, iterNodes, mapNodes} =
  Reap[
    Cases[ast,
      n_ /; (
        MatchQ[n, _SetDelayed | _Set]        && Sow[n, "raw"];
        MatchQ[n, _Module | _Block | _With]  && Sow[n, "closure"];
        MatchQ[n, _If | _Which | _Switch]    && Sow[n, "branch"];
        MatchQ[n, CallNode[LeafNode[Symbol, _, _], _, _]] && Sow[n, "call"];
        MatchQ[n, _Table | _Do | _Sum | _Product | _Array] && Sow[n, "iter"];
        MatchQ[n, _Map | _Scan | _Select | _Pick]         && Sow[n, "map"];
        False
      ),
      Infinity
    ],
    {"raw", "closure", "branch", "call", "iter", "map"}
  ][[2]]
```

This walks the AST once and scatters nodes into per-category buckets. The existing per-category processing logic is unchanged — only the `Cases` calls are eliminated.

## Section 4: Workspace Indexing Parallelism

In `InitializePacletIndex` (PacletIndex.wl), after the cache-hit/miss split:

```wolfram
(* Was: cooperative batch queue for cache misses *)
(* Now: parallel map across sub-kernels *)
If[Length[cacheMisses] > 0,
  indexKernels = LaunchKernels[Min[Length[cacheMisses], $ProcessorCount]];
  DistributeDefinitions[indexFilePure, (* any helpers *)];
  parallelResults = ParallelMap[indexFilePure, cacheMisses,
    DistributedContexts -> Automatic,
    Method -> "CoarsestGrained"
  ];
  CloseKernels[indexKernels];
  (* Merge results into $PacletIndex *)
  Scan[mergeIndexResult, parallelResults]
]
```

`indexFilePure[filePath]` is a version of `indexFile` that returns the index entry as a value (no side effects on `$PacletIndex`). The merge step runs on the main kernel.

The `$PendingIndexFiles` cooperative batch queue is kept for files discovered after startup (e.g. newly created files during editing), since those are rare and the batch approach is adequate there.

## Section 5: Error Handling

| Failure mode | Handling |
|---|---|
| `$DiagnosticsKernel` crashes (returns `$Failed`) | `dispatchWorkspaceDiagnostics` catches with `Check`, relaunches one kernel, logs warning, retries once |
| Slow-tier worker returns `$Failed` | `mergeAndPublishWorkspaceLints` skips the merge; fast-tier partial results remain visible |
| New change arrives before slow tier finishes | `TaskRemove` cancels in-flight task; new task submitted; stale result discarded |
| Parallel indexing: one file fails | `indexFilePure` wraps in `Quiet[Check[..., $Failed]]`; failed files fall back to `$PendingIndexFiles` queue |
| `LaunchKernels` unavailable (no parallel toolkit) | Guard with `Quiet[Check[LaunchKernels[1], $Failed]]`; fall back to serial indexing + serial workspace diags |

## Section 6: Files Changed

| File | Changes |
|---|---|
| `LSPServer/Kernel/Diagnostics.wl` | Replace `expandContent[runDiagnostics]`; add `handleContent[runFastDiagnostics]`; add `dispatchWorkspaceDiagnostics`; add `runWorkspaceDiagnosticsWorker` (pure, serialisable); add `mergeAndPublishWorkspaceLints`; traversal fusion inside worker |
| `LSPServer/Kernel/LSPServer.wl` | Add `$DiagnosticsKernel` / `$DiagnosticsTask` init in `startServer`; add polling block in `ProcessScheduledJobs`; shutdown in server teardown |
| `LSPServer/Kernel/PacletIndex.wl` | Add `indexFilePure` (returns value, no side effects); parallel init in `InitializePacletIndex`; add `mergeIndexResult` |
| `build/paclet/LSPServer/Kernel/*` | Synced copies of all above (via `cp` as per project convention) |

## Section 7: Testing

- Existing test suite (`Tests/hover/*.wlt`, `Tests/builtin/*.wlt`) must continue to pass — the fast-tier handler produces identical lints to the current pipeline.
- Add `Tests/perf/LargeFileDiagnostics.wlt`: time fast tier on a synthetic 500-function file; assert < 2s.
- Manual verification: open a large `.wl` file, confirm partial lints appear before workspace lints, confirm workspace lints arrive and replace partial within a few seconds.
