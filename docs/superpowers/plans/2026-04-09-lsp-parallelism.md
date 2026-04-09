# LSP Parallelism & Performance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deliver partial diagnostic results (syntax/scope lints) within ~500ms of a keypress while workspace diagnostics complete in a background WL kernel, and speed up cold-start workspace indexing via `ParallelMap`.

**Architecture:** A new single-message fast tier replaces the current 11-message serial queue, runs synchronously in the main kernel, and publishes partial results immediately. A background parallel kernel (`$DiagnosticsKernel`) runs the expensive workspace diagnostics pass; `ProcessScheduledJobs` polls for its result and merges it into a second publish. At startup, cache-miss files are indexed in parallel via `ParallelMap`.

**Tech Stack:** Wolfram Language, CodeParser`, CodeInspector`, MUnit` (tests), WL Parallel Computing (LaunchKernels / ParallelSubmit / ParallelEvaluate / WaitFor)

**Spec:** `docs/superpowers/specs/2026-04-09-lsp-parallelism-design.md`

---

## File Map

| File | Role in this plan |
|------|-------------------|
| `LSPServer/Kernel/LSPServer.wl` | Add globals; kernel lifecycle in `StartServer`; stale cancellation in `didChangeFencepost`; slow-tier poll in `ProcessScheduledJobs` |
| `LSPServer/Kernel/Diagnostics.wl` | Replace `expandContent[runDiagnostics]`; add `handleContent[runFastDiagnostics]`; add `buildWorkerSnapshot`, `dispatchWorkspaceDiagnostics`, `runWorkspaceDiagnosticsWorker`, `mergeAndPublishWorkspaceLints` |
| `LSPServer/Kernel/PacletIndex.wl` | Add `indexFilePure`; modify `InitializePacletIndex` for parallel init |
| `build/paclet/LSPServer/Kernel/*` | Synced copies (via `cp`) — updated in final task |

**Project sync rule:** ALWAYS edit source (`LSPServer/Kernel/`), never `build/`. Sync with `cp LSPServer/Kernel/File.wl build/paclet/LSPServer/Kernel/File.wl` and also to the installed paclet (`~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel/`).

**Test runner:**
```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/DocComment.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

---

## Task 1: Declare New Globals and Kernel Lifecycle

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (globals section ~line 55; `StartServer` ~line 423)

The background diagnostics kernel must be launched once when the server starts and torn down on exit. Three new globals track it.

- [ ] **Step 1: Add global declarations after line 107 (`$WorkspaceRootPath`) in `LSPServer/Kernel/LSPServer.wl`**

Read the file to find line 107 first. Then add right after `$WorkspaceRootPath`:

```wolfram
$DiagnosticsKernel

$DiagnosticsTask

$DiagnosticsTaskURI

$DiagnosticsTaskResult
```

- [ ] **Step 2: Initialize globals and launch kernel in `StartServer`**

In `StartServer` (around line 423), find the line:
```wolfram
$kernelStartTime = Now;
```
After it, add:
```wolfram
(* Background kernel for async workspace diagnostics *)
$DiagnosticsTask       = None;
$DiagnosticsTaskURI    = None;
$DiagnosticsTaskResult = None;
$DiagnosticsKernel     = Quiet[Check[First[LaunchKernels[1]], $Failed]];
If[$DiagnosticsKernel =!= $Failed,
  (* Load required packages on the worker kernel, then distribute LSPServer definitions *)
  ParallelEvaluate[
    Needs["CodeParser`"];
    Needs["CodeInspector`"];
    Needs["CodeFormatter`"];
    ,
    $DiagnosticsKernel
  ];
  DistributeDefinitions["LSPServer`", "LSPServer`Private`", "LSPServer`PacletIndex`",
    "LSPServer`Diagnostics`"];
  ,
  log[0, "WARNING: LaunchKernels failed — workspace diagnostics will run synchronously"]
];
```

- [ ] **Step 3: Close kernel on server shutdown**

In `LSPServer.wl`, find `exitHard` and `exitGracefully`. Both should call `shutdownLSPComm`. Find where cleanup runs (search for `$ServerState = "shutdown"` or the function that handles `shutdown`). Add before the final exit:

```wolfram
If[$DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
  Quiet[CloseKernels[$DiagnosticsKernel]];
  $DiagnosticsKernel = None
];
```

- [ ] **Step 4: Commit**

```bash
git add LSPServer/Kernel/LSPServer.wl
git commit -m "feat: add \$DiagnosticsKernel globals and lifecycle in StartServer"
```

---

## Task 2: Collapse the 11-Message Pipeline Expansion

**Files:**
- Modify: `LSPServer/Kernel/Diagnostics.wl` (line 107 — `expandContent[runDiagnostics]`)

Currently `expandContent["textDocument/runDiagnostics"]` expands to 11 sub-messages processed one-per-event-loop-iteration. Replace with a single `runFastDiagnostics` message.

- [ ] **Step 1: Write a failing test that verifies the new expansion**

Create `Tests/perf/DiagnosticsPipeline.wlt`:

```wolfram
Needs["LSPServer`"];
Needs["MUnit`"];

(* Verify runDiagnostics now produces exactly 1 sub-message (the fast tier) *)
VerificationTest[
  Module[{fakeContent, result},
    (* Fake a non-stale runDiagnostics content with a dummy URI *)
    $PreExpandContentQueue = {};
    fakeContent = <|
      "method" -> "textDocument/runDiagnostics",
      "params" -> <|"textDocument" -> <|"uri" -> "file:///test.wl"|>|>
    |>;
    result = expandContent[fakeContent, {1}];
    Map[#["method"]&, result]
  ],
  {"textDocument/runFastDiagnostics"},
  TestID -> "RunDiagnosticsExpandsToFastTierOnly"
]
```

- [ ] **Step 2: Run it to confirm it fails**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Failed: 1` (the expansion still returns 11 messages)

- [ ] **Step 3: Replace the 11-message list in `expandContent[runDiagnostics]`**

In `LSPServer/Kernel/Diagnostics.wl` at line 124, replace:
```wolfram
  res = <| "method" -> #, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/suppressedRegions",
    "textDocument/parseIgnoreComments",
    "textDocument/runConcreteDiagnostics",
    "textDocument/aggregateParse",
    "textDocument/runAggregateDiagnostics",
    "textDocument/abstractParse",
    "textDocument/runAbstractDiagnostics",
    "textDocument/runScopingData",
    "textDocument/runScopingDiagnostics",
    "textDocument/runWorkspaceDiagnostics"
  };
```
with:
```wolfram
  res = {<| "method" -> "textDocument/runFastDiagnostics", "params" -> params |>};
```

- [ ] **Step 4: Run the test to confirm it passes**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Passed: 1`

- [ ] **Step 5: Commit**

```bash
git add LSPServer/Kernel/Diagnostics.wl Tests/perf/DiagnosticsPipeline.wlt
git commit -m "feat: collapse runDiagnostics expansion from 11 messages to 1 (fast tier)"
```

---

## Task 3: Write the Fast-Tier Handler

**Files:**
- Modify: `LSPServer/Kernel/Diagnostics.wl` (add `handleContent[runFastDiagnostics]` after the `expandContent` definition, around line 142)

This handler runs all cheap lint passes synchronously in a single event-loop iteration, publishes partial results immediately, then fires the slow tier.

- [ ] **Step 1: Add the test for fast-tier output to `Tests/perf/DiagnosticsPipeline.wlt`**

Add this test to the file:
```wolfram
(* Fast tier produces an immediate publishDiagnostics notification *)
VerificationTest[
  Module[{fakeURI, result},
    fakeURI = "file:///test.wl";
    (* Seed a minimal open-files entry *)
    $OpenFilesMap = <||>;
    $OpenFilesMap[fakeURI] = <|
      "Text" -> "x = 1 + 1",
      "LastChange" -> Now
    |>;
    $ContentQueue = {};
    $DiagnosticsKernel = $Failed;  (* disable slow tier for this test *)
    result = handleContent[<|
      "method" -> "textDocument/runFastDiagnostics",
      "params" -> <|"textDocument" -> <|"uri" -> fakeURI|>|>
    |>];
    (* Result must contain a publishDiagnostics notification *)
    AnyTrue[result, MatchQ[#, KeyValuePattern["method" -> "textDocument/publishDiagnostics"]]&]
  ],
  True,
  TestID -> "FastTierPublishesImmediately"
]
```

- [ ] **Step 2: Run to confirm it fails**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Passed: 1, Failed: 1` (first test passes, new one fails — handler doesn't exist yet)

- [ ] **Step 3: Add `handleContent[runFastDiagnostics]` in `LSPServer/Kernel/Diagnostics.wl`**

Insert this after the closing `]]` of `expandContent[runDiagnostics]` (after line 141):

```wolfram
handleContent[content:KeyValuePattern["method" -> "textDocument/runFastDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, text, cst, agg, ast,
        suppressedRegions, ignoreData,
        cstLints, aggLints, astLints, scopingData, scopingLints,
        fastLints, diagnosticNotification},

  log[1, "textDocument/runFastDiagnostics: enter"];

  params = content["params"];
  doc    = params["textDocument"];
  uri    = doc["uri"];

  If[isStale[$ContentQueue, uri],
    log[2, "stale"];
    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];
  If[entry === Null,
    Throw[Failure["URINotFound", <|"URI" -> uri|>]]
  ];

  (* ── Parse (use UpdateFileIndex-cached artifacts if present) ── *)
  text = Lookup[entry, "PreprocessedText", entry["Text"]];

  cst = Lookup[entry, "CST", Null];
  If[cst === Null,
    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> If[StringEndsQ[uri, ".wl"], "Package", "Script"]]];
    entry["CST"] = cst
  ];

  agg = Lookup[entry, "Agg", Null];
  If[agg === Null,
    agg = CodeParser`Abstract`Aggregate[cst];
    entry["Agg"] = agg
  ];

  ast = Lookup[entry, "AST", Null];
  If[ast === Null,
    ast = CodeParser`Abstract`Abstract[agg];
    entry["AST"] = ast;
    entry["PreviousAST"] = ast
  ];

  (* ── Suppressed regions + ignore comments (cheap, needed before inspect) ── *)
  suppressedRegions = Lookup[entry, "SuppressedRegions",
    getSuppressedRegions[cst]];
  entry["SuppressedRegions"] = suppressedRegions;

  ignoreData = Lookup[entry, "IgnoreData",
    getIgnoreDataFromCST[cst, uri]];
  entry["IgnoreData"] = ignoreData;

  (* ── Lint passes ── *)
  cstLints = Quiet[CodeInspectCST[cst, "SuppressedRegions" -> suppressedRegions]];
  If[!ListQ[cstLints], cstLints = {}];
  entry["CSTLints"] = cstLints;

  aggLints = Quiet[CodeInspectAgg[agg, "SuppressedRegions" -> suppressedRegions]];
  If[!ListQ[aggLints], aggLints = {}];
  entry["AggLints"] = aggLints;

  If[!FailureQ[ast],
    astLints = Quiet[CodeInspectAST[ast, "SuppressedRegions" -> suppressedRegions]];
    If[!ListQ[astLints], astLints = {}];
    entry["ASTLints"] = astLints,
    astLints = {}; entry["ASTLints"] = {}
  ];

  (* ── Scoping ── *)
  scopingData = {};
  scopingLints = {};
  If[!FailureQ[ast],
    scopingData = Quiet[ScopingData[ast]];
    scopingData = Join[scopingData, Quiet[extractMathScopingData[ast]]];
    entry["ScopingData"] = scopingData;
    scopingLints = convertScopingDataToLints[uri, entry, suppressedRegions];
    entry["ScopingLints"] = scopingLints
  ];

  (* Clear workspace lints — they'll be repopulated by the slow tier *)
  entry["WorkspaceLints"] = Null;

  $OpenFilesMap[uri] = entry;

  (* ── Publish partial results immediately ── *)
  fastLints = cstLints ~Join~ aggLints ~Join~ astLints ~Join~ scopingLints;
  diagnosticNotification = buildPublishNotification[uri, entry, fastLints];

  (* ── Fire slow tier (no-op if kernel unavailable) ── *)
  dispatchWorkspaceDiagnostics[uri];

  log[1, "textDocument/runFastDiagnostics: exit"];

  {diagnosticNotification}
]]
```

Note: `getSuppressedRegions`, `getIgnoreDataFromCST`, `convertScopingDataToLints`, and `buildPublishNotification` are helpers you must extract from the existing handlers (see steps 4–5 below).

- [ ] **Step 4: Extract `getSuppressedRegions` helper**

Find `handleContent["textDocument/suppressedRegions"]` in `Diagnostics.wl`. The core logic is the CST → suppressed-regions mapping. Extract it into a named helper function ABOVE the `handleContent` definitions section (near other helpers):

```wolfram
getSuppressedRegions[cst_] :=
  (* copy the body that was previously inside handleContent[suppressedRegions],
     replacing entry["CST"] with cst and entry["SuppressedRegions"] = ... with a Return *)
  Module[{regions},
    (* The existing code calls something like: *)
    regions = CodeInspector`SuppressedRegions[cst];
    regions
  ]
```
Read the actual body of `handleContent["textDocument/suppressedRegions"]` and extract it. The handler currently stores into `entry["SuppressedRegions"]` — the helper should just return the value.

- [ ] **Step 5: Extract `getIgnoreDataFromCST` and `buildPublishNotification` helpers**

`getIgnoreDataFromCST[cst, uri]`: extract the logic from `handleContent["textDocument/parseIgnoreComments"]` that computes ignore data from CST comments. Return the `ignoreData` value.

`buildPublishNotification[uri, entry, lints]`: extract the filter/sort/limit/convert logic from `handleContent["textDocument/publishDiagnostics"]` (lines 2333–2349). The body is:

```wolfram
buildPublishNotification[uri_String, entry_?AssociationQ, lints_List] :=
Module[{lintsWithConfidence, filtered, ignoreData, diagnostics},
  lintsWithConfidence = Cases[lints,
    InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];
  filtered = Cases[lintsWithConfidence,
    InspectionObject[_, _, _, KeyValuePattern[
      ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];
  ignoreData = Lookup[entry, "IgnoreData", GetIgnoreData[uri]];
  filtered = Select[filtered, !ShouldIgnoreDiagnostic[#, ignoreData]&];
  filtered = SortBy[filtered,
    {-severityToInteger[#[[3]]]&, #[[4, Key[Source]]]&}];
  filtered = Take[filtered, UpTo[CodeInspector`Summarize`$DefaultLintLimit]];
  diagnostics = Flatten[lintToDiagnostics /@ filtered];
  <|"jsonrpc" -> "2.0",
    "method" -> "textDocument/publishDiagnostics",
    "params" -> <|"uri" -> uri, "diagnostics" -> diagnostics|>|>
]
```

- [ ] **Step 6: Extract `convertScopingDataToLints` helper**

Read `handleContent["textDocument/runScopingDiagnostics"]` in `Diagnostics.wl` (around line 425). Extract its core body into:

```wolfram
convertScopingDataToLints[uri_String, entry_?AssociationQ, suppressedRegions_] :=
Module[{scopingData, filtered, isActive, lints},
  scopingData = Lookup[entry, "ScopingData", {}];
  (* copy the body from handleContent[runScopingDiagnostics] that converts
     scopingData entries to InspectionObject lints, filters by suppressedRegions *)
  lints  (* return the list *)
]
```

Read the full handler body to copy the conversion logic faithfully.

- [ ] **Step 7: Run the test**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Passed: 2, Failed: 0`

- [ ] **Step 8: Commit**

```bash
git add LSPServer/Kernel/Diagnostics.wl Tests/perf/DiagnosticsPipeline.wlt
git commit -m "feat: add handleContent[runFastDiagnostics] — synchronous fast-tier lint pass"
```

---

## Task 4: Stale-Result Cancellation in `didChangeFencepost`

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (`handleContent[didChangeFencepost]`, ~line 2016)

When a new change arrives, any in-flight slow-tier task for the previous content is immediately stale. Cancel it before it can publish.

- [ ] **Step 1: Add test**

In `Tests/perf/DiagnosticsPipeline.wlt`, add:

```wolfram
VerificationTest[
  Module[{fakeTask, fakeURI},
    fakeURI = "file:///test.wl";
    (* Simulate an in-flight task *)
    $DiagnosticsTask    = "fake-task-sentinel";
    $DiagnosticsTaskURI = fakeURI;
    $DiagnosticsKernel  = $Failed;   (* disable real kernel for test *)
    $ContentQueue = {};
    $OpenFilesMap = <|fakeURI -> <|"PreviousAST" -> Null, "PreviousUserSymbols" -> {}|>|>;
    $didChangeScheduledJobs = {};
    handleContent[<|
      "method" -> "textDocument/didChangeFencepost",
      "params" -> <|
        "textDocument" -> <|"uri" -> fakeURI|>,
        "contentChanges" -> {<|"text" -> "x = 2"|>}
      |>
    |>];
    (* Task should be cleared (set to None after Cancel) *)
    $DiagnosticsTask === None
  ],
  True,
  TestID -> "DidChangeFencepostCancelsStaleTask"
]
```

- [ ] **Step 2: Run to confirm it fails**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Failed: 1`

- [ ] **Step 3: Add cancellation in `handleContent[didChangeFencepost]`**

In `LSPServer/Kernel/LSPServer.wl`, find `handleContent[didChangeFencepost]` at ~line 2016. After the stale check (around line 2035, after `Throw[{}]`), add:

```wolfram
  (* Cancel any in-flight slow-tier diagnostics task — its content is now stale *)
  If[$DiagnosticsTask =!= None,
    Quiet[If[$DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
      AbortKernels[$DiagnosticsKernel]
    ]];
    $DiagnosticsTask = None
  ];
```

Place this AFTER the stale `Throw[{}]` block and BEFORE `changes = params["contentChanges"]`.

- [ ] **Step 4: Run to confirm test passes**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Passed: 3, Failed: 0`

- [ ] **Step 5: Commit**

```bash
git add LSPServer/Kernel/LSPServer.wl Tests/perf/DiagnosticsPipeline.wlt
git commit -m "feat: cancel stale diagnostics task on didChangeFencepost"
```

---

## Task 5: `buildWorkerSnapshot` and `dispatchWorkspaceDiagnostics`

**Files:**
- Modify: `LSPServer/Kernel/Diagnostics.wl` (add two new functions near the start of the Diagnostics section)

`buildWorkerSnapshot` packages all mutable global state the worker needs. `dispatchWorkspaceDiagnostics` submits the task to the background kernel.

- [ ] **Step 1: Add test**

In `Tests/perf/DiagnosticsPipeline.wlt`, add:
```wolfram
VerificationTest[
  Module[{snap},
    $PacletIndex = <|"Symbols" -> <||>, "Files" -> <||>,
      "Contexts" -> <||>, "Dependencies" -> {}, "ContextAliases" -> <||>|>;
    $BuiltinPatterns = <||>;
    $WorkspaceRootPath = "/tmp/testws";
    snap = buildWorkerSnapshot["file:///test.wl"];
    KeyExistsQ[snap, "PacletIndex"] &&
    KeyExistsQ[snap, "BuiltinPatterns"] &&
    KeyExistsQ[snap, "WorkspaceRootPath"]
  ],
  True,
  TestID -> "BuildWorkerSnapshotHasRequiredKeys"
]
```

- [ ] **Step 2: Run to confirm it fails**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Failed: 1`

- [ ] **Step 3: Add `buildWorkerSnapshot` to `LSPServer/Kernel/Diagnostics.wl`**

Add near the top of the Diagnostics Private section (before `expandContent[runDiagnostics]`):

```wolfram
(*
Build a serialisable snapshot of mutable global state for the
background diagnostics worker kernel. The worker sets these globals
from the snapshot before running, so all helper functions work unchanged.
*)
buildWorkerSnapshot[uri_String] :=
Module[{fileEntry},
  fileEntry = Lookup[$PacletIndex["Files"], uri, <||>];
  <|
    "PacletIndex"       -> $PacletIndex,
    "BuiltinPatterns"   -> $BuiltinPatterns,
    "WorkspaceRootPath" -> $WorkspaceRootPath,
    "ConfidenceLevel"   -> $ConfidenceLevel
  |>
]
```

- [ ] **Step 4: Add `dispatchWorkspaceDiagnostics`**

Add immediately after `buildWorkerSnapshot`:

```wolfram
(*
Submit runWorkspaceDiagnosticsWorker to the background kernel.
No-op if the diagnostics kernel is unavailable ($Failed).
*)
dispatchWorkspaceDiagnostics[uri_String] :=
Module[{ast, snapshot},
  If[$DiagnosticsKernel === $Failed || $DiagnosticsKernel === None,
    (* No parallel kernel available — fall back to inline workspace diags *)
    handleContent[<|"method" -> "textDocument/runWorkspaceDiagnostics",
                    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>];
    Return[Null]
  ];
  (* Cancel any in-flight task *)
  If[$DiagnosticsTask =!= None,
    Quiet[AbortKernels[$DiagnosticsKernel]];
    $DiagnosticsTask = None
  ];
  ast      = $OpenFilesMap[uri]["AST"];
  snapshot = buildWorkerSnapshot[uri];
  $DiagnosticsTaskURI = uri;
  $DiagnosticsTask    = Check[
    ParallelSubmit[{$DiagnosticsKernel},
      runWorkspaceDiagnosticsWorker[uri, ast, snapshot]
    ],
    $Failed
  ];
  If[$DiagnosticsTask === $Failed,
    log[0, "WARNING: ParallelSubmit failed for workspace diagnostics"];
    $DiagnosticsTask = None;
    (* Relaunch kernel and retry once *)
    Quiet[CloseKernels[$DiagnosticsKernel]];
    $DiagnosticsKernel = Quiet[Check[First[LaunchKernels[1]], $Failed]];
    If[$DiagnosticsKernel =!= $Failed,
      ParallelEvaluate[
        Needs["CodeParser`"]; Needs["CodeInspector`"]; Needs["CodeFormatter`"],
        $DiagnosticsKernel
      ];
      DistributeDefinitions["LSPServer`", "LSPServer`Private`",
        "LSPServer`PacletIndex`", "LSPServer`Diagnostics`"];
      $DiagnosticsTask = Quiet[Check[
        ParallelSubmit[{$DiagnosticsKernel},
          runWorkspaceDiagnosticsWorker[uri, ast, snapshot]],
        None]]
    ]
  ]
]
```

- [ ] **Step 5: Run tests**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Passed: 4, Failed: 0`

- [ ] **Step 6: Commit**

```bash
git add LSPServer/Kernel/Diagnostics.wl Tests/perf/DiagnosticsPipeline.wlt
git commit -m "feat: add buildWorkerSnapshot and dispatchWorkspaceDiagnostics"
```

---

## Task 6: `runWorkspaceDiagnosticsWorker` with Fused AST Traversal

**Files:**
- Modify: `LSPServer/Kernel/Diagnostics.wl` (add new worker function; optimise internals)

The worker is a pure function that runs in the background kernel. It sets globals from the snapshot (so all existing helpers work unchanged), then runs the workspace diagnostics logic. The inner `Module` that constructs `localVarMap` has 7+ separate `Cases[ast, ..., Infinity]` calls — replace with a single fused traversal.

- [ ] **Step 1: Read the full body**

Before writing anything, read `LSPServer/Kernel/Diagnostics.wl` from lines 526 to ~2250 to understand the full shape of `handleContent[runWorkspaceDiagnostics]`. Pay attention to:
- All `Cases[ast, ..., Infinity]` calls (the ones to fuse)
- All references to `$PacletIndex`, `$BuiltinPatterns`, `$WorkspaceRootPath` (must be injected from snapshot)
- The shape of the return value (workspace lints list)

- [ ] **Step 2: Write `runWorkspaceDiagnosticsWorker` in `Diagnostics.wl`**

Add the worker function BEFORE `handleContent[runWorkspaceDiagnostics]`. The function has three phases:

**Phase A: Set globals from snapshot** (so all existing helpers work without changes)
**Phase B: Run workspace diagnostics with traversal fusion** (extracted from the existing handler)
**Phase C: Return result Association**

```wolfram
(*
Worker that runs in $DiagnosticsKernel.
Accepts a serialised snapshot of global state — sets globals, runs workspace
diagnostics, returns <|"URI" -> uri, "Lints" -> lints|> or $Failed.
The caller (mergeAndPublishWorkspaceLints) polls for this result.
*)
runWorkspaceDiagnosticsWorker[uri_String, ast_, snapshot_?AssociationQ] :=
Catch[
Module[{workspaceLints},

  (* ── Phase A: Restore globals so all helper functions operate correctly ── *)
  $PacletIndex       = snapshot["PacletIndex"];
  $BuiltinPatterns   = snapshot["BuiltinPatterns"];
  $WorkspaceRootPath = snapshot["WorkspaceRootPath"];
  $ConfidenceLevel   = snapshot["ConfidenceLevel"];

  (* ── Phase B: Run workspace diagnostics ── *)
  (* Copy the BODY of handleContent[runWorkspaceDiagnostics] here, starting
     from the large inner Module{ast, defLHSSources, callNodes, ...} at line 713.
     Replace the 7 separate Cases[ast,...,Infinity] traversals (for defLHSSources,
     localDefMap, closureVarRanges, rawEntries, branchNodes, iterNodes, mapNodes)
     with the single fused traversal below. *)

  workspaceLints = runWorkspaceDiagnosticsCore[uri, ast];

  (* ── Phase C: Return result ── *)
  If[FailureQ[workspaceLints],
    Throw[$Failed]
  ];
  <|"URI" -> uri, "Lints" -> workspaceLints|>
],
_,
$Failed
]
```

- [ ] **Step 3: Extract `runWorkspaceDiagnosticsCore[uri, ast]`**

Add this function — it is the extracted body of `handleContent[runWorkspaceDiagnostics]` (the big Module starting at line 713), with the traversal fusion applied to the `localVarMap` construction.

Replace all seven separate traversals in the `localVarMap` Module. Find these patterns in the source (they are `Cases[ast, pattern, Infinity]` for each of: `defLHSSources`, `localDefMap`, closure nodes, assignment nodes, branch nodes, iterator nodes, and map-param nodes). Replace them with a single `Reap`/`Sow` pass:

```wolfram
(* Single fused traversal replacing 7 separate Cases[ast,...,Infinity] calls.
   Uses Cases with a side-effecting predicate that always returns False,
   so Reap/Sow collects all matching nodes in one tree walk.

   IMPORTANT: All patterns must match CodeParser AST node forms:
   CallNode[LeafNode[Symbol, "HeadName", _], args, data] — NOT standard WL _Head forms.
*)
Module[{defNodes, closureNodes, branchNodes, assignNodes, iterNodes, mapNodes, callNodeList},
  {defNodes, closureNodes, branchNodes, assignNodes, iterNodes, mapNodes, callNodeList} =
    Reap[
      Cases[ast,
        n_ /; (
          (* Definitions (Set/SetDelayed for localDefMap + defLHSSources) *)
          If[MatchQ[n,
               CallNode[LeafNode[Symbol,
                 "Set"|"SetDelayed"|"UpSet"|"UpSetDelayed"|"TagSet"|"TagSetDelayed",
                 _], _, _]],
            Sow[n, "def"]
          ];
          (* Closures (Module/Block/With for closureVarRanges and localVarMap) *)
          If[MatchQ[n,
               CallNode[LeafNode[Symbol, "Module"|"Block"|"With", _], _, _]],
            Sow[n, "closure"]
          ];
          (* Branches (If/Which/Switch for condEntries + convergenceEntries) *)
          If[MatchQ[n,
               CallNode[LeafNode[Symbol, "If"|"Which"|"Switch", _], _, _]],
            Sow[n, "branch"]
          ];
          (* Assignment nodes (Set for rawEntries) *)
          If[MatchQ[n,
               CallNode[LeafNode[Symbol, "Set", _], _, _]],
            Sow[n, "assign"]
          ];
          (* Iterator expressions (Table/Do/Sum/Product/Array for iterEntries) *)
          If[MatchQ[n,
               CallNode[LeafNode[Symbol,
                 "Table"|"Do"|"Sum"|"Product"|"Array", _], _, _]],
            Sow[n, "iter"]
          ];
          (* Map-param expressions for mapParamEntries *)
          If[MatchQ[n,
               CallNode[LeafNode[Symbol,
                 "Map"|"Scan"|"Select"|"Pick"|"MapIndexed"|"MapThread", _], _, _]],
            Sow[n, "map"]
          ];
          (* Call nodes for inputMismatchLints *)
          If[MatchQ[n, CallNode[LeafNode[Symbol, _, _], _, _]],
            Sow[n, "call"]
          ];
          False  (* never collect via Cases — only via Reap *)
        ),
        Infinity
      ],
      {"def", "closure", "branch", "assign", "iter", "map", "call"}
    ][[2]];

  (* Flatten each bucket — Reap returns {{items}, ...} per tag *)
  defNodes     = Flatten[defNodes,     1];
  closureNodes = Flatten[closureNodes, 1];
  branchNodes  = Flatten[branchNodes,  1];
  assignNodes  = Flatten[assignNodes,  1];
  iterNodes    = Flatten[iterNodes,    1];
  mapNodes     = Flatten[mapNodes,     1];
  callNodeList = Flatten[callNodeList, 1];

  (* Now compute defLHSSources, localDefMap, closureVarRanges etc.
     from the pre-collected node lists instead of repeating Cases calls.
     Replace each: Cases[ast, pattern, Infinity] → Select[<bucket>, MatchQ[#, pattern]&]
     for the specific sub-patterns used in localVarMap construction. *)
  ...
]
```

After writing this, go through the original `localVarMap` Module body and replace each `Cases[ast, specificPattern, Infinity]` with `Select[<appropriateBucket>, MatchQ[#, specificPattern]&]`.

- [ ] **Step 4: Add test for worker return shape**

Add to `Tests/perf/DiagnosticsPipeline.wlt`:
```wolfram
VerificationTest[
  Module[{snap, result},
    (* Minimal snapshot *)
    snap = <|
      "PacletIndex" -> <|"Symbols" -> <||>, "Files" -> <||>,
        "Contexts" -> <||>, "Dependencies" -> {}, "ContextAliases" -> <||>|>,
      "BuiltinPatterns" -> <||>,
      "WorkspaceRootPath" -> None,
      "ConfidenceLevel" -> 0.0
    |>;
    result = runWorkspaceDiagnosticsWorker["file:///test.wl",
      (* minimal AST for "x = 1" *)
      CodeParser`Abstract`Abstract[
        CodeParser`Abstract`Aggregate[
          CodeConcreteParse["x = 1"]]],
      snap];
    AssociationQ[result] &&
    KeyExistsQ[result, "URI"] &&
    KeyExistsQ[result, "Lints"]
  ],
  True,
  TestID -> "WorkerReturnsResultAssociation"
]
```

- [ ] **Step 5: Run tests**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Passed: 5, Failed: 0`

- [ ] **Step 6: Commit**

```bash
git add LSPServer/Kernel/Diagnostics.wl Tests/perf/DiagnosticsPipeline.wlt
git commit -m "feat: add runWorkspaceDiagnosticsWorker with fused single-pass AST traversal"
```

---

## Task 7: `ProcessScheduledJobs` Polling and `mergeAndPublishWorkspaceLints`

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (`ProcessScheduledJobs`, ~line 693)
- Modify: `LSPServer/Kernel/Diagnostics.wl` (add `mergeAndPublishWorkspaceLints`)

`ProcessScheduledJobs` runs on every event-loop idle tick. Add a check: if the slow-tier task is done, collect the result and publish merged diagnostics.

- [ ] **Step 1: Add `mergeAndPublishWorkspaceLints` and `handleContent[mergeWorkspaceLints]` to `Diagnostics.wl`**

Add after `dispatchWorkspaceDiagnostics`:

```wolfram
(*
Merges slow-tier worker lints with fast-tier lints and returns
a publishDiagnostics notification. Called by handleContent[mergeWorkspaceLints].
*)
mergeAndPublishWorkspaceLints[taskResult_?AssociationQ] :=
Module[{uri, workspaceLints, entry, allLints},
  uri           = taskResult["URI"];
  workspaceLints = Lookup[taskResult, "Lints", {}];
  entry = Lookup[$OpenFilesMap, uri, Null];
  If[entry === Null, Return[{}]];
  entry["WorkspaceLints"] = workspaceLints;
  $OpenFilesMap[uri] = entry;
  allLints = Lookup[entry, "CSTLints",     {}] ~Join~
             Lookup[entry, "AggLints",     {}] ~Join~
             Lookup[entry, "ASTLints",     {}] ~Join~
             Lookup[entry, "ScopingLints", {}] ~Join~
             workspaceLints;
  {buildPublishNotification[uri, entry, allLints]}
]

mergeAndPublishWorkspaceLints[_] := {}


(*
Invoked by the main event loop when ProcessScheduledJobs detects the slow-
tier task is done. Reads $DiagnosticsTaskResult and publishes merged lints.
*)
handleContent[content:KeyValuePattern["method" -> "textDocument/mergeWorkspaceLints"]] :=
Catch[
Module[{params, doc, uri},
  log[1, "textDocument/mergeWorkspaceLints: enter"];
  params = content["params"];
  doc    = params["textDocument"];
  uri    = doc["uri"];
  If[isStale[$ContentQueue, uri],
    log[2, "stale"];
    $DiagnosticsTaskResult = None;
    Throw[{}]
  ];
  Module[{result},
    result = $DiagnosticsTaskResult;
    $DiagnosticsTaskResult = None;
    log[1, "textDocument/mergeWorkspaceLints: exit"];
    mergeAndPublishWorkspaceLints[result]
  ]
]]
```

- [ ] **Step 2: Add the polling block in `ProcessScheduledJobs` in `LSPServer.wl`**

`ProcessScheduledJobs` can't call `writeLSPResult` directly — the socket is a local in `readEvalWriteLoop`. Instead, follow the existing architecture: when the task is done, append a `textDocument/mergeWorkspaceLints` message to `$ContentQueue`. The main event loop dequeues it, calls `handleContent[mergeWorkspaceLints]` via `LSPEvaluate`, and writes the response.

In `ProcessScheduledJobs` (line 693), immediately after `ProcessPendingIndexFiles[]` (line 709), add:

```wolfram
  (*
  Poll for completed background workspace diagnostics task.
  WaitFor[{eo}, 0.001] returns immediately: {} if still running, {eo} if done.
  When done, queue a mergeWorkspaceLints message for the main event loop to write.
  *)
  If[$DiagnosticsTask =!= None && $DiagnosticsKernel =!= $Failed,
    Module[{done, taskResult},
      done = Quiet[WaitFor[{$DiagnosticsTask}, 0.001]];
      If[Length[done] > 0,
        taskResult = Quiet[WaitAll[done]];
        $DiagnosticsTask = None;
        If[AssociationQ[taskResult] &&
           Lookup[taskResult, "URI", None] === $DiagnosticsTaskURI,
          (* Store the worker result so the handler can retrieve it *)
          $DiagnosticsTaskResult = taskResult;
          (* Queue a merge message — the main loop will write the response *)
          AppendTo[$ContentQueue,
            <|"method" -> "textDocument/mergeWorkspaceLints",
              "params" -> <|"textDocument" ->
                <|"uri" -> $DiagnosticsTaskURI|>|>|>]
        ]
      ]
    ]
  ];
```

Also declare `$DiagnosticsTaskResult` in the globals section (Task 1 additions):
```wolfram
$DiagnosticsTaskResult
```

- [ ] **Step 3: Run regression tests to catch any breakage**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/DocComment.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/builtin/BuiltinVariadic.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: same pass/fail counts as before this PR (pre-existing failures only). Check `MEMORY.md` for known baseline: DocComment 4/19 pass, BuiltinVariadic 8/8 pass.

- [ ] **Step 4: Commit**

```bash
git add LSPServer/Kernel/LSPServer.wl LSPServer/Kernel/Diagnostics.wl
git commit -m "feat: poll slow-tier task in ProcessScheduledJobs and merge workspace lints"
```

---

## Task 8: `indexFilePure` — Side-Effect-Free Indexing

**Files:**
- Modify: `LSPServer/Kernel/PacletIndex.wl` (add `indexFilePure` near `indexFile`, ~line 559)

`indexFile` mutates `$PacletIndex` and writes to disk cache. `indexFilePure` returns the computed result as an Association without side effects, enabling `ParallelMap`.

- [ ] **Step 1: Add test**

Create `Tests/index/IndexFilePure.wlt`:

```wolfram
Needs["LSPServer`"];
Needs["MUnit`"];

VerificationTest[
  Module[{tmp, result},
    (* Write a tiny WL file *)
    tmp = FileNameJoin[{$TemporaryDirectory, "testpure.wl"}];
    Export[tmp, "myFunc[x_] := x + 1", "Text"];
    result = indexFilePure[tmp];
    DeleteFile[tmp];
    AssociationQ[result] &&
    KeyExistsQ[result, "URI"] &&
    KeyExistsQ[result, "Definitions"] &&
    KeyExistsQ[result, "Dependencies"]
  ],
  True,
  TestID -> "IndexFilePureReturnsAssociation"
]

VerificationTest[
  Module[{tmp, before, after},
    tmp = FileNameJoin[{$TemporaryDirectory, "testpure2.wl"}];
    Export[tmp, "myFunc[x_] := x + 1", "Text"];
    before = $PacletIndex;        (* capture state before *)
    indexFilePure[tmp];           (* should NOT mutate $PacletIndex *)
    DeleteFile[tmp];
    $PacletIndex === before
  ],
  True,
  TestID -> "IndexFilePureDoesNotMutatePacletIndex"
]
```

- [ ] **Step 2: Run to confirm both fail**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/index/IndexFilePure.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Failed: 2`

- [ ] **Step 3: Write `indexFilePure` in `LSPServer/Kernel/PacletIndex.wl`**

Add immediately after `indexFile`:

```wolfram
(*
Pure version of indexFile: performs the same parse/extract steps but
returns the result as an Association rather than mutating $PacletIndex.
Used during parallel startup indexing.
Returns <|"URI"->..., "FilePath"->..., "Definitions"->..., ...
          "Dependencies"->..., "ContextLoads"->..., ...
          "PackageContext"->..., "ContextAliases"->...|>
or $Failed.
*)
indexFilePure[filePath_String] :=
Catch[
Module[{text, cst, ast, uri, definitions, usages, symbols, fileDeps,
        contextLoads, explicitContextRefs, packageContext, fileAliases,
        cacheData, modTime},

  uri = "file://" <> filePath;

  (* Check cache first — same logic as indexFile *)
  cacheData = readWorkspaceCacheEntry[uri, filePath];
  If[AssociationQ[cacheData],
    Throw[<|
      "URI"                -> uri,
      "FilePath"           -> filePath,
      "Definitions"        -> Lookup[cacheData, "Definitions",         {}],
      "Usages"             -> Lookup[cacheData, "Usages",              {}],
      "Symbols"            -> Lookup[cacheData, "Symbols",             {}],
      "Dependencies"       -> Lookup[cacheData, "Dependencies",        {}],
      "ContextLoads"       -> Lookup[cacheData, "ContextLoads",        {}],
      "ExplicitContextRefs"-> Lookup[cacheData, "ExplicitContextRefs", {}],
      "PackageContext"     -> Lookup[cacheData, "PackageContext",       None],
      "ContextAliases"     -> Lookup[cacheData, "ContextAliases",      {}],
      "FromCache"          -> True
    |>]
  ];

  (* Cache miss: read + parse + extract *)
  text = Quiet[Import[filePath, "Text"]];
  If[!StringQ[text], Throw[$Failed]];

  cst = Quiet[CodeConcreteParse[text]];
  If[FailureQ[cst], Throw[$Failed]];

  ast = Quiet[CodeParser`Abstract`Abstract[
          CodeParser`Abstract`Aggregate[cst]]];
  If[FailureQ[ast], Throw[$Failed]];

  (* Extract the same fields as indexFile — copy the extraction calls from
     indexFile verbatim. These are the extractXxx[cst, ast, uri] calls that
     produce definitions, usages, symbols, deps, etc. *)
  definitions        = extractDefinitions[cst, ast, uri];
  usages             = extractUsages[ast, uri];
  symbols            = extractSymbols[cst, ast, uri];
  fileDeps           = extractDependencies[ast];
  contextLoads       = extractContextLoads[ast];
  explicitContextRefs= extractExplicitContextRefs[ast];
  packageContext     = extractPackageContext[cst];
  fileAliases        = extractContextAliases[ast];

  <|
    "URI"                -> uri,
    "FilePath"           -> filePath,
    "Definitions"        -> definitions,
    "Usages"             -> usages,
    "Symbols"            -> symbols,
    "Dependencies"       -> fileDeps,
    "ContextLoads"       -> contextLoads,
    "ExplicitContextRefs"-> explicitContextRefs,
    "PackageContext"     -> packageContext,
    "ContextAliases"     -> fileAliases,
    "FromCache"          -> False
  |>
]]
```

Note: read `indexFile` carefully to find the exact extraction function names — copy them, don't guess. They may be `ExtractDefinitions`, `ExtractUsages`, etc.

Then add `mergeIndexResult` that applies one pure result to `$PacletIndex`:

```wolfram
mergeIndexResult[result_?AssociationQ] :=
Module[{uri, deps},
  uri  = result["URI"];
  deps = Lookup[result, "Dependencies", {}];
  addFileToIndex[
    uri,
    Lookup[result, "Definitions",         {}],
    Lookup[result, "Usages",              {}],
    Lookup[result, "Symbols",             {}],
    deps,
    Lookup[result, "ContextLoads",        {}],
    Lookup[result, "ExplicitContextRefs", {}],
    Lookup[result, "PackageContext",       None],
    Lookup[result, "ContextAliases",      {}]
  ];
  If[Length[deps] > 0,
    $PacletIndex["Dependencies"] = DeleteDuplicates[
      Join[$PacletIndex["Dependencies"], deps]
    ];
    loadExternalDependencies[deps]
  ];
  If[!result["FromCache"],
    (* Write cache entry for future startups *)
    writeWorkspaceCacheEntry[uri, result]
  ]
]

mergeIndexResult[$Failed] := Null
mergeIndexResult[_]       := Null
```

- [ ] **Step 4: Run tests**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/index/IndexFilePure.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: `Passed: 2, Failed: 0`

- [ ] **Step 5: Commit**

```bash
git add LSPServer/Kernel/PacletIndex.wl Tests/index/IndexFilePure.wlt
git commit -m "feat: add indexFilePure (side-effect-free) and mergeIndexResult for parallel indexing"
```

---

## Task 9: Parallel `InitializePacletIndex`

**Files:**
- Modify: `LSPServer/Kernel/PacletIndex.wl` (`InitializePacletIndex`, ~line 340)

Replace the `$PendingIndexFiles` cooperative batch queue for cache-miss files with a one-shot `ParallelMap` at startup.

- [ ] **Step 1: Add test**

Add to `Tests/index/IndexFilePure.wlt`:
```wolfram
VerificationTest[
  Module[{tmpDir, files, indexKernels, results},
    (* Create 3 small temp files *)
    tmpDir = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "lsp-par-test"}]];
    files  = Table[
      With[{f = FileNameJoin[{tmpDir, "f" <> ToString[i] <> ".wl"}]},
        Export[f, "f" <> ToString[i] <> "[x_] := x", "Text"]; f],
      {i, 3}];
    (* Parallel index them *)
    indexKernels = Quiet[Check[LaunchKernels[Length[files]], $Failed]];
    If[indexKernels === $Failed,
      DeleteDirectory[tmpDir, DeleteContents -> True];
      Throw["KernelLaunchFailed", "skip"]
    ];
    DistributeDefinitions["LSPServer`PacletIndex`"];
    results = ParallelMap[indexFilePure, files,
      DistributedContexts -> Automatic];
    CloseKernels[indexKernels];
    DeleteDirectory[tmpDir, DeleteContents -> True];
    AllTrue[results, AssociationQ]
  ],
  True,
  TestID -> "ParallelIndexFilePure"
]
```

- [ ] **Step 2: Run to confirm test passes or `KernelLaunchFailed` (acceptable skip)**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/index/IndexFilePure.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```

- [ ] **Step 3: Modify `InitializePacletIndex` in `PacletIndex.wl`**

Find the section at line 450 that sets `$PendingIndexFiles = cacheMisses`. Replace:
```wolfram
  $PendingIndexFiles    = cacheMisses;
```
with:
```wolfram
  If[Length[cacheMisses] > 0,
    (*
    Parallel-index all cache-miss files at startup.
    Falls back to the cooperative batch queue if LaunchKernels fails.
    *)
    Module[{indexKernels, parallelResults},
      indexKernels = Quiet[Check[
        LaunchKernels[Min[Length[cacheMisses], $ProcessorCount]],
        $Failed]];
      If[indexKernels =!= $Failed,
        If[$Debug2,
          log["InitializePacletIndex: parallel indexing ", Length[cacheMisses],
            " cache-miss files on ", Length[indexKernels], " kernels"]
        ];
        DistributeDefinitions["LSPServer`PacletIndex`"];
        parallelResults = ParallelMap[indexFilePure, cacheMisses,
          DistributedContexts -> Automatic,
          Method -> "CoarsestGrained"];
        CloseKernels[indexKernels];
        Scan[mergeIndexResult, parallelResults];
        $PendingIndexFiles = {}  (* all done — no background queue needed *)
        ,
        (* Fallback: serial cooperative batch queue *)
        log[0, "WARNING: LaunchKernels failed — falling back to serial indexing batch queue"];
        $PendingIndexFiles = cacheMisses
      ]
    ],
    $PendingIndexFiles = {}
  ];
```

- [ ] **Step 4: Run regression tests**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/DocComment.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/builtin/BuiltinVariadic.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/index/IndexFilePure.wlt"]; Print["Passed: ", report["TestsSucceededCount"]]; Print["Failed: ", report["TestsFailedCount"]]'
```
Expected: DocComment 4/19, BuiltinVariadic 8/8, IndexFilePure 3/3 (or 2+skip)

- [ ] **Step 5: Commit**

```bash
git add LSPServer/Kernel/PacletIndex.wl Tests/index/IndexFilePure.wlt
git commit -m "feat: parallel InitializePacletIndex using ParallelMap over cache-miss files"
```

---

## Task 10: Sync to Build and Installed, Full Regression Run

**Files:**
- Sync: `LSPServer/Kernel/*.wl` → `build/paclet/LSPServer/Kernel/*.wl` → installed paclet

- [ ] **Step 1: Sync all modified source files to build/paclet**

```bash
for f in LSPServer.wl Diagnostics.wl PacletIndex.wl; do
  cp LSPServer/Kernel/$f build/paclet/LSPServer/Kernel/$f
done
```

- [ ] **Step 2: Sync to installed paclet**

```bash
INSTALLED=$(ls -d ~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel | head -1)
for f in LSPServer.wl Diagnostics.wl PacletIndex.wl; do
  cp LSPServer/Kernel/$f "$INSTALLED/$f"
done
echo "Synced to: $INSTALLED"
```

- [ ] **Step 3: Run the full test suite**

```bash
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/DocComment.wlt"]; Print["DocComment Passed: ", report["TestsSucceededCount"], " Failed: ", report["TestsFailedCount"]]'
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/BranchNarrowing.wlt"]; Print["BranchNarrowing Passed: ", report["TestsSucceededCount"], " Failed: ", report["TestsFailedCount"]]'
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/hover/DocCommentDiag.wlt"]; Print["DocCommentDiag Passed: ", report["TestsSucceededCount"], " Failed: ", report["TestsFailedCount"]]'
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/builtin/BuiltinVariadic.wlt"]; Print["BuiltinVariadic Passed: ", report["TestsSucceededCount"], " Failed: ", report["TestsFailedCount"]]'
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/perf/DiagnosticsPipeline.wlt"]; Print["DiagnosticsPipeline Passed: ", report["TestsSucceededCount"], " Failed: ", report["TestsFailedCount"]]'
wolframscript -code 'Needs["MUnit`"]; report = TestReport["Tests/index/IndexFilePure.wlt"]; Print["IndexFilePure Passed: ", report["TestsSucceededCount"], " Failed: ", report["TestsFailedCount"]]'
```

Expected: all pass counts match or exceed pre-PR baselines. Known pre-existing failures (DocComment 15/19, DocCommentDiag 5/10) are acceptable.

- [ ] **Step 4: Final commit**

```bash
git add build/paclet/LSPServer/Kernel/LSPServer.wl \
        build/paclet/LSPServer/Kernel/Diagnostics.wl \
        build/paclet/LSPServer/Kernel/PacletIndex.wl
git commit -m "chore: sync build/paclet from source after LSP parallelism implementation"
```
