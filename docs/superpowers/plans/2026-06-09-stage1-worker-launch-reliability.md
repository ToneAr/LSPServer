# Stage 1: Worker-Kernel Launch Reliability — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the LSP's background worker kernel launch reliably, prove it's alive, tell the user (and a status command) whether it's up, and retry/relaunch on failure — degrading gracefully to synchronous execution at all times.

**Architecture:** Refactor the existing `launchDiagnosticsKernel` into a robust `launchWorkerKernel` helper (load `Parallel``, enable built-in auto-relaunch, `LaunchKernels[1]` with error capture, health-check, provision, notify). Add retry/backoff + periodic health-check driven from `ProcessScheduledJobs`, and a `worker_kernel_status` execute-command. This is Stage 1 of the off-main-thread architecture (spec: `docs/superpowers/specs/2026-06-09-off-main-thread-architecture-design.md`).

**Tech Stack:** Wolfram Language (LSPServer paclet), Parallel` subkernels, MUnit `.wlt` tests.

---

## Spec

Implements Stage 1 of `docs/superpowers/specs/2026-06-09-off-main-thread-architecture-design.md`. Read it first.

## Critical workflow notes (project conventions)

- Tests load the paclet from `build/paclet/`. After editing `LSPServer/Kernel/*.wl`, **copy to `build/paclet/` before running tests**; never edit `build/paclet/` directly.
- `build/paclet/` is gitignored but its `.wl` files are force-tracked — commit them with `git add -f`.
- `VerificationTest` uses `SameQ`. Use TDD: failing test first.
- `Tests/ServerInternals.wlt` has **7 pre-existing failures** (Formatting×2, FoldingRange, ProcessScheduledJobs-Starts-Sync-Closed-File-Sweep, DispatchWorkspaceDiagnostics-Dedupes, CodeAction-Ignore-QuickFixes, DefinitionFencepost-Skips-Malformed) — that is the regression baseline; do not aim for zero.

**Sync command:**
```
cp LSPServer/Kernel/LSPServer.wl build/paclet/LSPServer/Kernel/LSPServer.wl
cp LSPServer/Kernel/Workspace.wl build/paclet/LSPServer/Kernel/Workspace.wl
```
**Test-run command:**
```
wolframscript -code 'Needs["MUnit`"]; r = TestReport["Tests/WorkerKernel.wlt"]; Print["Passed: ", r["TestsSucceededCount"], " Failed: ", r["TestsFailedCount"]]; Scan[Print["FAIL ", #["TestID"]]&, Cases[r["TestResults"], _?(#["Outcome"]=!="Success"&)]]'
```

## File Structure

- **Modify** `LSPServer/Kernel/LSPServer.wl`: new worker-lifecycle state + helpers (`workerKernelHealthyQ`, `notifyWorkerStatus`, `scheduleWorkerRelaunch`, `workerStatusReport`, `launchWorkerKernel`), refactor `launchDiagnosticsKernel`, retry/backoff + periodic health-check in `ProcessScheduledJobs`, register `worker_kernel_status` in `$ExecuteCommandProvider`.
- **Modify** `LSPServer/Kernel/Workspace.wl`: handle `worker_kernel_status` in the `executeCommand` Switch.
- **Create** `Tests/WorkerKernel.wlt`: unit tests for the kernel-free logic + one opt-in integration test.

All new helpers live in the `LSPServer`Private`` region of `LSPServer.wl` (the same region as `launchDiagnosticsKernel`).

---

## Task 1: Worker-lifecycle state + test scaffold

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (export list near line 84; top-level init near line 853; `StartServer` reset block near line 920-936)
- Create: `Tests/WorkerKernel.wlt`

- [ ] **Step 1: Create the test file with loader + smoke test**

`Tests/WorkerKernel.wlt`:
```wolfram
(* Load LSPServer from the repository's build/paclet so in-tree changes are picked up. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "build", "paclet"}]]];
<<LSPServer`
LSPServer`LoadAllFeatureModules[];

(* Stage 1 worker-kernel launch reliability tests. *)

VerificationTest[
  True,
  True,
  TestID -> "WorkerKernel-loader-smoke"
]
```

- [ ] **Step 2: Add new state symbol declarations**

In `LSPServer/Kernel/LSPServer.wl`, find the export line (around line 84):
```wolfram
$DiagnosticsKernelLaunchAfter
```
Replace with:
```wolfram
$DiagnosticsKernelLaunchAfter
$WorkerLaunchAttempts
$WorkerLastFailureReason
$WorkerStatusNotified
```

- [ ] **Step 3: Add top-level defaults**

In `LSPServer/Kernel/LSPServer.wl`, find (around line 853):
```wolfram
$DiagnosticsKernelLaunchAfter = None
```
Replace with:
```wolfram
$DiagnosticsKernelLaunchAfter = None
$WorkerLaunchAttempts = 0
$WorkerLastFailureReason = None
$WorkerStatusNotified = False
$WorkerMaxLaunchAttempts = 4
$WorkerBackoffSchedule = {5, 15, 45, 120}
$WorkerHealthCheckInterval = 10
$WorkerLastHealthCheck = 0
```

- [ ] **Step 4: Reset new state in StartServer**

In `LSPServer/Kernel/LSPServer.wl`, find (around line 920):
```wolfram
  $DiagnosticsKernelLaunchAfter = None;
```
Replace with:
```wolfram
  $DiagnosticsKernelLaunchAfter = None;
  $WorkerLaunchAttempts         = 0;
  $WorkerLastFailureReason      = None;
  $WorkerStatusNotified         = False;
  $WorkerLastHealthCheck        = 0;
```

- [ ] **Step 5: Sync + run smoke test**

Run the sync + test commands. Expected: `Passed: 1 Failed: 0`.

- [ ] **Step 6: Commit**
```bash
git add LSPServer/Kernel/LSPServer.wl Tests/WorkerKernel.wlt
git add -f build/paclet/LSPServer/Kernel/LSPServer.wl
git commit -m "stage1: worker-lifecycle state + test scaffold"
```

---

## Task 2: `workerKernelHealthyQ` helper

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (add helper just above `launchDiagnosticsKernel[]`, ~line 1168)
- Test: `Tests/WorkerKernel.wlt`

- [ ] **Step 1: Write the failing tests**

Append to `Tests/WorkerKernel.wlt`:
```wolfram
(* Health check returns False for non-kernel sentinels without touching ParallelEvaluate. *)
VerificationTest[
  {LSPServer`Private`workerKernelHealthyQ[None],
   LSPServer`Private`workerKernelHealthyQ[$Failed]},
  {False, False},
  TestID -> "workerKernelHealthyQ-rejects-sentinels"
]
```

- [ ] **Step 2: Sync + run to verify it fails**

Expected: FAIL (`workerKernelHealthyQ` undefined → returns unevaluated, not `{False,False}`).

- [ ] **Step 3: Implement the helper**

In `LSPServer/Kernel/LSPServer.wl`, immediately above `launchDiagnosticsKernel[] :=` (~line 1168), insert:
```wolfram
(*
workerKernelHealthyQ[kernel] — True only if kernel is a live subkernel that
answers a trivial round-trip within a timeout. Never throws.
*)
workerKernelHealthyQ[kernel_] :=
  kernel =!= None && kernel =!= $Failed &&
  Quiet[TimeConstrained[ParallelEvaluate[1 + 1, kernel], 5, $TimedOut]] === 2

```

- [ ] **Step 4: Sync + run to verify it passes**

Expected: `workerKernelHealthyQ-rejects-sentinels` PASSES; `Passed: 2 Failed: 0`.

- [ ] **Step 5: Commit**
```bash
git add LSPServer/Kernel/LSPServer.wl Tests/WorkerKernel.wlt
git add -f build/paclet/LSPServer/Kernel/LSPServer.wl
git commit -m "stage1: workerKernelHealthyQ health-check helper"
```

---

## Task 3: `notifyWorkerStatus` helper

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (add helper above `launchDiagnosticsKernel[]`)
- Test: `Tests/WorkerKernel.wlt`

- [ ] **Step 1: Write the failing tests**

Append to `Tests/WorkerKernel.wlt`:
```wolfram
(* Failure notification: enqueues a Warning window/showMessage containing the reason. *)
VerificationTest[
  Module[{msgs},
    LSPServer`$ContentQueue = {};
    LSPServer`$WorkerStatusNotified = False;
    LSPServer`Private`notifyWorkerStatus[False, "boom"];
    msgs = Cases[LSPServer`$ContentQueue,
      KeyValuePattern["method" -> "window/showMessage"]];
    {Length[msgs],
     Lookup[Lookup[First[msgs, <||>], "params", <||>], "type", None],
     StringContainsQ[Lookup[Lookup[First[msgs, <||>], "params", <||>], "message", ""], "boom"]}
  ],
  {1, 2, True},
  TestID -> "notifyWorkerStatus-failure-warns"
]

(* Success notification is sent at most once (guarded by $WorkerStatusNotified). *)
VerificationTest[
  Module[{},
    LSPServer`$ContentQueue = {};
    LSPServer`$WorkerStatusNotified = False;
    LSPServer`Private`notifyWorkerStatus[True, ""];
    LSPServer`Private`notifyWorkerStatus[True, ""];
    Count[LSPServer`$ContentQueue, KeyValuePattern["method" -> "window/showMessage"]]
  ],
  1,
  TestID -> "notifyWorkerStatus-success-once"
]
```

> Note: `$MessageType["Warning"]` is `2` (defined in LSPServer.wl).

- [ ] **Step 2: Sync + run to verify they fail**

Expected: both FAIL (`notifyWorkerStatus` undefined).

- [ ] **Step 3: Implement the helper**

In `LSPServer/Kernel/LSPServer.wl`, above `launchDiagnosticsKernel[] :=`, insert:
```wolfram
(*
notifyWorkerStatus[ok, reason] — enqueue a window/showMessage so the user knows
whether the background worker started. Failures always notify (Warning, with
reason). Success notifies once (Info), guarded by $WorkerStatusNotified.
*)
notifyWorkerStatus[ok_, reason_] :=
Module[{type, message},
  If[TrueQ[ok],
    If[TrueQ[$WorkerStatusNotified], Return[Null]];
    $WorkerStatusNotified = True;
    type = $MessageType["Info"];
    message = "LSPServer: background worker kernel started."
  ,
    type = $MessageType["Warning"];
    message = "LSPServer: background worker kernel failed to start; " <>
      "diagnostics and coloring will run on the main thread (slower). Reason: " <>
      ToString[reason]
  ];
  appendContentsToContentQueue[{
    <|"method" -> "window/showMessage", "params" -> <|"type" -> type, "message" -> message|>|>
  }];
  Null
]

```

- [ ] **Step 4: Sync + run to verify they pass**

Expected: both PASS; `Passed: 4 Failed: 0`.

- [ ] **Step 5: Commit**
```bash
git add LSPServer/Kernel/LSPServer.wl Tests/WorkerKernel.wlt
git add -f build/paclet/LSPServer/Kernel/LSPServer.wl
git commit -m "stage1: notifyWorkerStatus user-visible worker status"
```

---

## Task 4: `scheduleWorkerRelaunch` (retry/backoff)

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (add helper above `launchDiagnosticsKernel[]`)
- Test: `Tests/WorkerKernel.wlt`

- [ ] **Step 1: Write the failing tests**

Append to `Tests/WorkerKernel.wlt`:
```wolfram
(* First few failures schedule a future relaunch with increasing backoff. *)
VerificationTest[
  Module[{t0, sched1, sched2},
    LSPServer`$WorkerLaunchAttempts = 1;
    LSPServer`$DiagnosticsKernelLaunchAfter = None;
    t0 = AbsoluteTime[];
    LSPServer`Private`scheduleWorkerRelaunch[];
    sched1 = LSPServer`$DiagnosticsKernelLaunchAfter;
    LSPServer`$WorkerLaunchAttempts = 2;
    LSPServer`$DiagnosticsKernelLaunchAfter = None;
    LSPServer`Private`scheduleWorkerRelaunch[];
    sched2 = LSPServer`$DiagnosticsKernelLaunchAfter;
    {NumberQ[sched1] && sched1 > t0, NumberQ[sched2] && sched2 >= sched1}
  ],
  {True, True},
  TestID -> "scheduleWorkerRelaunch-backoff-increases"
]

(* After the attempt cap, no further relaunch is scheduled. *)
VerificationTest[
  Module[{},
    LSPServer`$WorkerLaunchAttempts = 99;
    LSPServer`$DiagnosticsKernelLaunchAfter = None;
    LSPServer`Private`scheduleWorkerRelaunch[];
    LSPServer`$DiagnosticsKernelLaunchAfter
  ],
  None,
  TestID -> "scheduleWorkerRelaunch-stops-after-cap"
]
```

- [ ] **Step 2: Sync + run to verify they fail**

Expected: both FAIL (`scheduleWorkerRelaunch` undefined).

- [ ] **Step 3: Implement the helper**

In `LSPServer/Kernel/LSPServer.wl`, above `launchDiagnosticsKernel[] :=`, insert:
```wolfram
(*
scheduleWorkerRelaunch[] — schedule the next launch attempt with exponential
backoff, or give up (leaving $DiagnosticsKernelLaunchAfter = None) once the
attempt cap is reached. $WorkerLaunchAttempts is the count of attempts already made.
*)
scheduleWorkerRelaunch[] :=
Module[{idx, delay},
  If[$WorkerLaunchAttempts >= $WorkerMaxLaunchAttempts,
    $DiagnosticsKernelLaunchAfter = None;
    Return[Null]
  ];
  idx = Min[$WorkerLaunchAttempts, Length[$WorkerBackoffSchedule]];
  idx = Max[idx, 1];
  delay = $WorkerBackoffSchedule[[idx]];
  $DiagnosticsKernelLaunchAfter = AbsoluteTime[] + delay;
  Null
]

```

- [ ] **Step 4: Sync + run to verify they pass**

Expected: both PASS; `Passed: 6 Failed: 0`.

- [ ] **Step 5: Commit**
```bash
git add LSPServer/Kernel/LSPServer.wl Tests/WorkerKernel.wlt
git add -f build/paclet/LSPServer/Kernel/LSPServer.wl
git commit -m "stage1: scheduleWorkerRelaunch retry/backoff"
```

---

## Task 5: `workerStatusReport` + `worker_kernel_status` command

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (add `workerStatusReport[]` helper above `launchDiagnosticsKernel[]`; register command in `$ExecuteCommandProvider` ~line 386)
- Modify: `LSPServer/Kernel/Workspace.wl` (add Switch case ~line 130)
- Test: `Tests/WorkerKernel.wlt`

- [ ] **Step 1: Write the failing test**

Append to `Tests/WorkerKernel.wlt`:
```wolfram
(* Status command returns a report assoc reflecting current worker state. *)
VerificationTest[
  Module[{res, report},
    LSPServer`$DiagnosticsKernel = None;
    LSPServer`$WorkerLaunchAttempts = 3;
    LSPServer`$WorkerLastFailureReason = "nope";
    res = LSPServer`handleContent[<|
      "method" -> "workspace/executeCommand",
      "id" -> 5,
      "params" -> <|"command" -> "worker_kernel_status"|>
    |>];
    report = Lookup[First[res, <||>], "result", <||>];
    {Lookup[report, "running", "?"], Lookup[report, "attempts", "?"],
     Lookup[report, "lastFailureReason", "?"]}
  ],
  {False, 3, "nope"},
  TestID -> "worker-kernel-status-command"
]
```

- [ ] **Step 2: Sync + run to verify it fails**

Expected: FAIL (command unhandled → default returns `{}`, so the report keys are missing → `{"?","?","?"}`).

- [ ] **Step 3: Implement `workerStatusReport[]`**

In `LSPServer/Kernel/LSPServer.wl`, above `launchDiagnosticsKernel[] :=`, insert:
```wolfram
(*
workerStatusReport[] — a plain association describing worker state, for the
worker_kernel_status execute-command and logging.
*)
workerStatusReport[] :=
  <|
    "running" -> (workerKernelHealthyQ[$DiagnosticsKernel]),
    "kernelBin" -> Replace[$DiagnosticsKernelBin, Except[_String] -> Null],
    "attempts" -> $WorkerLaunchAttempts,
    "lastFailureReason" -> Replace[$WorkerLastFailureReason, Except[_String] -> Null]
  |>

```

> `workerKernelHealthyQ[None]` is `False` and does not call `ParallelEvaluate`, so this is safe with no live kernel.

- [ ] **Step 4: Register the command**

In `LSPServer/Kernel/LSPServer.wl`, find (in `$ExecuteCommandProvider`, ~line 386):
```wolfram
    "toggle_inlay_hints",
```
Replace with:
```wolfram
    "toggle_inlay_hints",
    (*
    worker_kernel_status reports background-worker launch state (debug/diagnosis)
    *)
    "worker_kernel_status",
```

- [ ] **Step 5: Handle the command in Workspace.wl**

In `LSPServer/Kernel/Workspace.wl`, find (~line 130):
```wolfram
          payload_responsiveness_test is an undocumented, debug command
          *)
          "payload_responsiveness_test",

            log[1, "payload_responsiveness_test:> \n\n"];

            {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>, <| "method" -> "payloadTest", "payload" -> StringJoin@Flatten@Table[CharacterRange["a", "z"], 100000] |>}
          ,
          _,
```
Replace with:
```wolfram
          payload_responsiveness_test is an undocumented, debug command
          *)
          "payload_responsiveness_test",

            log[1, "payload_responsiveness_test:> \n\n"];

            {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>, <| "method" -> "payloadTest", "payload" -> StringJoin@Flatten@Table[CharacterRange["a", "z"], 100000] |>}
          ,
          (*
          worker_kernel_status returns the background-worker status report.
          *)
          "worker_kernel_status",

            {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> LSPServer`Private`workerStatusReport[] |>}
          ,
          _,
```

- [ ] **Step 6: Sync + run to verify it passes**

Sync (both LSPServer.wl and Workspace.wl) + run.
Expected: `worker-kernel-status-command` PASSES; `Passed: 7 Failed: 0`.

- [ ] **Step 7: Commit**
```bash
git add LSPServer/Kernel/LSPServer.wl LSPServer/Kernel/Workspace.wl Tests/WorkerKernel.wlt
git add -f build/paclet/LSPServer/Kernel/LSPServer.wl build/paclet/LSPServer/Kernel/Workspace.wl
git commit -m "stage1: worker_kernel_status execute-command + report"
```

---

## Task 6: `launchWorkerKernel` + refactor `launchDiagnosticsKernel`

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (`launchDiagnosticsKernel`, lines ~1168-1227)
- Test: `Tests/WorkerKernel.wlt` (behavioral assertion that does not require a live kernel: a failed launch records a reason, notifies, and schedules a retry)

- [ ] **Step 1: Write the failing test**

This test forces the launch to fail by setting an impossible attempt state and stubbing the low-level launch to `$Failed` via a Block on `LaunchKernels`. Append to `Tests/WorkerKernel.wlt`:
```wolfram
(* A failed launch records the reason, notifies the user, and schedules a retry. *)
VerificationTest[
  Module[{notified, scheduled},
    LSPServer`$ContentQueue = {};
    LSPServer`$WorkerStatusNotified = False;
    LSPServer`$WorkerLaunchAttempts = 0;
    LSPServer`$DiagnosticsKernel = None;
    LSPServer`$DiagnosticsKernelLaunchAfter = None;
    LSPServer`$WorkerLastFailureReason = None;
    (* Force LaunchKernels to fail. *)
    Block[{Parallel`Kernels`LaunchKernels = ($Failed &), LaunchKernels = ($Failed &)},
      LSPServer`Private`launchWorkerKernel[]
    ];
    notified = Count[LSPServer`$ContentQueue, KeyValuePattern["method" -> "window/showMessage"]] >= 1;
    scheduled = NumberQ[LSPServer`$DiagnosticsKernelLaunchAfter];
    {LSPServer`$DiagnosticsKernel === $Failed,
     LSPServer`$WorkerLaunchAttempts >= 1,
     notified, scheduled}
  ],
  {True, True, True, True},
  TestID -> "launchWorkerKernel-failure-notifies-and-retries"
]
```

- [ ] **Step 2: Sync + run to verify it fails**

Expected: FAIL (`launchWorkerKernel` undefined).

- [ ] **Step 3: Implement `launchWorkerKernel` and refactor `launchDiagnosticsKernel`**

In `LSPServer/Kernel/LSPServer.wl`, replace the entire current `launchDiagnosticsKernel[] := …` definition (lines ~1168-1227) with:
```wolfram
(*
launchWorkerKernel[] — robustly launch the background worker subkernel.
Increments $WorkerLaunchAttempts. On success: provisions the kernel, sets
$DiagnosticsKernel and $DiagnosticsKernelBin, notifies (once). On failure:
records $WorkerLastFailureReason, sets $DiagnosticsKernel = $Failed, notifies,
and schedules a backoff retry. Always returns Null.
*)
launchWorkerKernel[] :=
Module[{kernel = $Failed, setupResult = $Failed, reason = "unknown"},
  If[$DiagnosticsKernel =!= None && $DiagnosticsKernel =!= $Failed,
    Return[Null]
  ];

  $WorkerLaunchAttempts = $WorkerLaunchAttempts + 1;

  Quiet[Needs["Parallel`"]];
  (* Built-in auto-relaunch of dead subkernels, belt-and-suspenders with our own. *)
  Quiet[Parallel`Settings`$RelaunchFailedKernels = True];

  kernel = Quiet[
    CheckAbort[
      Check[
        Module[{ks = LaunchKernels[1]},
          If[ListQ[ks] && Length[ks] > 0, First[ks], $Failed]
        ],
        $Failed
      ],
      $Failed
    ]
  ];

  If[!workerKernelHealthyQ[kernel],
    If[kernel =!= $Failed && kernel =!= None,
      Quiet[AbortKernels[kernel]]; Quiet[CloseKernels[kernel]]
    ];
    reason = "LaunchKernels failed or returned an unhealthy kernel (attempt " <>
      ToString[$WorkerLaunchAttempts] <> ")";
    $WorkerLastFailureReason = reason;
    $DiagnosticsKernel = $Failed;
    $DiagnosticsKernelBin = $Failed;
    log[0, "WARNING: worker kernel launch failed (attempt ", $WorkerLaunchAttempts, "): ", reason];
    notifyWorkerStatus[False, reason];
    scheduleWorkerRelaunch[];
    Return[Null]
  ];

  setupResult = Quiet[Check[
    ParallelEvaluate[
      Needs["CodeParser`"]; Needs["CodeInspector`"]; Needs["CodeFormatter`"],
      kernel
    ];
    DistributeDefinitions[
      "LSPServer`", "LSPServer`Private`", "LSPServer`Utils`",
      "LSPServer`PacletIndex`", "LSPServer`Diagnostics`",
      "LSPServer`Diagnostics`Private`",
      kernel
    ],
    $Failed
  ]];

  If[setupResult === $Failed,
    Quiet[AbortKernels[kernel]]; Quiet[CloseKernels[kernel]];
    $WorkerLastFailureReason = "worker provisioning (Needs/DistributeDefinitions) failed";
    $DiagnosticsKernel = $Failed;
    $DiagnosticsKernelBin = $Failed;
    log[0, "WARNING: worker kernel provisioning failed (attempt ", $WorkerLaunchAttempts, ")"];
    notifyWorkerStatus[False, $WorkerLastFailureReason];
    scheduleWorkerRelaunch[];
    Return[Null]
  ];

  $DiagnosticsKernel = kernel;
  $DiagnosticsKernelBin = $CommandLine[[1]];
  $WorkerLastFailureReason = None;
  log[0, "worker kernel launched (attempt ", $WorkerLaunchAttempts, ")"];
  notifyWorkerStatus[True, ""];
  Null
]


(*
Back-compat wrapper: existing call sites use launchDiagnosticsKernel[].
*)
launchDiagnosticsKernel[] := launchWorkerKernel[]
```

> The `Block[{LaunchKernels = ($Failed &)}, …]` in the test makes `LaunchKernels[1]` return `$Failed`, so `kernel` is `$Failed`, `workerKernelHealthyQ` is `False`, and the failure branch runs.

- [ ] **Step 4: Sync + run to verify it passes**

Expected: `launchWorkerKernel-failure-notifies-and-retries` PASSES; `Passed: 8 Failed: 0`.

- [ ] **Step 5: Commit**
```bash
git add LSPServer/Kernel/LSPServer.wl Tests/WorkerKernel.wlt
git add -f build/paclet/LSPServer/Kernel/LSPServer.wl
git commit -m "stage1: robust launchWorkerKernel with health-check, notify, retry"
```

---

## Task 7: Periodic health-check + relaunch-on-death in ProcessScheduledJobs

**Files:**
- Modify: `LSPServer/Kernel/LSPServer.wl` (the kernel-launch trigger in `ProcessScheduledJobs`, ~lines 1483-1486)
- Test: `Tests/WorkerKernel.wlt`

- [ ] **Step 1: Write the failing test**

Append to `Tests/WorkerKernel.wlt`:
```wolfram
(* maybeRelaunchDeadWorker: if the worker is supposed to be live but is unhealthy,
   it is marked failed and a relaunch is scheduled. *)
VerificationTest[
  Module[{},
    (* A bogus non-kernel value stands in for a dead kernel: healthyQ -> False. *)
    LSPServer`$DiagnosticsKernel = "deadkernel";
    LSPServer`$WorkerLaunchAttempts = 1;
    LSPServer`$DiagnosticsKernelLaunchAfter = None;
    LSPServer`$WorkerLastHealthCheck = 0;
    LSPServer`Private`maybeRelaunchDeadWorker[];
    {LSPServer`$DiagnosticsKernel, NumberQ[LSPServer`$DiagnosticsKernelLaunchAfter]}
  ],
  {None, True},
  TestID -> "maybeRelaunchDeadWorker-detects-dead"
]
```

- [ ] **Step 2: Sync + run to verify it fails**

Expected: FAIL (`maybeRelaunchDeadWorker` undefined).

- [ ] **Step 3: Implement `maybeRelaunchDeadWorker` and wire it in**

In `LSPServer/Kernel/LSPServer.wl`, above `launchDiagnosticsKernel[] :=` (now the wrapper), insert:
```wolfram
(*
maybeRelaunchDeadWorker[] — throttled liveness check. If we believe we have a
worker ($DiagnosticsKernel is not None/$Failed) but it fails a health check,
mark it dead (None) and schedule a relaunch. Throttled by $WorkerHealthCheckInterval.
*)
maybeRelaunchDeadWorker[] :=
Module[{},
  If[$DiagnosticsKernel === None || $DiagnosticsKernel === $Failed,
    Return[Null]
  ];
  If[AbsoluteTime[] - $WorkerLastHealthCheck < $WorkerHealthCheckInterval,
    Return[Null]
  ];
  $WorkerLastHealthCheck = AbsoluteTime[];
  If[!workerKernelHealthyQ[$DiagnosticsKernel],
    log[0, "WARNING: worker kernel became unresponsive; relaunching"];
    Quiet[AbortKernels[$DiagnosticsKernel]];
    Quiet[CloseKernels[$DiagnosticsKernel]];
    $DiagnosticsKernel = None;
    scheduleWorkerRelaunch[]
  ];
  Null
]

```

Then find the launch trigger in `ProcessScheduledJobs` (~lines 1483-1486):
```wolfram
  If[$DiagnosticsKernel === None &&
     NumberQ[$DiagnosticsKernelLaunchAfter] &&
     AbsoluteTime[] >= $DiagnosticsKernelLaunchAfter,
    $DiagnosticsKernelLaunchAfter = None;
    launchDiagnosticsKernel[]
  ];
```
Replace with:
```wolfram
  If[$DiagnosticsKernel === None &&
     NumberQ[$DiagnosticsKernelLaunchAfter] &&
     AbsoluteTime[] >= $DiagnosticsKernelLaunchAfter,
    $DiagnosticsKernelLaunchAfter = None;
    launchWorkerKernel[]
  ];

  (* Detect a worker that died/hung and schedule a relaunch (throttled). *)
  maybeRelaunchDeadWorker[];
```

- [ ] **Step 4: Sync + run to verify it passes**

Expected: `maybeRelaunchDeadWorker-detects-dead` PASSES; `Passed: 9 Failed: 0`.

- [ ] **Step 5: Commit**
```bash
git add LSPServer/Kernel/LSPServer.wl Tests/WorkerKernel.wlt
git add -f build/paclet/LSPServer/Kernel/LSPServer.wl
git commit -m "stage1: periodic worker health-check + relaunch-on-death"
```

---

## Task 8: Integration test (opt-in) + full regression + manual verification

**Files:**
- Modify: `Tests/WorkerKernel.wlt` (opt-in integration test)

- [ ] **Step 1: Add an opt-in integration test that actually launches a worker**

Append to `Tests/WorkerKernel.wlt`:
```wolfram
(* OPT-IN integration test: actually launch a subkernel and round-trip. Set the
   environment variable LSP_WORKER_INTEGRATION=1 to run it; otherwise it is a
   trivial pass so the suite stays fast/hermetic. *)
VerificationTest[
  If[Environment["LSP_WORKER_INTEGRATION"] === "1",
    Module[{ks, ok},
      ks = Quiet[Check[LaunchKernels[1], $Failed]];
      ok = LSPServer`Private`workerKernelHealthyQ[
        If[ListQ[ks] && Length[ks] > 0, First[ks], $Failed]];
      If[ListQ[ks], Quiet[CloseKernels[ks]]];
      ok
    ],
    True
  ],
  True,
  TestID -> "worker-launch-integration-optin"
]
```

- [ ] **Step 2: Sync + run the suite (hermetic mode)**

Expected: `Passed: 10 Failed: 0` (integration test trivially passes without the env var).

- [ ] **Step 3: Run the opt-in integration test for real**

Run:
```
LSP_WORKER_INTEGRATION=1 wolframscript -code 'Needs["MUnit`"]; r = TestReport["Tests/WorkerKernel.wlt"]; Print["Passed: ", r["TestsSucceededCount"], " Failed: ", r["TestsFailedCount"]]'
```
Expected: `Passed: 10 Failed: 0` — confirms a real worker launches + round-trips in this environment. If it fails, capture the output (this is the real-world signal for the binary/launch question) and report rather than forcing a pass.

- [ ] **Step 4: Run the existing internals suite for regressions**

```
wolframscript -code 'Needs["MUnit`"]; r = TestReport["Tests/ServerInternals.wlt"]; Print["Passed: ", r["TestsSucceededCount"], " Failed: ", r["TestsFailedCount"]]'
```
Expected: failures **no greater than the baseline 7**.

- [ ] **Step 5: Sync to the installed paclet for manual testing**
```bash
INSTALLED=$(ls -d ~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel | head -1)
cp LSPServer/Kernel/LSPServer.wl "$INSTALLED/LSPServer.wl"
cp LSPServer/Kernel/Workspace.wl "$INSTALLED/Workspace.wl"
echo "Synced to $INSTALLED"
```

- [ ] **Step 6: Manual verification (VSCode and Zed)**

Open a workspace. Within ~10 s you should see either no message (worker up) or a Warning that the worker failed (with a reason). Run the `worker_kernel_status` command (via the client's command palette / executeCommand) and confirm it reports `running: true` and the kernel binary. Confirm the editor still works (diagnostics/hover) regardless.

- [ ] **Step 7: Commit**
```bash
git add Tests/WorkerKernel.wlt
git commit -m "stage1: opt-in worker launch integration test"
```

---

## Self-Review notes (applied)

- **Spec coverage:** launch-with-error-capture + `$RelaunchFailedKernels` → Task 6; health check → Task 2 (helper), Tasks 6/7 (use); user-visible status → Task 3, wired in Task 6; retry/backoff → Task 4, wired Tasks 6/7; relaunch-on-death → Task 7; status command → Task 5; provisioning verification → Task 6. State globals → Task 1. Testing → every task + Task 8.
- **Naming consistency:** `launchWorkerKernel`, `workerKernelHealthyQ`, `notifyWorkerStatus`, `scheduleWorkerRelaunch`, `workerStatusReport`, `maybeRelaunchDeadWorker`; state `$WorkerLaunchAttempts`, `$WorkerLastFailureReason`, `$WorkerStatusNotified`, `$WorkerMaxLaunchAttempts`, `$WorkerBackoffSchedule`, `$WorkerHealthCheckInterval`, `$WorkerLastHealthCheck`. `launchDiagnosticsKernel` retained as a thin wrapper so existing call sites keep working.
- **Explicit-binary override:** intentionally deferred (spec fallback chain) — the captured `$WorkerLastFailureReason` is the signal that tells us whether it's needed; not implemented speculatively (YAGNI).
- **Out of scope (Stages 2-3):** no new work is offloaded; editing speed is unchanged by Stage 1.
