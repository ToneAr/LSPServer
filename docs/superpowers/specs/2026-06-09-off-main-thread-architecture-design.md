# Off-Main-Thread LSP Architecture — Design

- **Date:** 2026-06-09
- **Status:** Approved (design); Stage 1 to be implemented first
- **Goal:** Keep the main LSP kernel responsive by moving heavy work (parsing, symbol indexing, semantic-token computation, diagnostics) off the main thread, plus eliminate redundant per-edit work. Driven by profiling showing ~3 s of synchronous main-thread work per keystroke on a 3000-line file.

## Background / evidence

Profiling (`profile_edit_cycle.wls`, `profile_semantic_tokens.wls`, `spike_worker.wls`) on `LSPServer/Kernel/Hover.wl` (2966 lines, 98 KB):

- One edit blocks the main thread ~3 s: parse pipeline ~720 ms, fast token pass ~185 ms, `runIndexUpdate` ~1.5 s (of which symbol-index extraction ~1.76 s, raw parse ~0.75 s), `runScopingData` ~0.5 s — then more diagnostics work is queued.
- The text is **parsed twice** (token pipeline + `UpdateFileIndex`) and **tokens computed twice** (fast + full) per edit.
- This is ~95 % pre-existing (baseline before the semantic-token flicker work was ~2860 ms; that work added ~5 %).
- A worker kernel **launches** in a headless `wolframscript` (Wolfram 15.0) in ~5.2 s.
- **Result-size constraint:** the CST of a 98 KB file serializes to **26.7 MB** and costs ~1.15 s to ship back over WSTP. Small results (token lists, scoping data, diagnostics) are cheap. **Therefore the parse tree must never cross the WSTP boundary back to the main kernel** — whatever needs the CST/AST must run on the kernel that produced it.
- Offloading buys **responsiveness, not lower total CPU** (WSTP overhead makes total work slightly higher); the win is that the main thread stays free.

## Overall architecture (Approach Y — worker owns the file model)

End state:

- **Main kernel = thin I/O layer.** Owns the LSP transport, content queue, scheduling, and client communication. Holds no heavy parsed model.
- **Worker kernel owns the authoritative model:** per-open-file CST/AST/scoping and the workspace symbol index. Main ships document text/edits; the worker returns only small results (token lists, diagnostics, symbol metadata for cross-file features). The CST/AST never returns to main.
- **One worker is itself single-threaded,** so it runs the same cooperative scheduling the main kernel uses today, but with **interactive jobs (semantic tokens, hover, completion) ordered ahead of batch jobs (indexing, diagnostics)**, and batch jobs chunked so they yield. This prevents the bottleneck from simply moving to the worker.
- **Graceful degradation:** if the worker is unavailable, everything falls back to synchronous main-thread execution (current behavior), so the server always works.

This is delivered in three stages, each its own implementation plan:

- **Stage 1 — Worker-kernel launch reliability** (this document, detailed below). Foundation: make the worker reliably start with the correct kernel binary, health-check it, surface status to the user, retry/relaunch, and degrade gracefully. Both the existing async work (diagnostics/hover) and Stages 2–3 depend on it.
- **Stage 2 — Speed wins** (separate spec). Reduce redundant main-thread work regardless of the worker: parse once (reuse the token pipeline's CST/AST in `UpdateFileIndex`), tokenize once (drop the fast+full double compute where avoidable), and throttle/decouple the heavy symbol re-index from per-keystroke.
- **Stage 3 — Worker owns the file model** (separate spec). Move parsing/scoping/token/index computation onto the worker per Approach Y, with the interactive-priority cooperative scheduler and the synced compact index on main for any features main must answer directly.

Stages 2 and 3 will be specced when reached; this document fully specifies Stage 1.

---

## Stage 1 — Worker-kernel launch reliability (detailed)

### Problem

`launchDiagnosticsKernel[]` (`LSPServer/Kernel/LSPServer.wl:1168-1227`) calls `LaunchKernels[1]`, which uses the **default parallel-kernel configuration**. Its `KernelCommand` may not be the kernel actually running the LSP (the LSP runs `~/.local/bin/WolframKernel`; the spike's default config used `…/Wolfram/15.0/Executables/wolfram`) and may be unavailable under the LSP's `-nostartuppaclets` launch. On failure the code sets `$DiagnosticsKernel = $Failed`, logs a WARNING, and silently runs everything synchronously — and because the LSP is started with no logDir, the user never sees the warning. Result: "async" may already be a no-op in the user's environment.

### Components & changes (all in `LSPServer/Kernel/LSPServer.wl` unless noted)

1. **Correct-binary launch.** Replace the bare `LaunchKernels[1]` with a launch that explicitly uses the running kernel binary `$CommandLine[[1]]`. Use the `SubKernels`LocalKernels`` configuration API to construct a local kernel whose program is that binary (the exact constructor is resolved during implementation; fallback chain: explicit binary → bare `LaunchKernels[1]` → `$Failed`). New helper `launchWorkerKernel[]` returns the kernel object or `$Failed`. `launchDiagnosticsKernel[]` delegates to it.

2. **Health check.** New helper `workerKernelHealthyQ[kernel]`: run `TimeConstrained[ParallelEvaluate[1+1, kernel], 5, $TimedOut]` and require the result `=== 2`. Only mark the worker live after this passes. Used right after launch and to detect a dead worker before submitting work. (Pre-submit health checks during normal operation use a short 0.2 s timeout so a hung worker doesn't stall the main loop.)

3. **User-visible status.** New helper `notifyWorkerStatus[ok, reason]` returns a `window/showMessage` content assoc: on failure, type Warning, message "LSPServer: background worker kernel failed to start; diagnostics and coloring will run on the main thread (slower). Reason: <reason>"; on (first) success, type Info, a brief "background worker started" (Info, low-noise — sent once). The launch path appends this to the outbound queue.

4. **Retry + relaunch.** Track `$WorkerLaunchAttempts` and `$WorkerLaunchBackoffUntil`. On failed launch, schedule a retry with exponential backoff (e.g. 5 s, 15 s, 45 s, then give up and notify). Before submitting any batch/interactive job to the worker, if `workerKernelHealthyQ` fails, mark it dead (`$DiagnosticsKernel = None`), schedule a relaunch, and run that job synchronously this time.

5. **Status command.** Add `"worker_kernel_status"` to `$ExecuteCommandProvider` and a `workspace/executeCommand` handler returning whether the worker is up, the binary used, attempt count, and last failure reason (for on-demand diagnosis).

6. **Worker provisioning verification.** After launch, the existing `ParallelEvaluate[Needs[...]]` + `DistributeDefinitions[...]` must be checked for `$Failed`; if provisioning fails, treat the worker as failed (close it, notify, retry). (Sets the foundation for Stage 3 loading the full paclet on the worker.)

### State (new globals, initialized in `StartServer`)

`$WorkerLaunchAttempts = 0`, `$WorkerLaunchBackoffUntil = None`, `$WorkerLastFailureReason = None`, `$WorkerStatusNotified = False`. Existing `$DiagnosticsKernel`, `$DiagnosticsKernelBin`, `$DiagnosticsKernelLaunchAfter` are reused.

### Non-goals for Stage 1

- No new work is offloaded (that is Stage 3). Stage 1 only makes the *existing* worker usage (diagnostics, hover) reliable and observable, and lays the launch foundation.
- No change to what runs synchronously vs async beyond fixing the launch.

### Error handling

Every worker interaction is guarded: launch failure → `$Failed` + notify + backoff retry; health-check failure pre-submit → run synchronously + schedule relaunch; provisioning failure → close + retry. The server is fully functional (synchronously) at all times.

### Testing

Unit (`.wlt`, no live kernel needed):
- `workerKernelHealthyQ` returns False on a `$TimedOut`/non-2 result and True on `2` (inject a stub kernel/result).
- `notifyWorkerStatus[False, reason]` produces a Warning `window/showMessage` with the reason; `[True,_]` produces a single Info (and not again once `$WorkerStatusNotified`).
- Retry/backoff state machine: simulated repeated failures schedule increasing backoff and stop after the cap; the `worker_kernel_status` handler reports the tracked fields.
- Binary resolution helper returns `$CommandLine[[1]]`.

Integration (opt-in, slow, guarded so it can be skipped):
- Actually launch a worker via the configured binary and assert `ParallelEvaluate[1+1]` round-trips to `2`; then close it.

Manual: in VSCode/Zed, confirm a worker-status message appears and (via the status command) that the worker is up; confirm diagnostics run without freezing the main thread.

Per project conventions: edited `.wl` sources are synced to `build/paclet/` (and the installed paclet for manual testing); `build/paclet` copies are committed with `git add -f`. The existing 7 `ServerInternals.wlt` failures are the regression baseline.

## Risks

- The `SubKernels`LocalKernels`` configuration API differs across WL versions; the implementation must resolve the correct constructor for the target version and keep the fallback chain.
- Launching a second full kernel costs ~5 s and memory; it is deferred until after `initialized` (existing behavior) and only once.
- If the user's environment genuinely cannot launch a subkernel (sandboxing, licensing), Stage 1 ensures they are *told* and the server still works synchronously — but Stage 3's benefits would then be unavailable, which is the correct, visible outcome.

## Profiling artifacts (kept in repo root, untracked)

`profile_semantic_tokens.wls`, `profile_edit_cycle.wls`, `spike_worker.wls` — reusable to re-measure after each stage.
