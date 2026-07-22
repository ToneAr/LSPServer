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

(* Health check returns False for non-kernel sentinels without touching ParallelEvaluate. *)
VerificationTest[
  {LSPServer`Private`workerKernelHealthyQ[None],
   LSPServer`Private`workerKernelHealthyQ[$Failed]},
  {False, False},
  TestID -> "workerKernelHealthyQ-rejects-sentinels"
]

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

(* A failed launch records the reason, notifies the user, and schedules a retry. *)
VerificationTest[
  Module[{notified, scheduled},
    LSPServer`$ContentQueue = {};
    LSPServer`$WorkerStatusNotified = False;
    LSPServer`$WorkerLaunchAttempts = 0;
    LSPServer`$DiagnosticsKernel = None;
    LSPServer`$DiagnosticsKernelLaunchAfter = None;
    LSPServer`$WorkerLastFailureReason = None;
    (* Force the launch to fail via the seam. Do NOT Block System`LaunchKernels:
       it carries an autoload stub, and Block-ing it across the package load
       discards the real definitions for the rest of the session. *)
    Block[{LSPServer`Private`workerLaunchKernels = ($Failed &)},
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

(* The relaunch trigger must fire for a failed worker ($Failed), not only None —
   otherwise the backoff retry scheduled by a failed launch never runs. *)
VerificationTest[
  Module[{due},
    LSPServer`$DiagnosticsKernel = $Failed;
    LSPServer`$DiagnosticsKernelLaunchAfter = AbsoluteTime[] - 1;
    due = {LSPServer`Private`workerRelaunchDueQ[]};
    LSPServer`$DiagnosticsKernel = None;
    AppendTo[due, LSPServer`Private`workerRelaunchDueQ[]];
    LSPServer`$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 1000;
    AppendTo[due, LSPServer`Private`workerRelaunchDueQ[]];
    LSPServer`$DiagnosticsKernelLaunchAfter = None;
    AppendTo[due, LSPServer`Private`workerRelaunchDueQ[]];
    due
  ],
  {True, True, False, False},
  TestID -> "workerRelaunchDueQ-fires-for-failed-and-none"
]

(* OPT-IN integration test: actually launch a subkernel and round-trip. Set the
   environment variable LSP_WORKER_INTEGRATION=1 to run it; otherwise it is a
   trivial pass so the suite stays fast/hermetic. *)
VerificationTest[
  If[Environment["LSP_WORKER_INTEGRATION"] === "1",
    Module[{ks, ok},
      (* Judge by result shape, not Check: LaunchKernels can emit benign
         messages on success, which would make Check mis-report failure. *)
      ks = Quiet[LaunchKernels[1]];
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

(* Health check must reject non-KernelObject values instantly. Calling
   ParallelEvaluate on a bogus value silently launches default kernels (~5 s
   main-thread stall) and aborting that launch corrupts Parallel` state. *)
VerificationTest[
  Module[{t0, ok},
    t0 = AbsoluteTime[];
    ok = LSPServer`Private`workerKernelHealthyQ["deadkernel"];
    {ok, AbsoluteTime[] - t0 < 1.0}
  ],
  {False, True},
  TestID -> "workerKernelHealthyQ-rejects-non-kernels-fast"
]


VerificationTest[
  Module[{uri, params, content, result},
    uri = "file:///tmp/highlight_worker_waiting_reindex.wl";
    params = <|"textDocument" -> <|"uri" -> uri|>|>;
    content = <|"method" -> "textDocument/semanticTokens/full", "id" -> 42,
      "params" -> params|>;
    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "x = 1\n",
        "LastChange" -> 1,
        "IndexUpdatePending" -> True,
        "ScheduledJobs" -> {Function[{entry}, {{}, False}]}
      |>|>,
      LSPServer`$PreExpandContentQueue = {content},
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <||>,
      LSPServer`$HighlightKernel = Unique["HighlightKernel"],
      LSPServer`$HighlightTask = None
    },
      result = LSPServer`expandContent[content, {1}];
      Lookup[result, "method", Missing["NotFound"]]
    ]
  ],
  {"textDocument/semanticTokens/fullFencepost"},
  TestID -> "SemanticTokens-Expand-Uses-HighlightWorker-Without-Foreground-Parse"
]


VerificationTest[
  Module[{uri, params, text, cst, agg, ast, result},
    uri = "file:///tmp/highlight_worker_dispatch_semantic.wl";
    params = <|"textDocument" -> <|"uri" -> uri|>|>;
    text = "x = 1\n";
    cst = CodeParser`CodeConcreteParse[text, "FileFormat" -> "Package"];
    cst[[1]] = File;
    agg = CodeParser`Abstract`Aggregate[cst];
    ast = CodeParser`Abstract`Abstract[agg];
    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> text,
        "LastChange" -> 2,
        "ScheduledJobs" -> {},
        "CST" -> cst,
        "Agg" -> agg,
        "AST" -> ast
      |>|>,
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      LSPServer`$SemanticTokens = True,
      LSPServer`$HighlightKernel = Unique["HighlightKernel"],
      LSPServer`$HighlightTask = None,
      ParallelSubmit = Function[{kernels, expr}, "fake-highlight-task", HoldAll]
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/semanticTokens/fullFencepost",
        "id" -> 42,
        "params" -> params
      |>];
      {result, LSPServer`$HighlightTask, LSPServer`$HighlightTaskKind,
        LSPServer`$HighlightTaskURI, LSPServer`$HighlightTaskID}
    ]
  ],
  {{}, "fake-highlight-task", "semantic-tokens",
    "file:///tmp/highlight_worker_dispatch_semantic.wl", 42},
  TestID -> "SemanticTokens-Fencepost-Dispatches-To-HighlightWorker"
]


VerificationTest[
  Module[{uri, params, content, snapshot, result},
    uri = "file:///tmp/highlight_worker_parses_text_snapshot.wl";
    params = <|"textDocument" -> <|"uri" -> uri|>|>;
    content = <|"method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> 42, "params" -> params|>;
    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "Module[{x}, x]\n",
        "LastChange" -> 3,
        "IndexUpdatePending" -> True,
        "ScheduledJobs" -> {}
      |>|>,
      LSPServer`$SemanticTokens = True
    },
      snapshot = LSPServer`Private`buildHighlightWorkerSnapshot[uri]
    ];
    result = LSPServer`Private`runHighlightWorker[
      "semantic-tokens",
      content,
      snapshot
    ];
    {
      AssociationQ[result],
      MatchQ[Lookup[result, "Response", {}],
        {KeyValuePattern[{"id" -> 42, "result" -> KeyValuePattern["data" -> _List]}]}],
      KeyExistsQ[Lookup[result, "Entry", <||>], "CST"],
      KeyExistsQ[Lookup[result, "Entry", <||>], "AST"],
      KeyExistsQ[Lookup[result, "Entry", <||>], "SemanticTokens"]
    }
  ],
  {True, True, False, False, True},
  TestID -> "HighlightWorker-Can-Parse-Text-Only-SemanticSnapshot"
]


VerificationTest[
  Module[{uri, snapshot, entry},
    uri = "file:///tmp/highlight_worker_snapshot_strips_heavy.wl";
    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "x = 1\n",
        "LastChange" -> 3,
        "CST" -> "large-cst",
        "Agg" -> "large-agg",
        "AST" -> "large-ast",
        "PreviousAST" -> "large-previous-ast",
        "SemanticTokens" -> {1, 2, 3},
        "UserSymbols" -> {"x"}
      |>|>
    },
      snapshot = LSPServer`Private`buildHighlightWorkerSnapshot[uri];
      entry = Lookup[snapshot, "OpenFileEntry", <||>];
      {
        KeyExistsQ[entry, "Text"],
        KeyExistsQ[entry, "SemanticTokens"],
        KeyExistsQ[entry, "UserSymbols"],
        KeyExistsQ[entry, "CST"],
        KeyExistsQ[entry, "Agg"],
        KeyExistsQ[entry, "AST"],
        KeyExistsQ[entry, "PreviousAST"]
      }
    ]
  ],
  {True, True, True, False, False, False, False},
  TestID -> "HighlightWorker-Snapshot-Strips-Heavy-Parse-Artifacts"
]


VerificationTest[
  Module[{uri, content, snapshot, result},
    uri = "file:///tmp/highlight_worker_message_result.wl";
    content = <|"method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> 42, "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>;
    snapshot = <|"OpenFileEntry" -> <|"Text" -> "x = 1\n", "LastChange" -> 7|>|>;
    Block[{
      LSPServer`handleContent = Function[{c},
        Message[Power::infy];
        {<|"jsonrpc" -> "2.0", "id" -> Lookup[c, "id", 42],
          "result" -> <|"data" -> {1, 2, 3}|>|>}
      ]
    },
      result = LSPServer`Private`runHighlightWorker[
        "semantic-tokens",
        content,
        snapshot
      ];
      {
        AssociationQ[result],
        Lookup[result, "Response", {}],
        Lookup[Lookup[result, "Entry", <||>], "SemanticTokens", Missing["NotFound"]]
      }
    ]
  ],
  {
    True,
    {<|"jsonrpc" -> "2.0", "id" -> 42,
      "result" -> <|"data" -> {1, 2, 3}|>|>},
    Missing["NotFound"]
  },
  TestID -> "HighlightWorker-Messages-Do-Not-Fail-Result"
]


VerificationTest[
  Module[{uri, result, response, queued = None},
    uri = "file:///tmp/highlight_worker_publish_semantic.wl";
    result = <|
      "Kind" -> "semantic-tokens",
      "URI" -> uri,
      "ID" -> 42,
      "LastChange" -> 4,
      "WasStale" -> False,
      "Response" -> {<|"jsonrpc" -> "2.0", "id" -> 42,
        "result" -> <|"data" -> {0, 0, 1, 2, 0}|>|>},
      "Entry" -> <|
        "SemanticTokens" -> {0, 0, 1, 2, 0},
        "SemanticTokensIncomplete" -> True,
        "CST" -> "cst",
        "AST" -> "ast"
      |>
    |>;
    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> "x\n",
        "LastChange" -> 4, "ScheduledJobs" -> {}|>|>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      LSPServer`SemanticTokens`Private`queueSemanticTokenScopingFollowup =
        Function[{u}, queued = u]
    },
      response = LSPServer`handleContent[<|
        "method" -> "textDocument/publishSemanticTokensWorkerResult",
        "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>,
        "result" -> result
      |>];
      {
        response,
        Lookup[LSPServer`$OpenFilesMap[uri], "SemanticTokens", None],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "AST"],
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["Gone"]],
        queued
      }
    ]
  ],
  {
    {<|"jsonrpc" -> "2.0", "id" -> 42,
      "result" -> <|"data" -> {0, 0, 1, 2, 0}|>|>},
    {0, 0, 1, 2, 0},
    False,
    Missing["Gone"],
    "file:///tmp/highlight_worker_publish_semantic.wl"
  },
  TestID -> "PublishSemanticTokensWorkerResult-Merges-And-Queues-Scoping"
]


VerificationTest[
  Module[{uri, queuedContent, activeContent, freshContent},
    uri = "file:///tmp/highlight_worker_scoping_dedupe.wl";
    queuedContent = <|
      "method" -> "textDocument/runScopingData",
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>;
    activeContent = Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$HighlightPendingContents = {},
      LSPServer`$HighlightTaskKind = "scoping-data",
      LSPServer`$HighlightTaskURI = uri
    },
      LSPServer`SemanticTokens`Private`queueSemanticTokenScopingFollowup[uri];
      LSPServer`$ContentQueue
    ];
    queuedContent = Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$HighlightPendingContents = {queuedContent},
      LSPServer`$HighlightTaskKind = None,
      LSPServer`$HighlightTaskURI = None
    },
      LSPServer`SemanticTokens`Private`queueSemanticTokenScopingFollowup[uri];
      LSPServer`$ContentQueue
    ];
    freshContent = Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$HighlightPendingContents = {},
      LSPServer`$HighlightTaskKind = None,
      LSPServer`$HighlightTaskURI = None
    },
      LSPServer`SemanticTokens`Private`queueSemanticTokenScopingFollowup[uri];
      Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]]
    ];
    {activeContent, queuedContent, freshContent}
  ],
  {{}, {}, {"textDocument/runScopingData"}},
  TestID -> "SemanticTokens-ScopingFollowup-Dedupes-HighlightWorker"
]


VerificationTest[
  Module[{uri, result, response},
    uri = "file:///tmp/highlight_worker_stale_semantic_cached.wl";
    result = <|
      "Kind" -> "semantic-tokens",
      "URI" -> uri,
      "ID" -> 42,
      "LastChange" -> 3,
      "WasStale" -> True,
      "Response" -> {<|"jsonrpc" -> "2.0", "id" -> 42,
        "result" -> <|"data" -> {0, 0, 1, 2, 0}|>|>},
      "Entry" -> <|"SemanticTokens" -> {0, 0, 1, 2, 0}|>
    |>;
    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> "x\n",
        "LastChange" -> 4, "SemanticTokens" -> {9, 9, 9}|>|>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>
    },
      response = LSPServer`handleContent[<|
        "method" -> "textDocument/publishSemanticTokensWorkerResult",
        "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>,
        "result" -> result
      |>];
      {
        response,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["Gone"]]
      }
    ]
  ],
  {
    {<|"jsonrpc" -> "2.0", "id" -> 42,
      "result" -> <|"data" -> {9, 9, 9}|>|>},
    Missing["Gone"]
  },
  TestID -> "PublishSemanticTokensWorkerResult-Stale-Uses-Cached-Tokens"
]


VerificationTest[
  Module[{uri, text, response},
    uri = "file:///tmp/highlight_worker_failed_semantic_foreground.wl";
    text = "x = 1\n";
    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> text,
        "LastChange" -> 5,
        "IndexUpdatePending" -> True,
        "ScheduledJobs" -> {}
      |>|>,
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      LSPServer`$SemanticTokens = True,
      LSPServer`$HighlightKernel = Unique["HighlightKernel"],
      LSPServer`$HighlightTask = None
    },
      response = LSPServer`handleContent[<|
        "method" -> "textDocument/publishSemanticTokensWorkerResult",
        "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>,
        "result" -> $Failed
      |>];
      {
        MatchQ[response,
          {KeyValuePattern[{"id" -> 42,
            "result" -> KeyValuePattern["data" -> _List]}]}],
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["Gone"]],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokens"],
        LSPServer`$HighlightTask
      }
    ]
  ],
  {True, Missing["Gone"], True, None},
  TestID -> "PublishSemanticTokensWorkerResult-Failed-Falls-Back-Foreground"
]


VerificationTest[
  Module[{uri, content, recovered},
    uri = "file:///tmp/highlight_worker_timeout_requeues.wl";
    content = <|
      "method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> 42,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>;
    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$HighlightPendingContents = {},
      LSPServer`$HighlightTask = "hung-task",
      LSPServer`$HighlightTaskKind = "semantic-tokens",
      LSPServer`$HighlightTaskURI = uri,
      LSPServer`$HighlightTaskID = 42,
      LSPServer`$HighlightTaskContent = content,
      LSPServer`$HighlightTaskStartTime = AbsoluteTime[] - 20,
      LSPServer`$HighlightTaskTimeout = 0.01,
      LSPServer`$HighlightKernel = Unique["HighlightKernel"],
      LSPServer`$HighlightKernelLaunchAfter = None,
      LSPServer`$HighlightWorkerLaunchAttempts = 1
    },
      recovered = LSPServer`Private`recoverTimedOutHighlightTask[];
      {
        recovered,
        LSPServer`$HighlightTask,
        LSPServer`$HighlightKernel,
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        NumberQ[LSPServer`$HighlightKernelLaunchAfter]
      }
    ]
  ],
  {True, None, None, {"textDocument/semanticTokens/fullFencepost"}, True},
  TestID -> "HighlightWorker-Timeout-Requeues-Active-Request"
]


VerificationTest[
  Module[{uri, params, result},
    uri = "file:///tmp/highlight_worker_document_color.wl";
    params = <|"textDocument" -> <|"uri" -> uri|>|>;
    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "RGBColor[1, 0, 0]\n",
        "LastChange" -> 5,
        "ScheduledJobs" -> {}
      |>|>,
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$HighlightKernel = Unique["HighlightKernel"],
      LSPServer`$HighlightTask = None,
      ParallelSubmit = Function[{kernels, expr}, "fake-color-task", HoldAll]
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/documentColorFencepost",
        "id" -> 77,
        "params" -> params
      |>];
      {result, LSPServer`$HighlightTask, LSPServer`$HighlightTaskKind,
        LSPServer`$HighlightTaskID}
    ]
  ],
  {{}, "fake-color-task", "document-color", 77},
  TestID -> "DocumentColor-Fencepost-Dispatches-To-HighlightWorker"
]

(* ── Diagnostics-worker snapshot ships only small fields ── *)

VerificationTest[
  Module[{uri, snapshot, entry},
    uri = "file:///tmp/diagnostics_worker_snapshot_strips_heavy.wl";
    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "x = 1\n",
        "LastChange" -> 3,
        "CST" -> "large-cst",
        "Agg" -> "large-agg",
        "AST" -> "large-ast",
        "SemanticTokens" -> {1, 2, 3},
        "ScopingData" -> {},
        "UserSymbols" -> {"x"}
      |>|>
    },
      snapshot = LSPServer`buildWorkerSnapshot[uri];
      entry = Lookup[snapshot, "OpenFileEntry", <||>];
      {
        KeyExistsQ[entry, "Text"],
        KeyExistsQ[entry, "ScopingData"],
        KeyExistsQ[entry, "UserSymbols"],
        KeyExistsQ[entry, "CST"],
        KeyExistsQ[entry, "Agg"],
        KeyExistsQ[entry, "AST"],
        KeyExistsQ[entry, "SemanticTokens"]
      }
    ]
  ],
  {True, True, True, False, False, False, False},
  TestID -> "DiagnosticsWorker-Snapshot-Strips-Heavy-Parse-Artifacts"
]

VerificationTest[
  Module[{uri, snapshot, result},
    uri = "file:///tmp/diag_worker_text_only.wl";
    snapshot = <|
      "PacletIndex" -> <|
        "Symbols" -> <||>, "Files" -> <||>, "Contexts" -> <||>,
        "Dependencies" -> {}, "ContextAliases" -> <||>
      |>,
      "BuiltinPatterns" -> <||>,
      "WorkspaceRootPath" -> None,
      "ConfidenceLevel" -> 0.5,
      "OpenFileEntry" -> <|"Text" -> "f[x_] := x + 1\n", "LastChange" -> 9|>,
      "IndexingWasActive" -> False,
      "PendingIndexFiles" -> {},
      "PendingExternalDepFiles" -> {}
    |>;
    result = LSPServer`Diagnostics`Private`runWorkspaceDiagnosticsWorker[uri, snapshot];
    {
      AssociationQ[result],
      ListQ[Lookup[result, "WorkspaceLints", Null]],
      Lookup[result, "LastChange", None]
    }
  ],
  {True, True, 9},
  TestID -> "DiagnosticsWorker-Can-Parse-Text-Only-Snapshot"
]
