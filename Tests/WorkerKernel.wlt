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
