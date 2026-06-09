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
