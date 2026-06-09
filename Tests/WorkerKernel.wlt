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
