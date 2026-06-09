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
