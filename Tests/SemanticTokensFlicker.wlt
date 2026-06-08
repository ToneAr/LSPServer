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
