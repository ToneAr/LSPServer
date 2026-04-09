(* Load LSPServer from the repository's build/paclet so in-tree changes are picked up. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "..", "build", "paclet"}]]];
<<LSPServer`

(* Set the confidence level so tests work without calling StartServer[]. *)
$ConfidenceLevel = 0.50;
