(* Load LSPServer from the repository's build/paclet so in-tree changes are picked up. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "..", "build", "paclet"}]]];
<<LSPServer`

(* Set the confidence level so diagnostic tests work without calling StartServer[].
   Matches $DefaultConfidenceLevel = 0.50 from LSPServer.wl. *)
$ConfidenceLevel = 0.50;

initFunction[filePath_]:=
Module[{uri, fileText},
  uri = LocalObjects`PathToURI[filePath];
  fileText = ReadString[filePath];

  LSPServer`handleContent[<|
    "method"-> #,
    "params"-> <|
      "textDocument"-> <|
        "uri" -> uri,
        "languageId"->"wolfram",
        "version"-> 1,
        "text"-> fileText
      |>
    |>
  |>]& /@
  {
    "textDocument/didOpenFencepost",
    "textDocument/concreteParse",
    "textDocument/concreteTabsParse",
    "textDocument/aggregateParse",
    "textDocument/aggregateTabsParse",
    "textDocument/abstractParse"
  }
]
