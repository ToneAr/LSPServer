(* Load LSPServer from the repository's build/paclet so in-tree changes are picked up. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "..", "build", "paclet"}]]];
<< LSPServer`

completionResponse[uri_, id_, line_, character_] :=
  LSPServer`handleContent[<|
    "method" -> "textDocument/completionFencepost",
    "id" -> id,
    "params" -> <|
      "textDocument" -> <|"uri" -> uri|>,
      "position" -> <|"line" -> line, "character" -> character|>
    |>
  |>]

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
