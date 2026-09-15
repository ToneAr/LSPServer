(* Load only the main package so feature dependencies are tested lazily. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "build", "paclet"}]]];
<<LSPServer`


VerificationTest[
  Module[{before, result, documentation},
    before = Length[DownValues[LSPServer`Hover`linearToMDSyntax]];
    result = LSPServer`handleContent[<|
      "method" -> "completionItem/resolve",
      "id" -> 17,
      "params" -> <|"label" -> "Sin"|>
    |>];
    documentation = Lookup[
      Lookup[First[result]["result"], "documentation", <||>],
      "value",
      None
    ];
    {
      before,
      Length[DownValues[LSPServer`Hover`linearToMDSyntax]] > 0,
      StringQ[documentation],
      StringQ[Developer`WriteRawJSONString[First[result]]]
    }
  ],
  {0, True, True, True},
  TestID -> "Completion-Lazily-Loads-Hover-Markdown-Dependency"
]
