PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "..", "build", "paclet"}]]];
Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];

providerPath = FileNameJoin[{DirectoryName[$TestFileName], "OptionsProvider.wl"}];
providerUri = LocalObjects`PathToURI[providerPath];
providerText = ReadString[providerPath];
LSPServer`PacletIndex`UpdateFileIndex[providerUri, providerText];

consumerPath = FileNameJoin[{DirectoryName[$TestFileName], "OptionsConsumer.wl"}];
consumerText = ReadString[consumerPath];
consumerLines = StringSplit[consumerText, {"\r\n", "\n", "\r"}, All];
initFunction[consumerPath];
consumerUri = LocalObjects`PathToURI[consumerPath];

completionItems[line0_Integer, character_Integer] :=
  Lookup[
    Lookup[
      First[
        Flatten[
          LSPServer`handleContent /@
            LSPServer`expandContents[{
              <|
                "method" -> "textDocument/completion",
                "id" -> line0 * 100 + character,
                "params" -> <|
                  "textDocument" -> <|"uri" -> consumerUri|>,
                  "position" -> <|"line" -> line0, "character" -> character|>
                |>
              |>
            }]
        ]
      ],
      "result",
      <||>
    ],
    "items",
    {}
  ];

completionLabels[line0_Integer, character_Integer] :=
  Lookup[completionItems[line0, character], "label", {}];


VerificationTest[
  SubsetQ[
    Sort[completionLabels[0, StringLength[consumerLines[[1]]]]],
    {"ProjectChoice", "ProjectFlag"}
  ],
  True,
  TestID -> "IDE-Test-Project-Option-Completion"
]


VerificationTest[
  SubsetQ[
    Sort[completionLabels[1, StringLength[consumerLines[[2]]]]],
    {"ProjectChoice", "ProjectFlag"}
  ],
  True,
  TestID -> "IDE-Test-Project-Option-Completion-Inherited-OptionsPattern"
]


VerificationTest[
  Module[{items = completionItems[2, StringLength[consumerLines[[3]]]], plotRangeItems},
    plotRangeItems = Select[items, Lookup[#, "label", ""] === "PlotRange" &];
    AnyTrue[plotRangeItems,
      Lookup[#, "detail", ""] === "Option" &&
        Lookup[#, "insertText", ""] === "PlotRange -> " &]
  ],
  True,
  TestID -> "IDE-Test-Builtin-Option-Completion-UsesOptionInsertText"
]
