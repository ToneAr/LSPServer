PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "..", "build", "paclet"}]]];
Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];

LSPServer`Completion`Private`$WorkspaceAssocKeyCache = <||>;
LSPServer`Completion`Private`$ClosedFileAssocKeyCache = <||>;

providerPath = FileNameJoin[{DirectoryName[$TestFileName], "CrossFileAssociationProvider.wl"}];
providerUri = LocalObjects`PathToURI[providerPath];
providerText = ReadString[providerPath];
LSPServer`PacletIndex`UpdateFileIndex[providerUri, providerText];

consumerPath = FileNameJoin[{DirectoryName[$TestFileName], "CrossFileAssociationConsumer.wl"}];
consumerText = ReadString[consumerPath];
consumerLines = StringSplit[consumerText, {"\r\n", "\n", "\r"}, All];
initFunction[consumerPath];
consumerUri = LocalObjects`PathToURI[consumerPath];

completionLabels[line0_Integer, character_Integer] :=
  Lookup[
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
    ],
    "label",
    {}
  ];


VerificationTest[
  KeyExistsQ[LSPServer`$OpenFilesMap, providerUri],
  False,
  TestID -> "IDE-Test-CrossFile-Completion-Provider-Stays-Closed"
]


VerificationTest[
  Sort[completionLabels[0, StringLength[consumerLines[[1]]]]],
  {"\"settings\"", "\"user\""},
  TestID -> "IDE-Test-CrossFile-Association-Key-Completion"
]


VerificationTest[
  completionLabels[1, StringLength[consumerLines[[2]]]],
  {"1", "2"},
  TestID -> "IDE-Test-CrossFile-List-Index-Completion"
]


VerificationTest[
  Sort[completionLabels[2, StringLength[consumerLines[[3]]]]],
  {"\"id\"", "\"name\""},
  TestID -> "IDE-Test-CrossFile-List-Element-Key-Completion"
]
