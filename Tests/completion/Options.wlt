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
  SubsetQ[
    Sort[completionLabels[0, StringLength[consumerLines[[1]]]]],
    {"ProjectChoice", "ProjectFlag"}
  ],
  True,
  TestID -> "IDE-Test-Project-Option-Completion"
]
