Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "OptionsPatternValidationTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "OptionsPatternValidationTest.wl"}]];
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "OptionsPatternValidationTest.wl"}]]];

LSPServer`handleContent[
  <|"method" -> "textDocument/runWorkspaceDiagnostics",
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
  |>
];

VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentArityMismatch"] &&
        #["range"]["start"]["line"] === 7 &]
  ],
  {},
  TestID -> "IDE-Test-OptionsPattern-NoOptions-NoArityWarning"
]


VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "DocCommentArityMismatch"] &&
        #["range"]["start"]["line"] === 9 &]
  ],
  {},
  TestID -> "IDE-Test-OptionsPattern-KnownOption-NoArityWarning"
]


VerificationTest[
  Module[{result, diags},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>
    ];
    diags = result[[1, "params", "diagnostics"]];
    Length[Select[diags,
      StringStartsQ[Lookup[#, "code", ""], "UnknownOption"] &&
        #["range"]["start"]["line"] === 11 &]] > 0
  ],
  True,
  TestID -> "IDE-Test-OptionsPattern-UnknownOptionWarning"
]
