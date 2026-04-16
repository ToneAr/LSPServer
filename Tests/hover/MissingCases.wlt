Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "MissingCasesTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "MissingCasesTest.wl"}]];

(* Function is defined with no usage message *)
VerificationTest[
  Module[{result, value},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/hoverFencepost",
        "id" -> 1,
        "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 5, "character" -> 11|>|>
      |>
    ];
    value = result[[1]]["result"]["contents"]["value"];
    value == "**Definitions**\n\n```wolfram\nnoFunctionUsage[x_Integer]\n```\n\n---\n**Doc Comments**\n\n**Parameters:** `_Integer`  \n**Returns:** `_?NumericQ`"
  ],
  True,
TestID -> "IDE-Test-NoUsage"
]


(* Function is not defined but only has usage message *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 2,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 8, "character" -> 5|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 2,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "Test function with only usage."
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-OnlyUsage"]


(* Function is not defined and has usage message *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 3,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 11, "character" -> 8|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 3,
      "result" -> <|"contents" -> <|
        "kind" -> "markdown",
        "value" -> "No function information."
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-NoFunction-Information"]

