Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "SystemSymbolTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "SystemSymbolTest.wl"}]];

(* SystemSymbol containing usage message with linear syntax *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 1, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 0, "character" -> 1|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "`System``\n\nSin[*z*] gives the sine of *z*. \n\n_[Sin: Web Documentation](https://reference.wolfram.com/language/ref/Sin.html)_"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-SystemSymbol-Linear-Syntax"
]


(* SystemSymbol (cos) is located one tabspace away from start of line in SystemSymbolTest.wl *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 2, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 2, "character" -> 6|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 2, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "`System``\n\nCos[*z*] gives the cosine of *z*. \n\n_[Cos: Web Documentation](https://reference.wolfram.com/language/ref/Cos.html)_"
        |>
      |>
    |>
  },
TestID -> "IDE-Test-SystemSymbol-withTab"]


(* SystemSymbol (ExternalEvaluate) with multi-line usage message *)
(* Note: ExternalEvaluate usage may vary between Wolfram kernel versions *)
VerificationTest[
  Module[{result, value},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/hoverFencepost", 
        "id" -> 3, 
        "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 4, "character" -> 11|>|>
    |>];
    value = result[[1]]["result"]["contents"]["value"];
    (* Verify the context line and key parts of the usage are present *)
    StringStartsQ[value, "`System``\n\nExternalEvaluate"] &&
    StringContainsQ[value, "EXPERIMENTAL"] &&
    StringContainsQ[value, "Web Documentation"]
  ]
  ,
  True,
  TestID -> "IDE-Test-SystemSymbol-Multiline-Usage"
]
  

