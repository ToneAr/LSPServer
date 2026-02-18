Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "PackageTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "PackageTest.wl"}]];

(* SystemSymbol in a package *)
(* Note: Plot usage may vary between Wolfram kernel versions *)
VerificationTest[
  Module[{result, value},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/hoverFencepost", "id" -> 1, 
        "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 25, "character" -> 2|>|>
    |>];
    value = result[[1]]["result"]["contents"]["value"];
    StringStartsQ[value, "`` System` ``\n\nPlot["] &&
    StringContainsQ[value, "Web Documentation"]
  ]
  ,
  True, 
  TestID -> "IDE-Test-SystemSymbol-In-Package"
]


(* Function is defined in the Private context in a package*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 2, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 11, "character" -> 14|>|>
    |>
  ]
  , 
  {
    <|
      "jsonrpc" -> "2.0", "id" -> 2, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "Usage message of testFunction.\n\n**Definitions**\n\n```wolfram\ntestFunction[a_][b_]\n```"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-ContextDepth-1-In-Package"
]


(* 
  If a function is defined one level below the Private context level,  
  we are not going to detect that.
*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 3, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 16, "character" -> 18|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "Usage message of testFunction.\n\n**Definitions**\n\n```wolfram\ntestFunction[a_][b_]\n```"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-ContextDepth-2-In-Package"
]


(* Function with multiple usage in a package, defined with Set and SetDelayed *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 4, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 35, "character" -> 15|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 4, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "First usage message of multiUsageFunction.\n\nSecond usage message of multiUsageFunction.\n\n**Definitions**\n\n```wolfram\nmultiUsageFunction[x_]\n```"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-MultipleUsage-In-Package"
]


(* Function defined one tabspace away from the start of the line inside a package *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 5, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 31, "character" -> 5|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 5, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Definitions**\n\n```wolfram\nfoo[]\n```"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-Tab-Function-In-Package"
]


(* A function is defined with UpSetDelayed inside a package *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 6, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 23, "character" -> 2|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 6, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Definitions**\n\n```wolfram\nf[g[x_]]\n```"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-UpSetDelayed-Function-1-In-Package"
]


(* 
  If a function is defined with TagSetDelayed,
  hovering over the untagged function "f" will show "No function information." 
*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 7, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 27, "character" -> 5|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 7, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "No function information."
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-TagSetDelayed-Function-2-In-Package"
]
