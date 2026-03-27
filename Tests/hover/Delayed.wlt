Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "DelayedTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "DelayedTest.wl"}]];

(* Function with multiple definition *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 1, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 3, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, 
    "result" -> <|"contents" -> <|
      "kind" -> "markdown", 
      "value" -> "f[x] has multiple definition.\n\n**Definitions**\n\n```wolfram\nf[x_]\nf[x_String]\nf[x_String] /; x\nf[x_Number]\n```"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-SetDelayed-Multiple-Definition"
]


(* Hovering over q on p[q[x_]] ^:= pq[x] *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 2, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 10, "character" -> 2|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 2, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Definitions**\n\n```wolfram\np[q[x_]]\n```"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-UpSetDelayed-Definition-InnerFunction"
]


(* Hovering over p where p[q[x_]] ^:= pq[x] *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 3, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 12, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, "result" -> <|"contents" -> <|"kind" -> "markdown", "value" -> "No function information."|>|>|>
  },
  TestID -> "IDE-Test-UpSetDelayed-Definition-Innerfunction"
]


(* Hovering over g where g/:f2[g[x_]]:=f2g[x] *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 4, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 16, "character" -> 0|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 4, 
      "result" -> <|"contents" -> 
          <|"kind" -> "markdown", 
            "value" -> "**Definitions**\n\n```wolfram\nf2[g[x_]]\n```"
          |>
      |>
    |>
  },
TestID -> "IDE-Test-TagSetDelayed-Definition-TaggedFunction"
]


(* Hovering over f2 where g/:f2[g[x_]]:=f2g[x] *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 5, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 18, "character" -> 1|>|>  
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 5, "result" -> <|"contents" -> <|"kind" -> "markdown", "value" -> "No function information."|>|>|>
  },
  TestID -> "IDE-Test-TagSetDelayed-Definition-UntaggedFunction"
]


(* Function defined with arg with a specific type *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 6, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 25, "character" -> 3|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 6, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Definitions**\n\n```wolfram\naddTwo[a_Integer, b_]\n```"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-SetDelayed-ConstrainedArgs"
]
