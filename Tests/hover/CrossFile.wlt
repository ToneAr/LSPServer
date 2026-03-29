Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];

(*
Cross-file hover test setup:
1. Initialize the provider file (defines crossFileFunc and crossFileConst)
2. Index it in the PacletIndex so cross-file resolution works
3. Initialize the consumer file (uses crossFileFunc and crossFileConst)
4. Hover on symbols in the consumer file
*)

(* Initialize and index the provider file *)
providerPath = FileNameJoin[{DirectoryName[$TestFileName], "CrossFileProviderTest.wl"}];
initFunction[providerPath];
providerUri = LocalObjects`PathToURI[providerPath];
providerText = ReadString[providerPath];
LSPServer`PacletIndex`UpdateFileIndex[providerUri, providerText];

(* Initialize the consumer file *)
consumerPath = FileNameJoin[{DirectoryName[$TestFileName], "CrossFileConsumerTest.wl"}];
initFunction[consumerPath];
consumerUri = LocalObjects`PathToURI[consumerPath];


(* Test 1: Hover over crossFileFunc in consumer file - should show usage and definition patterns *)
(* Line 8 (0-based): result = crossFileFunc[5]; -- crossFileFunc starts at character 9 *)
VerificationTest[
  Module[{result, value},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/hoverFencepost", "id" -> 1, 
        "params" -> <|"textDocument" -> <|"uri" -> consumerUri|>, "position" -> <|"line" -> 8, "character" -> 12|>|>
      |>];
    value = result[[1]]["result"]["contents"]["value"];
    (* Should contain usage message from provider *)
    StringContainsQ[value, "crossFileFunc[x] computes x squared."] &&
    (* Should contain function definition patterns *)
    StringContainsQ[value, "crossFileFunc"]
  ]
  ,
  True, 
  TestID -> "IDE-Test-CrossFile-Function-With-Usage"
]


(* Test 2: Hover over crossFileConst in consumer file *)
(* Line 10 (0-based): value = crossFileConst; -- crossFileConst starts at character 8 *)
VerificationTest[
  Module[{result, value},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/hoverFencepost", "id" -> 2, 
        "params" -> <|"textDocument" -> <|"uri" -> consumerUri|>, "position" -> <|"line" -> 10, "character" -> 10|>|>
      |>];
    value = result[[1]]["result"]["contents"]["value"];
    (* Should show something about this being a defined symbol *)
    StringQ[value] && StringLength[value] > 0
  ]
  ,
  True, 
  TestID -> "IDE-Test-CrossFile-Constant"
]


(* Test 3: Hover over crossFileFunc in provider file itself should still work via in-file resolution *)
(* Line 9 (0-based): crossFileFunc[x_] := x^2; -- at character 0 *)
VerificationTest[
  Module[{result, value},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/hoverFencepost", "id" -> 3, 
        "params" -> <|"textDocument" -> <|"uri" -> providerUri|>, "position" -> <|"line" -> 9, "character" -> 2|>|>
      |>];
    value = result[[1]]["result"]["contents"]["value"];
    (* Should still get usage info from in-file resolution *)
    StringContainsQ[value, "crossFileFunc[x] computes x squared."]
  ]
  ,
  True, 
  TestID -> "IDE-Test-CrossFile-ProviderFile-SameFile-Hover"
]


(* Test 4: Verify context is shown in cross-file hover *)
VerificationTest[
  Module[{result, value},
    result = LSPServer`handleContent[
      <|"method" -> "textDocument/hoverFencepost", "id" -> 4, 
        "params" -> <|"textDocument" -> <|"uri" -> consumerUri|>, "position" -> <|"line" -> 8, "character" -> 12|>|>
      |>];
    value = result[[1]]["result"]["contents"]["value"];
    (* Should show the context from the PacletIndex *)
    StringContainsQ[value, "CrossFileTest`"]
  ]
  ,
  True, 
  TestID -> "IDE-Test-CrossFile-Context-Displayed"
]
