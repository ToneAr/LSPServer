PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "..", "build", "paclet"}]]];
Needs["LSPServer`"];
Needs["MUnit`"];

$ConfidenceLevel = 0.50;

(* Verify runDiagnostics now produces exactly 1 sub-message (the fast tier) *)
VerificationTest[
  Module[{fakeContent, result},
    fakeContent = <|
      "method" -> "textDocument/runDiagnostics",
      "params" -> <|"textDocument" -> <|"uri" -> "file:///test.wl"|>|>
    |>;
    (* Set queue to {fakeContent} so queue[[2;;]] returns {} cleanly (no Part::take) *)
    LSPServer`$PreExpandContentQueue = {fakeContent};
    result = LSPServer`expandContent[fakeContent, {1}];
    Map[#["method"]&, result]
  ],
  {"textDocument/runFastDiagnostics"},
  TestID -> "RunDiagnosticsExpandsToFastTierOnly"
]

(* Fast tier produces an immediate publishDiagnostics notification *)
VerificationTest[
  Module[{fakeURI, result},
    fakeURI = "file:///test.wl";
    $OpenFilesMap = <||>;
    $OpenFilesMap[fakeURI] = <|
      "Text" -> "x = 1 + 1",
      "LastChange" -> Now
    |>;
    $ContentQueue = {};
    $DiagnosticsKernel = $Failed;  (* disable slow tier for this test *)
    result = handleContent[<|
      "method" -> "textDocument/runFastDiagnostics",
      "params" -> <|"textDocument" -> <|"uri" -> fakeURI|>|>
    |>];
    AnyTrue[result, MatchQ[#, KeyValuePattern["method" -> "textDocument/publishDiagnostics"]]&]
  ],
  True,
  TestID -> "FastTierPublishesImmediately"
]

VerificationTest[
  Module[{fakeURI},
    fakeURI = "file:///test.wl";
    $DiagnosticsTask    = "fake-task-sentinel";
    $DiagnosticsTaskURI = fakeURI;
    $DiagnosticsKernel  = $Failed;
    $ContentQueue = {};
    $OpenFilesMap = <|fakeURI -> <|"PreviousAST" -> Null, "PreviousUserSymbols" -> {}|>|>;
    $didChangeScheduledJobs = {};
    handleContent[<|
      "method" -> "textDocument/didChangeFencepost",
      "params" -> <|
        "textDocument" -> <|"uri" -> fakeURI|>,
        "contentChanges" -> {<|"text" -> "x = 2"|>}
      |>
    |>];
    $DiagnosticsTask === None
  ],
  True,
  TestID -> "DidChangeFencepostCancelsStaleTask"
]

VerificationTest[
  Module[{snap},
    $PacletIndex = <|"Symbols" -> <||>, "Files" -> <||>,
      "Contexts" -> <||>, "Dependencies" -> {}, "ContextAliases" -> <||>|>;
    $BuiltinPatterns = <||>;
    $WorkspaceRootPath = "/tmp/testws";
    snap = buildWorkerSnapshot["file:///test.wl"];
    KeyExistsQ[snap, "PacletIndex"] &&
    KeyExistsQ[snap, "BuiltinPatterns"] &&
    KeyExistsQ[snap, "WorkspaceRootPath"] &&
    KeyExistsQ[snap, "ConfidenceLevel"]
  ],
  True,
  TestID -> "BuildWorkerSnapshotHasRequiredKeys"
]
