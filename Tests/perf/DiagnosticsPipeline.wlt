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
