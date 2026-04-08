Needs["MUnit`"];
report1 = TestReport["Tests/hover/DocCommentDiag.wlt"];
Print["DocCommentDiag - Passed: ", report1["TestsSucceededCount"], " Failed: ", report1["TestsFailedCount"]];
Do[r = report1["TestResults"][[i]]; If[r["Outcome"] =!= "Success", Print["FAIL: ", r["TestID"]]; Print["  Exp: ", r["ExpectedOutput"]]; Print["  Got: ", r["ActualOutput"]]], {i, Length[report1["TestResults"]]}];

report2 = TestReport["Tests/hover/BranchNarrowing.wlt"];
Print["BranchNarrowing - Passed: ", report2["TestsSucceededCount"], " Failed: ", report2["TestsFailedCount"]];
Do[r = report2["TestResults"][[i]]; If[r["Outcome"] =!= "Success", Print["FAIL: ", r["TestID"]]; Print["  Exp: ", r["ExpectedOutput"]]; Print["  Got: ", r["ActualOutput"]]], {i, Length[report2["TestResults"]]}]
