Needs["MUnit`"];
r = TestReport["Tests/hover/DocComment.wlt"];
Print["Passed: ", r["TestsSucceededCount"]];
Print["Failed: ", r["TestsFailedCount"]];
Scan[
  Function[t,
    If[t["Outcome"] =!= "Success",
      Print["FAIL: ", t["TestID"]];
      Print["  Expected: ", t["ExpectedOutput"]];
      Print["  Actual:   ", t["ActualOutput"]]
    ]
  ],
  r["TestResults"]
]
