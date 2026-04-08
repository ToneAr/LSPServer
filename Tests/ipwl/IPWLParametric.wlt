Get[FileNameJoin[{DirectoryName[$TestFileName], "..", "hover", "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "IPWLParametricTest.ipwl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "IPWLParametricTest.ipwl"}]];
LSPServer`PacletIndex`UpdateFileIndex[uri, ReadString[FileNameJoin[{DirectoryName[$TestFileName], "IPWLParametricTest.ipwl"}]]];

(* Test 1: DeclaredType stored as Blank[ParametricRef["in"]] for named parametric *)
VerificationTest[
  Lookup[
    SelectFirst[
      Lookup[
        Lookup[LSPServer`PacletIndex`$PacletIndex["Symbols"], "g", <||>],
        "Definitions", {}
      ],
      #["kind"] === "declaration" &,
      <||>
    ],
    "DeclaredType",
    Missing["NotFound"]
  ],
  Blank[LSPServer`TypeWL`ParametricRef["in"]],
  TestID -> "IPWL-Parametric-StoredAsParametricRef"
]

(* Test 2: DeclaredType stored as Blank[ParametricRef[1]] for positional parametric *)
VerificationTest[
  Lookup[
    SelectFirst[
      Lookup[
        Lookup[LSPServer`PacletIndex`$PacletIndex["Symbols"], "h", <||>],
        "Definitions", {}
      ],
      #["kind"] === "declaration" &,
      <||>
    ],
    "DeclaredType",
    Missing["NotFound"]
  ],
  Blank[LSPServer`TypeWL`ParametricRef[{1, 1}]],
  TestID -> "IPWL-Parametric-StoredAsPositionalRef"
]

(* Test 3: hover on g at line 4 (0-based = line 5 1-based) returns "parametric" label *)
VerificationTest[
  Module[{val = LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost",
      "id" -> 1,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>,
                    "position" -> <|"line" -> 4, "character" -> 0|>|>
    |>
  ][[1]]["result"]["contents"]["value"]},
    StringQ[val] && StringContainsQ[val, "parametric"]
  ],
  True,
  TestID -> "IPWL-Parametric-HoverShowsParametric"
]
