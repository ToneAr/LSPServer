Get[FileNameJoin[{DirectoryName[$TestFileName], "..", "hover", "Init.wl"}]];

ipwlSrc = ReadString[FileNameJoin[{DirectoryName[$TestFileName], "IPWLParseTest.ipwl"}]];

(* Test 1: function annotation is stripped to plain SetDelayed *)
VerificationTest[
  StringContainsQ[LSPServer`TypeWL`PreprocessIPWL[ipwlSrc][[1]], "computeSquare[x_Integer] :="],
  True,
  TestID -> "IPWL-Parse-FunctionAnnotationStripped"
]

(* Test 2: DeclaredType extracted for computeSquare *)
VerificationTest[
  Select[LSPServer`TypeWL`PreprocessIPWL[ipwlSrc][[2]],
    #["symbol"] === "computeSquare" &
  ][[1]]["DeclaredType"],
  _Integer,
  TestID -> "IPWL-Parse-FunctionDeclaredType"
]

(* Test 3: declaration-only produces a DeclaredType entry, no SetDelayed in output *)
VerificationTest[
  Module[{result = LSPServer`TypeWL`PreprocessIPWL[ipwlSrc]},
    {
      !StringContainsQ[result[[1]], "greet[name_String] :="],
      AnyTrue[result[[2]], #["symbol"] === "greet" &]
    }
  ],
  {True, True},
  TestID -> "IPWL-Parse-DeclarationOnly"
]

(* Test 4: variable annotation passes through unchanged *)
VerificationTest[
  StringContainsQ[LSPServer`TypeWL`PreprocessIPWL[ipwlSrc][[1]], "myVar: _Integer = 42"],
  True,
  TestID -> "IPWL-Parse-VariableAnnotationPassthrough"
]

(* Test 5: invalid annotation produces IPWLSyntaxError entry *)
VerificationTest[
  AnyTrue[LSPServer`TypeWL`PreprocessIPWL[ipwlSrc][[2]],
    #["kind"] === "IPWLSyntaxError" &
  ],
  True,
  TestID -> "IPWL-Parse-SyntaxError"
]


(* Tests 6-7: UpdateFileIndex stores DeclaredType and IsIPWL flag *)
LSPServer`PacletIndex`$PacletIndex = <|"Symbols" -> <||>, "Files" -> <||>, "Contexts" -> <||>, "Dependencies" -> {}, "ContextAliases" -> <||>|>;
ipwlPath = FileNameJoin[{DirectoryName[$TestFileName], "IPWLParseTest.ipwl"}];
ipwlURI  = LocalObjects`PathToURI[ipwlPath];
LSPServer`PacletIndex`UpdateFileIndex[ipwlURI, ReadString[ipwlPath]];

VerificationTest[
  Lookup[
    SelectFirst[
      Lookup[
        Lookup[LSPServer`PacletIndex`$PacletIndex["Symbols"], "computeSquare", <||>],
        "Definitions", {}
      ],
      #["kind"] === "declaration" &,
      <||>
    ],
    "DeclaredType",
    Missing["NotFound"]
  ],
  _Integer,
  TestID -> "IPWL-Index-DeclaredTypeStored"
]

VerificationTest[
  TrueQ[Lookup[
    Lookup[LSPServer`PacletIndex`$PacletIndex["Files"], ipwlURI, <||>],
    "IsIPWL",
    False
  ]],
  True,
  TestID -> "IPWL-Index-IsIPWLFlag"
]
