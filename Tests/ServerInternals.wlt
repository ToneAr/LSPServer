(* Load LSPServer from the repository's build/paclet so in-tree changes are picked up. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "build", "paclet"}]]];
<<LSPServer`
LSPServer`LoadAllFeatureModules[];
Needs["CodeParser`"];


VerificationTest[
  Module[{cst, agg, ast, lhs, signatureInfo},
    cst = CodeParser`CodeConcreteParse[
      "conditionedSignatureFn[x_String] /; x := x\n",
      "FileFormat" -> "Package"
    ];
    cst[[1]] = File;
    agg = CodeParser`Abstract`Aggregate[cst];
    ast = CodeParser`Abstract`Abstract[agg];
    lhs = First[
      Cases[ast,
        CodeParser`CallNode[
          CodeParser`LeafNode[Symbol, "SetDelayed", _],
          {lhsNode_, _},
          _
        ] :> lhsNode,
        Infinity
      ]
    ];
    signatureInfo = LSPServer`PacletIndex`ExtractFunctionSignatureInfo[
      "conditionedSignatureFn",
      lhs
    ];
    {
      ToString[Lookup[signatureInfo, "InputPatterns", {}], InputForm],
      Lookup[signatureInfo, "Variadic", Missing["NotFound"]]
    }
  ],
  {"{_String}", False},
  TestID -> "PacletIndex-Conditioned-LHS-Signature-Uses-Inner-Call"
]


VerificationTest[
  {
    Names["LSPServer`Hover`Private`GetVisibleSymbolDefinitions"],
    Names["LSPServer`Completion`Private`GetVisibleSymbolDefinitions"],
    Names["LSPServer`Definitions`Private`GetVisibleSymbolDefinitions"],
    Names["LSPServer`InlayHints`Private`GetVisibleSymbolDefinitions"],
    Context[LSPServer`PacletIndex`GetVisibleSymbolDefinitions],
    Length[DownValues[LSPServer`PacletIndex`GetVisibleSymbolDefinitions]] > 0
  },
  {{}, {}, {}, {}, "LSPServer`PacletIndex`", True},
  TestID -> "GetVisibleSymbolDefinitions-Is-Provided-By-PacletIndex-Package"
]


VerificationTest[
  {
    Context[LSPServer`PacletIndex`IsWorkspaceSymbol],
    Length[DownValues[LSPServer`PacletIndex`IsWorkspaceSymbol]] > 0,
    Context[LSPServer`PacletIndex`GetIndexedDependencySymbols],
    Length[DownValues[LSPServer`PacletIndex`GetIndexedDependencySymbols]] > 0
  },
  {"LSPServer`PacletIndex`", True, "LSPServer`PacletIndex`", True},
  TestID -> "SemanticTokens-PacletIndex-Public-Helpers-Are-Exported"
]


VerificationTest[
  Module[{cst, agg, ast},
    cst = CodeConcreteParse[
      "Needs[\"Foo`\"]\nGet[\"src/StrategySystem.m\"]\nGet[\"Bar`\"]\n"
    ];
    agg = CodeParser`Abstract`Aggregate[cst];
    ast = CodeParser`Abstract`Abstract[agg];
    LSPServer`PacletIndex`Private`extractDependencies[ast]
  ],
  {"Foo`", "Bar`"},
  TestID -> "PacletIndex-Ignores-Get-File-Paths"
]


VerificationTest[
  Block[{
    LSPServer`$OpenFilesMap = <|
      "file:///tmp/SemanticTokensRefresh.wl" -> <|
        "SemanticTokens" -> {1, 2, 3},
        "Other" -> 1
      |>
    |>,
    LSPServer`$InternalRequestId = 0
  },
    {
      LSPServer`handleContent[<|"method" -> "workspace/semanticTokens/refresh"|>],
      KeyExistsQ[
        LSPServer`$OpenFilesMap["file:///tmp/SemanticTokensRefresh.wl"],
        "SemanticTokens"
      ],
      LSPServer`$OpenFilesMap["file:///tmp/SemanticTokensRefresh.wl", "Other"]
    }
  ],
  {
    {<|"jsonrpc" -> "2.0", "id" -> -1, "method" -> "workspace/semanticTokens/refresh"|>},
    True,
    1
  },
  TestID -> "SemanticTokens-Refresh-Keeps-Cache"
]


VerificationTest[
  Block[{
    LSPServer`$SemanticTokens = True,
    LSPServer`$ContentQueue = {<|"method" -> "workspace/semanticTokens/refresh"|>}
  },
    LSPServer`Private`queueSemanticTokensRefresh[];
    Count[
      Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
      "workspace/semanticTokens/refresh"
    ]
  ],
  1,
  TestID -> "SemanticTokens-Refresh-Queue-Dedupes"
]


VerificationTest[
  Block[{
    LSPServer`$ContentQueue = {<|"method" -> "initialized"|>},
    LSPServer`$CancelMap = <||>
  },
    LSPServer`expandContentsAndAppendToContentQueue[{
      <|"jsonrpc" -> "2.0", "id" -> -2, "result" -> Null|>,
      <|"method" -> "workspace/didChangeConfiguration", "params" -> <||>|>
    }];
    Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]]
  ],
  {"initialized", "workspace/didChangeConfiguration"},
  TestID -> "Client-Responses-Without-Method-Are-Dropped-Before-Queueing"
]


VerificationTest[
  Block[{
    LSPServer`$ContentQueue = {<|"method" -> "textDocument/runFastDiagnostics"|>}
  },
    LSPServer`Private`appendContentsToContentQueue[{
      <|"method" -> "textDocument/hoverFencepost", "priority" -> True|>
    }];
    Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]]
  ],
  {"textDocument/hoverFencepost", "textDocument/runFastDiagnostics"},
  TestID -> "Priority-Flag-Reorders-Queue"
]


VerificationTest[
  Module[{uri, text, result},
    uri = "file:///tmp/FormattingCommentList.wl";
    text = StringJoin["{\n  a,\n  (", "*\n    c\n  *", ")\n  b\n}\n"];

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text|>|>
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/formatting",
        "id" -> 501,
        "params" -> <|
          "textDocument" -> <|"uri" -> uri|>,
          "options" -> <|"tabSize" -> 2, "insertSpaces" -> True|>
        |>
      |>];
      Lookup[
        Lookup[First[result], "result", <||>],
        "newText",
        Missing["NotFound"]
      ]
    ]
  ],
  StringJoin["{\n  a,\n  (", "*\n    c\n  *", ")\n  b\n}\n"],
  TestID -> "Formatting-List-Comment-Block-Stays-Inert"
]


VerificationTest[
  Module[{uri, text, result},
    uri = "file:///tmp/FormattingCommentAssociation.wl";
    text = StringJoin["<|\n  \"a\" -> 1,\n  (", "*\n    c\n  *", ")\n  \"b\" -> 2\n|>\n"];

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text|>|>
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/formatting",
        "id" -> 502,
        "params" -> <|
          "textDocument" -> <|"uri" -> uri|>,
          "options" -> <|"tabSize" -> 2, "insertSpaces" -> True|>
        |>
      |>];
      Lookup[
        Lookup[First[result], "result", <||>],
        "newText",
        Missing["NotFound"]
      ]
    ]
  ],
  StringJoin["<|\n  \"a\" -> 1,\n  (", "*\n    c\n  *", ")\n  \"b\" -> 2\n|>\n"],
  TestID -> "Formatting-Association-Comment-Block-Stays-Inert"
]


VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/SyncHover.wl";
    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "Sin[x]\n",
        "AST" -> HoldComplete[Null],
        "LastChange" -> Now,
        "ScheduledJobs" -> {}
      |>|>,
      ParallelSubmit = Function[{kernels, expr}, Throw["ParallelSubmitCalled", "parallel"]]
    },
      result = Catch[
        LSPServer`handleContent[<|
          "method" -> "textDocument/hoverFencepost",
          "id" -> 91,
          "params" -> <|
            "textDocument" -> <|"uri" -> uri|>,
            "position" -> <|"line" -> 0, "character" -> 1|>
          |>
        |>],
        "parallel"
      ];
      {
        MatchQ[result, {KeyValuePattern[{"jsonrpc" -> "2.0", "id" -> 91, "result" -> _Association}]}],
        result =!= "ParallelSubmitCalled"
      }
    ]
  ],
  {True, True},
  TestID -> "HoverFencepost-Runs-Synchronously-Without-Worker"
]


VerificationTest[
  Lookup[
    First @ LSPServer`handleContent[<|
      "method" -> "textDocument/publishHoverResult",
      "id" -> 77,
      "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/RemovedAsyncHover.wl"|>|>
    |>],
    "error",
    <||>
  ]["code"],
  LSPServer`Private`$ErrorCodes["MethodNotFound"],
  TestID -> "PublishHoverResult-Removed-MethodNotFound"
]


VerificationTest[
  Module[{text, cst, folds},
    text = "config = Module[{x},\n  x = 1;\n  x\n]\n\nfoo[a_] := Module[{y},\n  y = a;\n  y\n]\n";
    cst = Quiet[CodeParser`CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    folds = LSPServer`FoldingRange`Private`filterAssignmentAnchoredFoldingRanges[
      cst,
      {
        <|"startLine" -> 0, "endLine" -> 3, "kind" -> "region"|>,
        <|"startLine" -> 5, "endLine" -> 8, "kind" -> "region"|>
      }
    ];
    folds
  ],
  {
    <|"startLine" -> 5, "endLine" -> 8, "kind" -> "region"|>
  },
  TestID -> "FoldingRange-Filters-Plain-Set-Anchored-Folds"
]


VerificationTest[
  Block[{
    LSPServer`$ContentQueue = {
      <|"method" -> "textDocument/runFastDiagnostics"|>,
      <|"method" -> "textDocument/concreteParse", "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/Pipeline.wl"|>|>|>,
      <|"method" -> "textDocument/aggregateParse", "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/Pipeline.wl"|>|>|>,
      <|"method" -> "textDocument/hoverFencepost", "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/Pipeline.wl"|>,
          "position" -> <|"line" -> 0, "character" -> 0|>|>|>
    }
  },
    LSPServer`Private`takeFirstContentQueueItem[];
    Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]]
  ],
  {
    "textDocument/aggregateParse",
    "textDocument/hoverFencepost",
    "textDocument/runFastDiagnostics"
  },
  TestID -> "TakeFirstContentQueueItem-Promotes-Whole-Request-Pipeline"
]


VerificationTest[
  Block[{
    LSPServer`$PendingTokenRefresh = True
  },
    {
      LSPServer`handleContent[<|"jsonrpc" -> "2.0", "id" -> -1, "result" -> Null|>],
      LSPServer`$PendingTokenRefresh
    }
  ],
  {{}, False},
  TestID -> "ServerInitiatedResponse-Clears-PendingTokenRefresh"
]


VerificationTest[
  Module[{requestResult, notificationResult},
    requestResult = LSPServer`LSPEvaluate[<|
      "method" -> "workspace/unknownUnitTestMethod",
      "id" -> 999,
      "params" -> <||>
    |>];
    notificationResult = LSPServer`LSPEvaluate[<|
      "method" -> "workspace/unknownUnitTestNotification",
      "params" -> <||>
    |>];
    {
      Lookup[First[requestResult], "id", Missing["NotFound"]],
      Lookup[Lookup[First[requestResult], "error", <||>], "code", Missing["NotFound"]],
      notificationResult
    }
  ],
  {999, LSPServer`Private`$ErrorCodes["MethodNotFound"], {}},
  TestID -> "UnknownMethods-ReturnErrorForRequests-AndIgnoreNotifications"
]


VerificationTest[
  LSPServer`PacletIndex`Private`parseDocComment["(* Return: _[1 *)"],
  <|"Description" -> None, "ReturnPattern" -> None, "ReturnPatternString" -> "_[1"|>,
  TestID -> "PacletIndex-ParseDocComment-Swallows-Incomplete-ReturnPatterns"
]


VerificationTest[
  StringQ[
    LSPServer`PacletIndex`Private`decodeStringNodeValue[
      LeafNode[String, FromCharacterCode[{34, 92, 91, 65, 108, 112, 104, 97, 34}], <||>]
    ]
  ],
  True,
  TestID -> "PacletIndex-ExtractUsages-Swallows-Malformed-StringTokens"
]


VerificationTest[
  Module[{uri, text, params, result},
    uri = "file:///tmp/SingleConvergenceEntry.wl";
    text = StringRiffle[{
      "needsInt[x_Integer] := x",
      "test[] := Module[{ref},",
      "  If[True, ref = 0, ref = 1];",
      "  needsInt[ref]",
      "]"
    }, "\n"];
    params = <|"textDocument" -> <|"uri" -> uri|>|>;

    Block[{
      LSPServer`$WorkspaceRootPath = "/tmp",
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$ClosedFileDiagnosticsNotifications = <||>,
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text, "LastChange" -> Now|>|>,
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`Diagnostics`Private`$BuiltinPatterns = <||>
    },
      Scan[
        LSPServer`handleContent[<|"method" -> #, "params" -> params|>] &,
        {
          "textDocument/concreteParse",
          "textDocument/aggregateParse",
          "textDocument/abstractParse"
        }
      ];

      result = Check[
        LSPServer`handleContent[<|"method" -> "textDocument/runWorkspaceDiagnostics", "params" -> params|>],
        "MESSAGE"
      ];

      {
        result =!= "MESSAGE",
        MatchQ[result, {} | {_Association}],
        ListQ[Lookup[LSPServer`$OpenFilesMap[uri], "WorkspaceLints", {}]]
      }
    ]
  ],
  {True, True, True},
  TestID -> "RunWorkspaceDiagnostics-Single-Convergence-Entry-NoMessages"
]


VerificationTest[
  Module[{fixturePath, uri, fileText, openParams, uriParams, result},
    fixturePath = FileNameJoin[{DirectoryName[$TestFileName], "..", "LSPServer", "Kernel", "Diagnostics.wl"}];
    uri = LocalObjects`PathToURI[fixturePath];
    fileText = ReadString[fixturePath];
    openParams = <|
      "textDocument" -> <|
        "uri" -> uri,
        "languageId" -> "wolfram",
        "version" -> 1,
        "text" -> fileText
      |>
    |>;
    uriParams = <|"textDocument" -> <|"uri" -> uri|>|>;

    Block[{
      LSPServer`$WorkspaceRootPath = FileNameJoin[{DirectoryName[$TestFileName], ".."}],
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$ClosedFileDiagnosticsNotifications = <||>,
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$DiagnosticsKernel = $Failed,
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`Diagnostics`Private`$BuiltinPatterns = <||>
    },
      Scan[
        LSPServer`handleContent[
          <|
            "method" -> #,
            "params" -> If[# === "textDocument/didOpenFencepost", openParams, uriParams]
          |>
        ] &,
        {
          "textDocument/didOpenFencepost",
          "textDocument/runOpenIndexUpdate",
          "textDocument/concreteParse",
          "textDocument/suppressedRegions",
          "textDocument/parseIgnoreComments",
          "textDocument/runConcreteDiagnostics",
          "textDocument/aggregateParse",
          "textDocument/runAggregateDiagnostics",
          "textDocument/abstractParse",
          "textDocument/runAbstractDiagnostics"
        }
      ];

      result = Check[
        LSPServer`handleContent[<|"method" -> "textDocument/runWorkspaceDiagnostics", "params" -> uriParams|>],
        "MESSAGE"
      ];

      {
        result =!= "MESSAGE",
        MatchQ[result, {} | {_Association}]
      }
    ]
  ],
  {True, True},
  TestID -> "RunWorkspaceDiagnostics-DiagnosticsFile-NoMessages"
]


VerificationTest[
  Module[{uri, params, result},
    uri = "file:///tmp/HoverFastPath.wl";
    params = <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 0, "character" -> 0|>|>;

    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> "foo", "AST" -> HoldComplete[foo], "ScheduledJobs" -> {}|>|>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$ContentQueue = {}
    },
      result = LSPServer`expandContent[
        <|"method" -> "textDocument/hover", "id" -> 42, "params" -> params|>,
        {0}
      ];
      {
        Length[result],
        Lookup[result[[1]], "method", Missing["NotFound"]],
        Lookup[result[[1]], "priority", False]
      }
    ]
  ],
  {1, "textDocument/hoverFencepost", True},
  TestID -> "Hover-Ready-Entry-Queues-Priority-Fencepost-Only"
]


VerificationTest[
  Module[{launchCalled = False, dispatched = {}, uri, workspaceRoot},
    uri = "file:///tmp/ClosedFile.wl";
    workspaceRoot = DirectoryName[LSPServer`Utils`normalizeURI[uri]];

    Block[{
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$WorkspaceRootPath = workspaceRoot,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {uri},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$DiagnosticsKernel = None,
      LSPServer`$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 60,
      LSPServer`Private`launchDiagnosticsKernel = Function[{}, launchCalled = True],
      LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics = Function[{uri}, AppendTo[dispatched, uri]]
    },
      LSPServer`handleContent[<|"method" -> "workspace/bootstrapClosedFileDiagnostics"|>];
      {
        launchCalled,
        dispatched,
        LSPServer`$WorkspaceDiagnosticsSweepURIs
      }
    ]
  ],
  {False, {"file:///tmp/ClosedFile.wl"}, {}},
  TestID -> "Bootstrap-Closed-File-Diagnostics-Uses-Synchronous-Fallback"
]


VerificationTest[
  {
    Length[OwnValues[LSPServer`Library`Private`getStartupError]] > 0,
    Length[DownValues[LSPServer`Private`takeFirstContentQueueItem]] > 0,
    Names["LSPServer`CST`Private`takeFirstContentQueueItem"] === {}
  },
  {True, True, True},
  TestID -> "Startup-PreloadAndCSTContext-AreBoundCorrectly"
]


VerificationTest[
  Module[{dispatched = {}, uri, workspaceRoot},
    uri = "file:///tmp/ClosedFile.wl";
    workspaceRoot = DirectoryName[LSPServer`Utils`normalizeURI[uri]];

    Block[{
      LSPServer`$ServerState = "running",
      LSPServer`$PendingExternalDepFiles = {},
      LSPServer`$PendingIndexFiles = {},
      LSPServer`$PendingReferenceFiles = {},
      LSPServer`$IndexingWasActive = False,
      LSPServer`$ContentQueue = {},
      LSPServer`$QueueLastNonEmptyTime = AbsoluteTime[] - 5,
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$DiagnosticsTaskStartTime = None,
      LSPServer`$DiagnosticsKernel = None,
      LSPServer`$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 60,
      LSPServer`$WorkspaceRootPath = workspaceRoot,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {uri},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`Private`ProcessPendingIndexFiles = Function[{}, False],
      LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics = Function[{uri}, AppendTo[dispatched, uri]]
    },
      LSPServer`ProcessScheduledJobs[];
      {
        dispatched,
        LSPServer`$WorkspaceDiagnosticsSweepURIs
      }
    ]
  ],
  {{"file:///tmp/ClosedFile.wl"}, {}},
  TestID -> "ProcessScheduledJobs-Starts-Sync-Closed-File-Sweep-While-Kernel-Pending"
]


VerificationTest[
  Block[{
    LSPServer`$ContentQueue = {},
    LSPServer`$OpenFilesMap = <||>,
    LSPServer`$DiagnosticsTask = None,
    LSPServer`$DiagnosticsTaskKind = None,
    LSPServer`$DiagnosticsTaskURI = None,
    LSPServer`$DiagnosticsTaskResult = None,
    LSPServer`$DiagnosticsTaskStartTime = None,
    LSPServer`$DiagnosticsKernel = Unique["Kernel"]
  },
    LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics["file:///tmp/ClosedFile.wl"];
    {
      Count[
        LSPServer`$ContentQueue,
        KeyValuePattern[{
          "method" -> "textDocument/runClosedFileDiagnostics",
          "params" -> KeyValuePattern["textDocument" -> KeyValuePattern["uri" -> "file:///tmp/ClosedFile.wl"]]
        }]
      ],
      LSPServer`$DiagnosticsTask,
      LSPServer`$DiagnosticsTaskKind,
      LSPServer`$DiagnosticsTaskURI
    }
  ],
  {1, None, None, None},
  TestID -> "DispatchClosedFileDiagnostics-Queues-Sync-Work-Even-With-Live-Kernel"
]


VerificationTest[
  Block[{
    LSPServer`$ContentQueue = {
      <|"method" -> "textDocument/runWorkspaceDiagnostics", "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/OpenFile.wl"|>|>|>,
      <|"method" -> "initialized"|>
    },
    LSPServer`$DiagnosticsTask = None,
    LSPServer`$DiagnosticsTaskKind = None,
    LSPServer`$DiagnosticsTaskURI = None,
    LSPServer`$DiagnosticsTaskResult = None,
    LSPServer`$DiagnosticsTaskStartTime = None,
    LSPServer`$DiagnosticsKernel = Unique["Kernel"],
    LSPServer`$OpenFilesMap = <|
      "file:///tmp/OpenFile.wl" -> <|
        "AST" -> HoldComplete[openFileAST],
        "LastChange" -> Now
      |>
    |>,
    ParallelSubmit = Function[{kernels, expr}, "fake-task", HoldAll]
  },
    LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics["file:///tmp/OpenFile.wl"];
    {
      Count[
        LSPServer`$ContentQueue,
        KeyValuePattern[{
          "method" -> "textDocument/runWorkspaceDiagnostics",
          "params" -> KeyValuePattern["textDocument" -> KeyValuePattern["uri" -> "file:///tmp/OpenFile.wl"]]
          }]
      ],
      Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
      LSPServer`$DiagnosticsTask,
      LSPServer`$DiagnosticsTaskKind,
      LSPServer`$DiagnosticsTaskURI
    }
  ],
  {1, {"textDocument/runWorkspaceDiagnostics", "initialized"}, None, None, None},
  TestID -> "DispatchWorkspaceDiagnostics-Dedupes-Synchronous-Work-With-Live-Kernel"
]


VerificationTest[
  {
    LSPServer`SourceFileFormat["file:///tmp/TestFile.wl"],
    LSPServer`SourceFileFormat["file:///tmp/TestFile.m"],
    LSPServer`SourceFileFormat["file:///tmp/TestFile.wls"]
  },
  {"Package", "Package", "Script"},
  TestID -> "SourceFileFormat-Treats-Only-WLS-As-Script"
]


VerificationTest[
  Module[{queued = {}, needsCalls = {}},
    Block[{
      LSPServer`PacletIndex`$PacletIndex = <|
        "Contexts" -> <|"Workspace`" -> {}|>
      |>,
      LSPServer`PacletIndex`Private`$LoadedExternalDependencies = <||>,
      LSPServer`PacletIndex`Private`queueExternalPackageFiles = Function[{dep}, AppendTo[queued, dep]],
      Needs = Function[{dep}, AppendTo[needsCalls, dep]; Null]
    },
      LSPServer`PacletIndex`Private`loadExternalDependencies[{
        "Parallel`",
        "Workspace`",
        "Workspace`Private`",
        "src/StrategySystem.m"
      }];
      {
        queued,
        needsCalls,
        Lookup[LSPServer`PacletIndex`Private`$LoadedExternalDependencies, "Parallel`", False],
        Lookup[LSPServer`PacletIndex`Private`$LoadedExternalDependencies, "Workspace`", False]
      }
    ]
  ],
  {
    {"Parallel`"},
    {},
    True,
    False
  },
  TestID -> "PacletIndex-External-Dependencies-Avoid-Needs"
]


VerificationTest[
  Block[{
    LSPServer`$AllowedImplicitTokens = {},
    LSPServer`$BracketMatcher = False,
    LSPServer`$SemanticTokens = False,
    LSPServer`$InlayHints = False,
    LSPServer`$CodeActionLiteralSupport = False,
    LSPServer`$HierarchicalDocumentSymbolSupport = True,
    LSPServer`$kernelStartTime = Now
  },
    Module[{result, capabilities},
      result = First @ LSPServer`handleContent[<|
        "method" -> "initialize",
        "id" -> 1,
        "params" -> <|
          "capabilities" -> <|
            "textDocument" -> <|
              "documentSymbol" -> <||>
            |>
          |>
        |>
      |>];
      capabilities = Lookup[Lookup[result, "result", <||>], "capabilities", <||>];
      Lookup[capabilities, "codeActionProvider", Missing["NotFound"]] === True &&
        LSPServer`$HierarchicalDocumentSymbolSupport === False
    ]
  ],
  True,
  TestID -> "Initialize-Missing-Optional-Document-Capabilities-Default-Safely"
]


VerificationTest[
  Block[{
    LSPServer`$AllowedImplicitTokens = {},
    LSPServer`$BracketMatcher = False,
    LSPServer`$SemanticTokens = False,
    LSPServer`$InlayHints = False,
    LSPServer`$CodeActionLiteralSupport = False,
    LSPServer`$HierarchicalDocumentSymbolSupport = True,
    LSPServer`$kernelStartTime = Now
  },
    Module[{result, capabilities, semanticTokensProvider, legend},
      result = Check[
        First @ LSPServer`handleContent[<|
          "method" -> "initialize",
          "id" -> 1,
          "params" -> <|
            "capabilities" -> <|
              "textDocument" -> <|
                "semanticTokens" -> <||>
              |>
            |>,
            "initializationOptions" -> <|
              "semanticTokens" -> True
            |>
          |>
        |>],
        "MESSAGE"
      ];
      If[result === "MESSAGE",
        "MESSAGE",
        capabilities = Lookup[Lookup[result, "result", <||>], "capabilities", <||>];
        semanticTokensProvider = Lookup[capabilities, "semanticTokensProvider", <||>];
        legend = Lookup[semanticTokensProvider, "legend", <||>];
        {
          Lookup[legend, "tokenTypes", Missing["NotFound"]],
          Lookup[legend, "tokenModifiers", Missing["NotFound"]]
        }
      ]
    ]
  ],
  {
    Keys[LSPServer`SemanticTokens`$SemanticTokenTypes],
    Keys[LSPServer`SemanticTokens`$SemanticTokenModifiers]
  },
  TestID -> "Initialize-Semantic-Tokens-Legend-Loads-Feature-Module"
]


VerificationTest[
  Module[{result, hoverTag = Unique["hoverTag"]},
    Block[{
      LSPServer`PacletIndex`GetDependencyContexts = Function[{}, {"UnitTestExternal`"}],
      LSPServer`PacletIndex`GetContextAliases = Function[{}, <||>],
      LSPServer`PacletIndex`GetKernelContextsCached = Function[{}, {"System`", "Global`"}],
      Needs = Function[args, Throw["NeedsCalled", hoverTag]]
    },
      result = Catch[
        LSPServer`Hover`Private`handleExternalSymbols["UnitTestExternal`Sub`EntityGet"],
        hoverTag
      ];
      {
        AssociationQ[result],
        Lookup[result, "SymbolType", Missing["NotFound"]],
        Lookup[result, "Context", Missing["NotFound"]],
        StringContainsQ[Lookup[result, "Usage", ""], "context not loaded"],
        Lookup[result, "FunctionDefinitionPatterns", Missing["NotFound"]]
      }
    ]
  ],
  {True, "External", "UnitTestExternal`Sub`", True, None},
  TestID -> "Hover-External-Unloaded-Context-Avoids-Needs"
]


VerificationTest[
  Module[{dispatched = {}, uri, workspaceRoot},
    uri = "file:///tmp/testws/BusyQueueClosed.wl";

    workspaceRoot = DirectoryName[LSPServer`Utils`normalizeURI[uri]];

    Block[{
      LSPServer`$ServerState = "running",
      LSPServer`$PendingExternalDepFiles = {},
      LSPServer`$PendingIndexFiles = {},
      LSPServer`$PendingReferenceFiles = {},
      LSPServer`$IndexingWasActive = False,
      LSPServer`$ContentQueue = {<|"method" -> "textDocument/documentSymbolFencepost"|>},
      LSPServer`$QueueLastNonEmptyTime = AbsoluteTime[],
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$DiagnosticsTaskStartTime = None,
      LSPServer`$DiagnosticsKernel = Unique["Kernel"],
      LSPServer`$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 60,
      LSPServer`$WorkspaceRootPath = workspaceRoot,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {uri},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`Private`ProcessPendingIndexFiles = Function[{}, False],
      LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics = Function[{uriArg}, AppendTo[dispatched, uriArg]]
    },
      LSPServer`ProcessScheduledJobs[];
      {
        dispatched,
        LSPServer`$WorkspaceDiagnosticsSweepURIs
      }
    ]
  ],
  {{}, {"file:///tmp/testws/BusyQueueClosed.wl"}},
  TestID -> "ProcessScheduledJobs-Defers-Closed-File-Sweep-While-Queue-Busy"
]


(*
SemanticTokens-Refresh-Precomputes-Missing-Tokens:
computeAndCacheSemanticTokens still supports explicit synchronous precompute
when another path already knows it needs tokens immediately.
*)
VerificationTest[
  Module[{uri, text, cst, agg, ast, entry, result, hasTokens},
    uri = "file:///tmp/test_precompute.wl";
    text = "x = 1\ny[z_] := z + 1\n";

    (* Simulate didOpenFencepost: parse and store CST/AST but no SemanticTokens *)
    cst = Quiet[CodeParser`CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    entry = <|"Text" -> text, "CST" -> cst, "Agg" -> agg, "AST" -> ast|>;

    Block[{LSPServer`$OpenFilesMap = <|uri -> entry|>},
      (* computeAndCacheSemanticTokens should populate SemanticTokens *)
      result = LSPServer`SemanticTokens`computeAndCacheSemanticTokens[uri];
      hasTokens = KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokens"] &&
                  ListQ[LSPServer`$OpenFilesMap[uri, "SemanticTokens"]];
      {result, hasTokens, Lookup[LSPServer`$OpenFilesMap[uri], "SemanticTokensIncomplete", Missing["NotFound"]]}
    ]
  ],
  {True, True, Missing["NotFound"]},
  TestID -> "SemanticTokens-Refresh-Precomputes-Missing-Tokens"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, entry, result, hasTokens},
    uri = "file:///tmp/test_refresh_handler_precompute.wl";
    text = "x = 1\ny[z_] := z + 1\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    entry = <|"Text" -> text, "CST" -> cst, "Agg" -> agg, "AST" -> ast|>;

    Block[{
      LSPServer`$OpenFilesMap = <|uri -> entry|>,
      LSPServer`$InternalRequestId = 0
    },
      result = LSPServer`handleContent[<|"method" -> "workspace/semanticTokens/refresh"|>];
      hasTokens = KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokens"] &&
                  ListQ[LSPServer`$OpenFilesMap[uri, "SemanticTokens"]];
      {result, hasTokens}
    ]
  ],
  {
    {<|"jsonrpc" -> "2.0", "id" -> -1, "method" -> "workspace/semanticTokens/refresh"|>},
    False
  },
  TestID -> "SemanticTokens-Refresh-Handler-Skips-Precompute-Without-Pending-Requests"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, result},
    uri = "file:///tmp/test_semantic_tokens_fencepost_queues_scoping_followup.wl";
    text = "x = 1\ny[z_] := Module[{a}, a + z]\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text, "CST" -> cst, "Agg" -> agg, "AST" -> ast|>|>,
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      LSPServer`$CancelMap = <||>
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/semanticTokens/fullFencepost",
        "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];

      {
        MatchQ[result, {<|"jsonrpc" -> "2.0", "id" -> 42, "result" -> KeyValuePattern["data" -> _List]|>}],
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        TrueQ[Lookup[LSPServer`$OpenFilesMap[uri], "SemanticTokensIncomplete", False]],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokens"],
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {True, {"textDocument/runScopingData"}, True, True, Missing["NotFound"]},
  TestID -> "SemanticTokens-Fencepost-Queues-Scoping-Followup-For-First-Pass"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, usedSlowClassifier = False, result},
    uri = "file:///tmp/test_semantic_tokens_first_pass_uses_fast_classifier.wl";
    text = "x = 1\ny[z_] := Module[{a}, a + z]\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text, "CST" -> cst, "Agg" -> agg, "AST" -> ast|>|>,
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      LSPServer`$CancelMap = <||>,
      LSPServer`SemanticTokens`Private`classifyGlobalSymbol = Function[{name}, usedSlowClassifier = True; {"comment", {"error"}}]
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/semanticTokens/fullFencepost",
        "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];

      {
        MatchQ[result, {<|"jsonrpc" -> "2.0", "id" -> 42, "result" -> KeyValuePattern["data" -> _List]|>}],
        usedSlowClassifier,
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]]
      }
    ]
  ],
  {True, False, {"textDocument/runScopingData"}},
  TestID -> "SemanticTokens-First-Pass-Uses-Fast-Classifier"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, result},
    uri = "file:///tmp/test_semantic_tokens_large_file_fast_only.wl";
    text = "x = 1\ny[z_] := Module[{a}, a + z]\n";

    cst = Quiet[CodeParser`CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`SemanticTokens`Private`$SemanticTokensScopingTextLengthLimit = 5,
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text, "CST" -> cst, "Agg" -> agg, "AST" -> ast|>|>,
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      LSPServer`$CancelMap = <||>
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/semanticTokens/fullFencepost",
        "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];

      {
        MatchQ[result, {<|"jsonrpc" -> "2.0", "id" -> 42, "result" -> KeyValuePattern["data" -> _List]|>}],
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokens"],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokensIncomplete"]
      }
    ]
  ],
  {True, Missing["NotFound"], True, False},
  TestID -> "SemanticTokens-LargeFile-Skips-Scoping-Followup"
]


VerificationTest[
  Module[{uri, text, result},
    uri = "file:///tmp/test_large_file_index_skip.wl";
    text = "largeIndexSkipValue = <|\"a\" -> 1, \"b\" -> 2|>\n";

    Block[{
      LSPServer`PacletIndex`Private`$LargeFileIndexTextLengthLimit = 5,
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>
    },
      result = LSPServer`PacletIndex`UpdateFileIndex[uri, text];
      {
        MatchQ[result, {_, _, _}],
        Lookup[LSPServer`PacletIndex`$PacletIndex["Files", uri], "Symbols", Missing["NotFound"]],
        KeyExistsQ[LSPServer`PacletIndex`$PacletIndex["Symbols"], "largeIndexSkipValue"]
      }
    ]
  ],
  {True, {}, False},
  TestID -> "PacletIndex-LargeFile-Skips-SymbolExtraction"
]


VerificationTest[
  Module[{uri, text, result},
    uri = "file:///tmp/test_runscopingdata_large_file_keeps_fast_tokens.wl";
    text = "x = 1\ny[z_] := Module[{a}, a + z]\n";

    Block[{
      LSPServer`SemanticTokens`Private`$SemanticTokensScopingTextLengthLimit = 5,
      LSPServer`$SemanticTokens = True,
      LSPServer`$PendingTokenRefresh = False,
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> text,
        "AST" -> HoldComplete[Null],
        "SemanticTokens" -> {1, 2, 3},
        "SemanticTokensIncomplete" -> True
      |>|>,
      LSPServer`$ContentQueue = {}
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/runScopingData",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];

      {
        result,
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "ScopingData"],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokens"],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokensIncomplete"],
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        LSPServer`$PendingTokenRefresh
      }
    ]
  ],
  {{}, True, True, False, Missing["NotFound"], False},
  TestID -> "RunScopingData-LargeFile-KeepsFastTokensWithoutRefresh"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, result},
    uri = "file:///tmp/test_runscopingdata_refreshes_incomplete_tokens.wl";
    text = "x = 1\ny[z_] := Module[{a}, a + z]\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$PendingTokenRefresh = False,
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> text,
        "CST" -> cst,
        "Agg" -> agg,
        "AST" -> ast,
        "SemanticTokens" -> {1, 2, 3},
        "SemanticTokensIncomplete" -> True
      |>|>,
      LSPServer`$ContentQueue = {}
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/runScopingData",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];

      {
        result,
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "ScopingData"],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokens"],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokensIncomplete"],
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        LSPServer`$PendingTokenRefresh
      }
    ]
  ],
  {{}, True, True, False, {"workspace/semanticTokens/refresh"}, True},
  TestID -> "RunScopingData-Recomputes-Incomplete-Tokens-And-Queues-Refresh"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, entry, responses, recovered, queuedIDs, fencepostResponses},
    uri = "file:///tmp/test_refresh_handler_recovers_pending_request.wl";
    text = "x = 1\ny[z_] := z + 1\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    entry = <|"Text" -> text, "CST" -> cst, "Agg" -> agg, "AST" -> ast|>;

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$OpenFilesMap = <|uri -> entry|>,
      LSPServer`$ContentQueue = {},
      LSPServer`$InternalRequestId = 0,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>
    },
      responses = LSPServer`handleContent[<|"method" -> "workspace/semanticTokens/refresh"|>];
      queuedIDs = Cases[
        LSPServer`$ContentQueue,
        KeyValuePattern[{"method" -> "textDocument/semanticTokens/fullFencepost", "id" -> pendingID_}] :> pendingID
      ];
      fencepostResponses = LSPServer`handleContent /@ Select[
        LSPServer`$ContentQueue,
        MatchQ[#, KeyValuePattern[{"method" -> "textDocument/semanticTokens/fullFencepost"}]] &
      ];
      recovered = AnyTrue[
        Flatten[fencepostResponses, 1],
        AssociationQ[#] &&
        Lookup[#, "jsonrpc", None] === "2.0" &&
        Lookup[#, "id", None] === 42 &&
        MatchQ[Lookup[#, "result", None], KeyValuePattern["data" -> _List]] &
      ];
      {
        Lookup[First[responses], "method", None] === "workspace/semanticTokens/refresh",
        queuedIDs,
        recovered,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {True, {42}, True, Missing["NotFound"]},
  TestID -> "SemanticTokens-Refresh-Recovers-Pending-Requests"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, entry, responses, recoveredData, fencepostResponses, queuedIDs},
    uri = "file:///tmp/test_refresh_handler_recovers_pending_request_with_cache.wl";
    text = "x = 1\ny[z_] := z + 1\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    entry = <|
      "Text" -> text,
      "CST" -> cst,
      "Agg" -> agg,
      "AST" -> ast,
      "SemanticTokens" -> {9, 9, 9}
    |>;

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$OpenFilesMap = <|uri -> entry|>,
      LSPServer`$ContentQueue = {},
      LSPServer`$InternalRequestId = 0,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>
    },
      responses = LSPServer`handleContent[<|"method" -> "workspace/semanticTokens/refresh"|>];
      queuedIDs = Cases[
        LSPServer`$ContentQueue,
        KeyValuePattern[{"method" -> "textDocument/semanticTokens/fullFencepost", "id" -> pendingID_}] :> pendingID
      ];
      fencepostResponses = LSPServer`handleContent /@ Select[
        LSPServer`$ContentQueue,
        MatchQ[#, KeyValuePattern[{"method" -> "textDocument/semanticTokens/fullFencepost"}]] &
      ];
      recoveredData = Cases[
        Flatten[fencepostResponses, 1],
        assoc_ /; AssociationQ[assoc] &&
          Lookup[assoc, "jsonrpc", None] === "2.0" &&
          Lookup[assoc, "id", None] === 42 :>
            Lookup[Lookup[assoc, "result", <||>], "data", Missing["NotFound"]],
        Infinity
      ];
      {
        Lookup[First[responses], "method", None] === "workspace/semanticTokens/refresh",
        queuedIDs,
        recoveredData =!= {} && First[recoveredData] === {9, 9, 9},
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "SemanticTokens"]
      }
    ]
  ],
  {True, {42}, True, Missing["NotFound"], True},
  TestID -> "SemanticTokens-Refresh-Recovers-Pending-Requests-With-Cached-Tokens"
]


VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/test_stale_fencepost_pending.wl";

    Block[{
      LSPServer`$ContentQueue = {
        <|
          "method" -> "textDocument/didChangeFencepost",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      },
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/semanticTokens/fullFencepost",
        "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];

      {
        result,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {
    {},
    {42}
  },
  TestID -> "SemanticTokens-Stale-Fencepost-Keeps-Pending-Request"
]


VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/test_didclose_stale_fencepost.wl";

    Block[{
      LSPServer`$ContentQueue = {
        <|
          "method" -> "textDocument/didCloseFencepost",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      },
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/semanticTokens/fullFencepost",
        "id" -> 42,
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];

      {
        result,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {
    {<|"jsonrpc" -> "2.0", "id" -> 42, "result" -> Null|>},
    Missing["NotFound"]
  },
  TestID -> "SemanticTokens-DidClose-Stale-Fencepost-Clears-Pending-Request"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, job, jobResult},
    uri = "file:///tmp/test_didchange_refresh.wl";
    text = "x = 1\ny[z_] := z + 1\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$didChangeScheduledJobs = {},
      LSPServer`$PendingTokenRefresh = False,
      LSPServer`$ContentQueue = {<|"method" -> "textDocument/runWorkspaceDiagnostics"|>},
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      LSPServer`$OpenFilesMap = <|
        uri -> <|
          "Text" -> text,
          "AST" -> ast,
          "UserSymbols" -> {},
          "PreviousAST" -> ast,
          "PreviousUserSymbols" -> {}
        |>
      |>,
      LSPServer`PacletIndex`UpdateFileIndex = Function[{uriArg, textArg}, {cst, agg, ast}],
      LSPServer`Private`UpdateFileIndex = Function[{uriArg, textArg}, {cst, agg, ast}],
      LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics = Function[uriArg, Null]
    },
      LSPServer`handleContent[<|
        "method" -> "textDocument/didChangeFencepost",
        "params" -> <|
          "textDocument" -> <|"uri" -> uri|>,
          "contentChanges" -> {<|"text" -> text|>}
        |>
      |>];

      LSPServer`$OpenFilesMap[uri, "LastChange"] = Now - Quantity[1, "Seconds"];
      job = First[LSPServer`$OpenFilesMap[uri, "ScheduledJobs"]];
      jobResult = job[LSPServer`$OpenFilesMap[uri]];

      (* The scheduled job now returns {"textDocument/runIndexUpdate"} to be
         queued, rather than running UpdateFileIndex inline. Simulate the
         ProcessScheduledJobs path that converts the job result into a queue
         item, then process it. *)
      AppendTo[LSPServer`$ContentQueue,
        <|"method" -> "textDocument/runIndexUpdate",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>];
      LSPServer`handleContent[Last[LSPServer`$ContentQueue]];

      {
        Count[
          Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
          "textDocument/semanticTokens/fullFencepost"
        ],
        Count[
          Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
          "workspace/semanticTokens/refresh"
        ],
        Cases[
          LSPServer`$ContentQueue,
          KeyValuePattern[{"method" -> "textDocument/semanticTokens/fullFencepost", "id" -> pendingID_}] :> pendingID
        ]
      }
    ]
  ],
  {1, 0, {42}},
  TestID -> "DidChange-Recovers-Pending-SemanticToken-Requests-While-Busy"
]


VerificationTest[
  Module[{uri, oldText, newText, cstOld, aggOld, astOld, job, oldEntry, newerStamp},
    uri = "file:///tmp/test_didchange_stale_refresh.wl";
    oldText = "x = 1\n";
    newText = "x = 2\n";

    cstOld = Quiet[CodeConcreteParse[oldText, "FileFormat" -> "Package"]];
    cstOld[[1]] = File;
    aggOld = Quiet[CodeParser`Abstract`Aggregate[cstOld]];
    astOld = Quiet[CodeParser`Abstract`Abstract[aggOld]];

    newerStamp = Now;

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$didChangeScheduledJobs = {},
      LSPServer`$ContentQueue = {},
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      LSPServer`$OpenFilesMap = <|uri -> <||>|>,
      LSPServer`PacletIndex`UpdateFileIndex = Function[{uriArg, textArg}, {cstOld, aggOld, astOld}],
      LSPServer`Private`UpdateFileIndex = Function[{uriArg, textArg}, {cstOld, aggOld, astOld}],
      LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics = Function[uriArg, Null]
    },
      LSPServer`handleContent[<|
        "method" -> "textDocument/didChangeFencepost",
        "params" -> <|
          "textDocument" -> <|"uri" -> uri|>,
          "contentChanges" -> {<|"text" -> oldText|>}
        |>
      |>];

      oldEntry = LSPServer`$OpenFilesMap[uri];
      oldEntry["LastChange"] = Now - Quantity[1, "Seconds"];
      job = First[oldEntry["ScheduledJobs"]];

      LSPServer`$OpenFilesMap[uri] = <|
        "Text" -> newText,
        "LastChange" -> newerStamp,
        "ScheduledJobs" -> {},
        "PreviousAST" -> astOld,
        "PreviousUserSymbols" -> {}
      |>;

      job[oldEntry];

      {
        Count[
          Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
          "textDocument/semanticTokens/fullFencepost"
        ],
        Count[
          Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
          "workspace/semanticTokens/refresh"
        ],
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "CST"],
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {0, 0, False, {42}},
  TestID -> "DidChange-Stale-Index-Does-Not-Queue-SemanticToken-Recovery"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast},
    uri = "file:///tmp/test_didopen_pending_refresh.wl";
    text = "x = 1\ny[z_] := z + 1\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$ContentQueue = {},
      LSPServer`$PendingTokenRefresh = False,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`PacletIndex`UpdateFileIndex = Function[{uriArg, textArg}, {cst, agg, ast}],
      LSPServer`Private`UpdateFileIndex = Function[{uriArg, textArg}, {cst, agg, ast}]
    },
      {
        LSPServer`handleContent[<|
          "method" -> "textDocument/didOpenFencepost",
          "params" -> <|
            "textDocument" -> <|
              "uri" -> uri,
              "text" -> text
            |>
          |>
        |>],
        LSPServer`handleContent[LSPServer`Private`takeFirstContentQueueItem[]],
        LSPServer`$ContentQueue,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]],
        LSPServer`$PendingTokenRefresh
      }
    ]
  ],
  {
    {},
    {},
    {
      <|"method" -> "textDocument/semanticTokens/fullFencepost", "id" -> 42, "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/test_didopen_pending_refresh.wl"|>|>, "priority" -> False, "deferrable" -> True|>,
      <|"method" -> "textDocument/runFastDiagnostics", "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/test_didopen_pending_refresh.wl"|>|>|>
    },
    {42},
    False
  },
  TestID -> "DidOpen-Queues-Pending-SemanticToken-Requests-And-FastDiagnostics"
]


VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/test_semantic_tokens_wait_for_didopen.wl";

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {
        <|
          "method" -> "textDocument/didOpenFencepost",
          "params" -> <|"textDocument" -> <|"uri" -> uri, "text" -> "x = 1\n"|>|>
        |>
      },
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <||>
    },
      result = LSPServer`expandContent[
        <|
          "method" -> "textDocument/semanticTokens/full",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>,
        {0}
      ];

      {
        result,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {{}, {42}},
  TestID -> "SemanticTokens-Open-Race-Waits-For-DidOpenFencepost"
]


VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/test_semantic_tokens_after_didopen_before_index.wl";

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "x = 1\n",
        "LastChange" -> Now,
        "ScheduledJobs" -> {},
        "IndexUpdatePending" -> True
      |>|>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <||>
    },
      result = LSPServer`expandContent[
        <|
          "method" -> "textDocument/semanticTokens/full",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>,
        {0}
      ];

      {
        Lookup[result, "method", Missing["NotFound"]],
        Lookup[result, "id", Missing["NotFound"]],
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {
    {
      "textDocument/concreteParse",
      "textDocument/aggregateParse",
      "textDocument/abstractParse",
      "textDocument/semanticTokens/fullFencepost"
    },
    {42, 42, 42, 42},
    {42}
  },
  TestID -> "SemanticTokens-Open-Entry-Builds-Parse-Pipeline-Before-IndexUpdate"
]


VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/test_semantic_tokens_wait_for_reindex.wl";

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "x = 1\n",
        "ScheduledJobs" -> {Function[{entry}, {{}, False}]}
      |>|>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <||>
    },
      result = LSPServer`expandContent[
        <|
          "method" -> "textDocument/semanticTokens/full",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>,
        {0}
      ];

      {
        result,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {
    {
      <|"method" -> "textDocument/concreteParse", "id" -> 42, "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/test_semantic_tokens_wait_for_reindex.wl"|>|>|>,
      <|"method" -> "textDocument/aggregateParse", "id" -> 42, "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/test_semantic_tokens_wait_for_reindex.wl"|>|>|>,
      <|"method" -> "textDocument/abstractParse", "id" -> 42, "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/test_semantic_tokens_wait_for_reindex.wl"|>|>|>,
      <|"method" -> "textDocument/semanticTokens/fullFencepost", "id" -> 42, "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/test_semantic_tokens_wait_for_reindex.wl"|>|>|>
    },
    {42}
  },
  TestID -> "SemanticTokens-Changed-Open-Request-Builds-Parse-Pipeline"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, result},
    uri = "file:///tmp/test_semantic_tokens_open_entry_expand_pending_index_ready.wl";
    text = "x = 1\ny[z_] := z + 1\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> text,
        "CST" -> cst,
        "Agg" -> agg,
        "AST" -> ast,
        "IndexUpdatePending" -> True,
        "ScheduledJobs" -> {Function[{entry}, {{}, False}]}
      |>|>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <||>
    },
      result = LSPServer`expandContent[
        <|
          "method" -> "textDocument/semanticTokens/full",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>,
        {0}
      ];

      {
        result,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {
    {<|"method" -> "textDocument/semanticTokens/fullFencepost", "id" -> 42, "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/test_semantic_tokens_open_entry_expand_pending_index_ready.wl"|>|>|>},
    {42}
  },
  TestID -> "SemanticTokens-Ready-Entry-Queues-Fencepost-Even-When-IndexUpdatePending"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, result},
    uri = "file:///tmp/test_semantic_tokens_open_entry_expand.wl";
    text = "x = 1\ny[z_] := z + 1\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text, "CST" -> cst, "Agg" -> agg, "AST" -> ast|>|>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <||>
    },
      result = LSPServer`expandContent[
        <|
          "method" -> "textDocument/semanticTokens/full",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>,
        {0}
      ];

      {
        result,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {
    {<|"method" -> "textDocument/semanticTokens/fullFencepost", "id" -> 42, "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/test_semantic_tokens_open_entry_expand.wl"|>|>|>},
    {42}
  },
  TestID -> "SemanticTokens-Open-Entry-Queues-Only-Fencepost"
]


VerificationTest[
  Module[{uri},
    uri = "file:///tmp/test_semantic_tokens_queue_priority.wl";

    Block[{
      LSPServer`$ContentQueue = {
        <|"method" -> "textDocument/runFastDiagnostics"|>,
        <|"method" -> "textDocument/documentSymbolFencepost"|>
      },
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> "x = 1\n", "CST" -> {1}, "AST" -> {1}|>|>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <||>
    },
      LSPServer`expandContentsAndAppendToContentQueue[{
        <|
          "method" -> "textDocument/semanticTokens/full",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      }];

      Take[
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        3
      ]
    ]
  ],
  {
    "textDocument/semanticTokens/fullFencepost",
    "textDocument/runFastDiagnostics",
    "textDocument/documentSymbolFencepost"
  },
  TestID -> "SemanticTokens-Fencepost-Prioritized-When-Queued"
]


VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/test_semantic_tokens_waiting_fencepost_reindex.wl";

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "x = 1\n",
        "ScheduledJobs" -> {Function[{entry}, {{}, False}]}
      |>|>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>
    },
      result = LSPServer`handleContent[
        <|
          "method" -> "textDocument/semanticTokens/fullFencepost",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      ];

      {
        result,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {{}, {42}},
  TestID -> "SemanticTokens-Fencepost-Waits-For-Reindex"
]


VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/test_closed_semantic_tokens_expand.wl";

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <||>
    },
      result = LSPServer`expandContent[
        <|
          "method" -> "textDocument/semanticTokens/full",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>,
        {0}
      ];

      {
        result,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {
    {<|
      "method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> 42,
      "params" -> <|"textDocument" -> <|"uri" -> "file:///tmp/test_closed_semantic_tokens_expand.wl"|>|>,
      "closed" -> True
    |>},
    Missing["NotFound"]
  },
  TestID -> "SemanticTokens-Closed-Request-ShortCircuits"
]


VerificationTest[
  Module[{uri},
    uri = "file:///tmp/test_recovered_semantic_tokens_queue_priority.wl";

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$ContentQueue = {
        <|"method" -> "textDocument/runFastDiagnostics"|>,
        <|"method" -> "textDocument/documentSymbolFencepost"|>
      },
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>
    },
      LSPServer`Private`queuePendingSemanticTokenFenceposts[uri];

      {
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        Lookup[Last[LSPServer`$ContentQueue], "priority", Missing["NotFound"]],
        Lookup[Last[LSPServer`$ContentQueue], "deferrable", Missing["NotFound"]]
      }
    ]
  ],
  {
    {
      "textDocument/runFastDiagnostics",
      "textDocument/documentSymbolFencepost",
      "textDocument/semanticTokens/fullFencepost"
    },
    False,
    True
  },
  TestID -> "SemanticTokens-Recovered-Fencepost-Appends-As-Deferrable"
]


VerificationTest[
  Module[{uri},
    uri = "file:///tmp/test_recovered_semantic_tokens_fold_priority.wl";

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$ContentQueue = {
        <|"method" -> "textDocument/concreteParse", "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>,
        <|"method" -> "textDocument/aggregateParse", "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>,
        <|"method" -> "textDocument/abstractParse", "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>,
        <|"method" -> "textDocument/documentNodeList", "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>,
        <|"method" -> "textDocument/foldingRangeFencepost", "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>
      },
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {99}|>
    },
      LSPServer`Private`queuePendingSemanticTokenFenceposts[uri];

      {
        Lookup[First[LSPServer`$ContentQueue], "method", Missing["NotFound"]],
        Lookup[Last[LSPServer`$ContentQueue], "method", Missing["NotFound"]],
        Lookup[Last[LSPServer`$ContentQueue], "priority", Missing["NotFound"]],
        Lookup[Last[LSPServer`$ContentQueue], "deferrable", Missing["NotFound"]]
      }
    ]
  ],
  {
    "textDocument/concreteParse",
    "textDocument/semanticTokens/fullFencepost",
    False,
    True
  },
  TestID -> "SemanticTokens-Recovered-Fencepost-Defers-To-Folding-Pipeline"
]


VerificationTest[
  Module[{uri},
    uri = "file:///tmp/test_recovered_semantic_tokens_later_folding_pipeline.wl";

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$ContentQueue = {
        <|"method" -> "textDocument/runFastDiagnostics"|>
      },
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {99}|>
    },
      LSPServer`Private`queuePendingSemanticTokenFenceposts[uri];

      LSPServer`expandContentsAndAppendToContentQueue[{
        <|
          "method" -> "textDocument/foldingRange",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      }];

      Table[
        Lookup[LSPServer`Private`takeFirstContentQueueItem[], "method", Missing["NotFound"]],
        7
      ]
    ]
  ],
  {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/abstractParse",
    "textDocument/documentNodeList",
    "textDocument/foldingRangeFencepost",
    "textDocument/runFastDiagnostics",
    "textDocument/semanticTokens/fullFencepost"
  },
  TestID -> "SemanticTokens-Recovered-Fencepost-Does-Not-Block-Later-Folding-Pipeline"
]


VerificationTest[
  Module[{uri, result, readStringCalled = False},
    uri = "file:///tmp/test_closed_semantic_tokens_fencepost.wl";

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      ReadString = Function[{args}, readStringCalled = True; "x = 1\n"],
      FileExistsQ = Function[{path}, True]
    },
      result = LSPServer`handleContent[
        <|
          "method" -> "textDocument/semanticTokens/fullFencepost",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      ];

      {
        result,
        readStringCalled,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {
    {<|"jsonrpc" -> "2.0", "id" -> 42, "result" -> Null|>},
    False,
    Missing["NotFound"]
  },
  TestID -> "SemanticTokens-Closed-Fencepost-Avoids-Disk-Compute"
]


VerificationTest[
  Module[{uri, result, readStringCalled = False},
    uri = "file:///tmp/test_open_race_semantic_tokens_fencepost.wl";

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$PreExpandContentQueue = {
        <|
          "method" -> "textDocument/didOpenFencepost",
          "params" -> <|"textDocument" -> <|"uri" -> uri, "text" -> "x = 1\n"|>|>
        |>
      },
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,
      ReadString = Function[{args}, readStringCalled = True; "x = 1\n"]
    },
      result = LSPServer`handleContent[
        <|
          "method" -> "textDocument/semanticTokens/fullFencepost",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      ];

      {
        result,
        readStringCalled,
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {
    {},
    False,
    {42}
  },
  TestID -> "SemanticTokens-Open-Race-Fencepost-Waits-Instead-Of-Reading-Disk"
]


VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/test_latest_only_pending_semantic_tokens.wl";

    Block[{
      LSPServer`$ContentQueue = {
        <|
          "method" -> "textDocument/semanticTokens/fullFencepost",
          "id" -> 42,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      },
      LSPServer`$PreExpandContentQueue = {},
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "x = 1\n",
        "ScheduledJobs" -> {Function[{entry}, {{}, False}]}
      |>|>,
      LSPServer`$CancelMap = <||>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>
    },
      result = LSPServer`expandContent[
        <|
          "method" -> "textDocument/semanticTokens/full",
          "id" -> 43,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>,
        {0}
      ];

      {
        Cases[
          result,
          KeyValuePattern[{"method" -> "textDocument/semanticTokens/fullFencepost", "superseded" -> True, "id" -> oldID_}] :> oldID
        ],
        AnyTrue[
          result,
          AssociationQ[#] &&
          Lookup[#, "method", None] === "textDocument/semanticTokens/fullFencepost" &&
          TrueQ[Lookup[#, "superseded", False]] &&
          Lookup[#, "id", None] === 42 &
        ],
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]],
        Cases[
          LSPServer`$ContentQueue,
          KeyValuePattern[{"method" -> "textDocument/semanticTokens/fullFencepost", "id" -> oldID_}] :> oldID
        ]
      }
    ]
  ],
  {
    {42},
    True,
    {43},
    {}
  },
  TestID -> "SemanticTokens-Pending-Requests-Are-Latest-Only"
]


VerificationTest[
  Module[{uri, otherURI},
    uri = "file:///tmp/test_didclose_queue_cleanup.wl";
    otherURI = "file:///tmp/test_didclose_queue_cleanup_other.wl";

    Block[{
      LSPServer`$didCloseMethods = {"textDocument/publishDiagnostics"},
      LSPServer`$ContentQueue = {
        <|"method" -> "textDocument/publishDiagnostics", "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>,
        <|"method" -> "textDocument/concreteParse", "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>,
        <|"method" -> "textDocument/runScopingData", "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>,
        <|"method" -> "textDocument/concreteParse", "params" -> <|"textDocument" -> <|"uri" -> otherURI|>|>|>
      },
      LSPServer`$OpenFilesMap = <|uri -> <||>, otherURI -> <||>|>,
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}, otherURI -> {7}|>
    },
      LSPServer`handleContent[
        <|
          "method" -> "textDocument/didCloseFencepost",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      ];

      {
        Cases[
          LSPServer`$ContentQueue,
          KeyValuePattern[{
            "params" -> KeyValuePattern["textDocument" -> KeyValuePattern["uri" -> uri]],
            "method" -> method_
          }] :> method
        ],
        Cases[
          LSPServer`$ContentQueue,
          KeyValuePattern[{
            "params" -> KeyValuePattern["textDocument" -> KeyValuePattern["uri" -> otherURI]],
            "method" -> method_
          }] :> method
        ],
        KeyExistsQ[LSPServer`$OpenFilesMap, uri],
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]],
        Lookup[LSPServer`$PendingSemanticTokenRequests, otherURI, Missing["NotFound"]]
      }
    ]
  ],
  {
    {"textDocument/publishDiagnostics"},
    {"textDocument/concreteParse"},
    False,
    Missing["NotFound"],
    {7}
  },
  TestID -> "DidClose-Purges-Queued-URI-Work"
]


(* ================================================================
  DidChange reindex -> no pending ids
   Do not proactively queue workspace/semanticTokens/refresh from the
   didChange completion path. VS Code can turn that into a self-sustaining
   semantic-token refresh loop. Late semantic token requests are handled by
  the normal request -> fencepost path if and when they arrive.
   ================================================================ *)
VerificationTest[
  Module[{uri, cst, agg, ast, newText, entry, job},
    uri     = "file:///tmp/test_didchange_no_pending_refresh.wl";
    newText = "x = 2\n";

    cst = Quiet[CodeConcreteParse[newText, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$ContentQueue = {},
      LSPServer`$PendingSemanticTokenRequests = <||>,   (* no pending requests *)
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$didChangeScheduledJobs = {},
      LSPServer`PacletIndex`UpdateFileIndex = Function[{uriArg, textArg}, {cst, agg, ast}],
      LSPServer`Private`UpdateFileIndex = Function[{uriArg, textArg}, {cst, agg, ast}],
      LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics = Function[uriArg, Null]
    },
      LSPServer`handleContent[<|
        "method" -> "textDocument/didChangeFencepost",
        "params" -> <|
          "textDocument" -> <|"uri" -> uri|>,
          "contentChanges" -> {<|"text" -> newText|>}
        |>
      |>];

      entry = LSPServer`$OpenFilesMap[uri];
      entry["LastChange"] = Now - Quantity[1, "Seconds"];
      job = First[Lookup[entry, "ScheduledJobs", {}]];

      LSPServer`$OpenFilesMap[uri] = entry;

      If[!FunctionQ[job], Return[{"no-job", {}, {}}]];

      job[entry];

      {
        Count[
          Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
          "workspace/semanticTokens/refresh"
        ],
        Count[
          Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
          "textDocument/semanticTokens/fullFencepost"
        ],
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {0, 0, Missing["NotFound"]},
  TestID -> "DidChange-NoPendingTokens-DoesNotSendRefresh"
]


VerificationTest[
  Module[{uri, text, cst, agg, ast, parseCalled = False, aggCalled = False, astCalled = False},
    uri = "file:///tmp/test_runindexupdate_reuses_cached_parse_artifacts.wl";
    text = "f[x_] := x + 1\n";

    cst = Quiet[CodeConcreteParse[text, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$SemanticTokens = False,
      LSPServer`$ContentQueue = {},
      LSPServer`$ClosedFileDiagnosticsNotifications = <||>,
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> text,
        "LastChange" -> Now,
        "ScheduledJobs" -> {},
        "IndexUpdatePending" -> True,
        "CST" -> cst,
        "Agg" -> agg,
        "AST" -> ast,
        "PreviousAST" -> ast,
        "PreviousUserSymbols" -> {}
      |>|>,
      LSPServer`$TryQueueThunk = Function[Null],
      LSPServer`$WriteLSPResultThunk = Function[{contentsArg}, Null],
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <||>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False,
      LSPServer`PacletIndex`Private`structuredPackageMetadata = Function[{filePathArg, astArg}, <||>],
      LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics = (Null &),
      LSPServer`Private`openFilesAffectedByDefinitions = ({} &),
      LSPServer`Private`queueWorkspaceDiagnosticsSweep = (Null &),
      CodeParser`CodeConcreteParse = (parseCalled = True; Failure["ParseCalled", <||>]) &,
      CodeParser`Abstract`Aggregate = (aggCalled = True; Failure["AggregateCalled", <||>]) &,
      CodeParser`Abstract`Abstract = (astCalled = True; Failure["AbstractCalled", <||>]) &
    },
      LSPServer`handleContent[<|
        "method" -> "textDocument/runIndexUpdate",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];

      {
        parseCalled,
        aggCalled,
        astCalled,
        KeyExistsQ[LSPServer`$OpenFilesMap[uri], "IndexUpdatePending"],
        MatchQ[Lookup[LSPServer`PacletIndex`$PacletIndex["Files"], uri, Missing["NotFound"]], _Association],
        ListQ[Lookup[LSPServer`$OpenFilesMap[uri], "UserSymbols", Missing["NotFound"]]]
      }
    ]
  ],
  {False, False, False, False, True, True},
  TestID -> "RunIndexUpdate-Reuses-Cached-Parse-Artifacts"
]


(* ================================================================
  DidChange reindex -> pending ids exist -> fenceposts queued, no refresh
   If there ARE pending token request ids when the reindex job fires,
   queue fenceposts (existing behavior) and do NOT send refresh.
   ================================================================ *)
VerificationTest[
  Module[{uri, text, cst, agg, ast, newText, entry, job},
    uri     = "file:///tmp/test_didchange_pending_no_refresh.wl";
    newText = "x = 2\n";

    cst = Quiet[CodeConcreteParse[newText, "FileFormat" -> "Package"]];
    cst[[1]] = File;
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];

    Block[{
      LSPServer`$SemanticTokens = True,
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$ContentQueue = {},
      LSPServer`$PendingSemanticTokenRequests = <|uri -> {42}|>,  (* id=42 pending *)
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$didChangeScheduledJobs = {},
      LSPServer`PacletIndex`UpdateFileIndex = Function[{uriArg, textArg}, {cst, agg, ast}],
      LSPServer`Private`UpdateFileIndex = Function[{uriArg, textArg}, {cst, agg, ast}],
      LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics = Function[uriArg, Null]
    },
      LSPServer`handleContent[<|
        "method" -> "textDocument/didChangeFencepost",
        "params" -> <|
          "textDocument" -> <|"uri" -> uri|>,
          "contentChanges" -> {<|"text" -> newText|>}
        |>
      |>];

      entry = LSPServer`$OpenFilesMap[uri];
      entry["LastChange"] = Now - Quantity[1, "Seconds"];
      job = First[Lookup[entry, "ScheduledJobs", {}]];

      LSPServer`$OpenFilesMap[uri] = entry;

      If[!FunctionQ[job], Return[{"no-job", {}, {}}]];

      job[entry];

      (* The scheduled job queues "textDocument/runIndexUpdate" instead of
         running inline. Simulate processing it. *)
      AppendTo[LSPServer`$ContentQueue,
        <|"method" -> "textDocument/runIndexUpdate",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>];
      LSPServer`handleContent[Last[LSPServer`$ContentQueue]];

      {
        Count[
          Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
          "textDocument/semanticTokens/fullFencepost"
        ],
        Count[
          Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
          "workspace/semanticTokens/refresh"
        ],
        Lookup[LSPServer`$PendingSemanticTokenRequests, uri, Missing["NotFound"]]
      }
    ]
  ],
  {1, 0, {42}},
  TestID -> "DidChange-PendingTokens-Queues-Fencepost-Not-Refresh"
]


VerificationTest[
  Module[{uri, params, result},
    uri = "file:///tmp/test_missing_parse_artifacts_handlers.wl";
    params = <|"textDocument" -> <|"uri" -> uri|>|>;

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "x = 1\n",
        "LastChange" -> Now,
        "ScheduledJobs" -> {Function[{entry}, {{}, False}]}
      |>|>
    },
      result = {
        Check[LSPServer`handleContent[<|"method" -> "textDocument/suppressedRegions", "params" -> params|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/parseIgnoreComments", "params" -> params|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/runConcreteDiagnostics", "params" -> params|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/aggregateParse", "params" -> params|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/runAggregateDiagnostics", "params" -> params|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/abstractParse", "params" -> params|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/runAbstractDiagnostics", "params" -> params|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/runScopingDiagnostics", "params" -> params|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/runWorkspaceDiagnostics", "params" -> params|>], "MESSAGE"]
      };

      result
    ]
  ],
  {{}, {}, {}, {}, {}, {}, {}, {}, {}},
  TestID -> "Missing-Parse-Artifacts-Handlers-NoOp"
]


VerificationTest[
  Module[{uri, textDocumentParams, positionParams, rangeParams, selectionRangeParams, result},
    uri = "file:///tmp/test_missing_parse_artifacts_requests.wl";
    textDocumentParams = <|"textDocument" -> <|"uri" -> uri|>|>;
    positionParams = Join[textDocumentParams, <|"position" -> <|"line" -> 0, "character" -> 0|>|>];
    rangeParams = Join[
      textDocumentParams,
      <|"range" -> <|
        "start" -> <|"line" -> 0, "character" -> 0|>,
        "end" -> <|"line" -> 0, "character" -> 0|>
      |>|>
    ];
    selectionRangeParams = Join[textDocumentParams, <|"positions" -> {<|"line" -> 0, "character" -> 0|>}|>];

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "x = 1\n",
        "LastChange" -> Now,
        "ScheduledJobs" -> {Function[{entry}, {{}, False}]}
      |>|>
    },
      result = {
        Check[LSPServer`handleContent[<|"method" -> "textDocument/documentNodeList", "id" -> 1, "params" -> textDocumentParams|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/hoverFencepost", "id" -> 2, "params" -> positionParams|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/definitionFencepost", "id" -> 3, "params" -> positionParams|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/referencesFencepost", "id" -> 4, "params" -> positionParams|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/selectionRangeFencepost", "id" -> 5, "params" -> selectionRangeParams|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/documentColorFencepost", "id" -> 6, "params" -> textDocumentParams|>], "MESSAGE"],
        Check[LSPServer`handleContent[<|"method" -> "textDocument/codeActionFencepost", "id" -> 7, "params" -> rangeParams|>], "MESSAGE"]
      };

      result
    ]
  ],
  {
    {},
    {<|"jsonrpc" -> "2.0", "id" -> 2, "result" -> Null|>},
    {<|"jsonrpc" -> "2.0", "id" -> 3, "result" -> Null|>},
    {<|"jsonrpc" -> "2.0", "id" -> 4, "result" -> Null|>},
    {<|"jsonrpc" -> "2.0", "id" -> 5, "result" -> Null|>},
    {<|"jsonrpc" -> "2.0", "id" -> 6, "result" -> Null|>},
    {<|"jsonrpc" -> "2.0", "id" -> 7, "result" -> {}|>}
  },
  TestID -> "Missing-Parse-Artifacts-Request-Fenceposts-ShortCircuit"
]


VerificationTest[
  Module[{result},
    result = Quiet[
      Catch[
        Block[{
          LSPServer`StdIO`Private`TryQueue = Function[Null],
          LSPServer`ProcessScheduledJobs = Function[Null],
          LSPServer`Private`contentQueueEmptyQ = Function[False],
          LSPServer`Private`takeFirstContentQueueItem = Function[Throw[<|"method" -> "shared"|>, "queue"]],
          LSPServer`StdIO`Private`$ContentQueue = {<|"method" -> "shadow"|>},
          LSPServer`LSPEvaluate = Function[{content}, Throw[content, "queue"]],
          LSPServer`StdIO`Private`writeLSPResult = Function[Null],
          Pause = Function[Null]
        },
          LSPServer`readEvalWriteLoop["StdIO", None]
        ],
        "queue"
      ],
      {Part::partd, Rest::normal}
    ];

    result
  ],
  <|"method" -> "shared"|>,
  TestID -> "StdIO-Loop-Uses-Shared-ContentQueue"
]


VerificationTest[
  Module[{uri, params, result},
    uri = "file:///tmp/test_codeaction_ignore_warning.wl";
    params = <|
      "textDocument" -> <|"uri" -> uri|>,
      "range" -> <|
        "start" -> <|"line" -> 0, "character" -> 0|>,
        "end" -> <|"line" -> 0, "character" -> 5|>
      |>,
      "context" -> <|
        "diagnostics" -> {
          <|
            "code" -> "UndefinedSymbol\[VeryThinSpace]\:25bb\[VeryThinSpace]foo",
            "message" -> "Symbol \"foo\" is not defined in the paclet or System context",
            "severity" -> 2,
            "range" -> <|
              "start" -> <|"line" -> 0, "character" -> 0|>,
              "end" -> <|"line" -> 0, "character" -> 3|>
            |>,
            "source" -> "wolfram lint"
          |>
        }
      |>
    |>;

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "foo[]\n"
      |>|>
    },
      result = Check[
        LSPServer`handleContent[
          <|"method" -> "textDocument/codeActionFencepost", "id" -> 77, "params" -> params|>
        ],
        "MESSAGE"
      ];

      SortBy[
        ({
          #["title"],
          #["edit", "changes", uri][[1, "range", "start"]],
          #["edit", "changes", uri][[1, "newText"]]
        } &) /@ Lookup[First[result], "result", {}],
        First
      ]
    ]
  ],
  {
    {
      "Disable \"UndefinedSymbol\" for next line",
      <|"line" -> 0, "character" -> 0|>,
      "(* wl-disable-next-line UndefinedSymbol *)\n"
    },
    {
      "Disable \"UndefinedSymbol\" for this line",
      <|"line" -> 0, "character" -> 5|>,
      " (* wl-disable-line UndefinedSymbol *)"
    }
  },
  TestID -> "CodeAction-Ignore-QuickFixes-Use-Context-Diagnostics"
]


VerificationTest[
  Module[{result},
    result = LSPServer`handleContent[<|"method" -> "diagnostics", "id" -> 2|>];
    {
      MatchQ[result, {_Association}],
      Lookup[First[result], "id", Missing["NotFound"]],
      KeyExistsQ[Lookup[First[result], "result", <||>], "kernelVersion"],
      KeyExistsQ[Lookup[First[result], "result", <||>], "lspServerVersion"]
    }
  ],
  {True, 2, True, True},
  TestID -> "ServerDiagnostics-Dispatches-Through-Main-HandleContent"
]


VerificationTest[
  Module[{root, kernelDir, initPath, utilPath, result, publicDef, scopedDef, privateDef},
    root = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "lsp-structured-" <> StringDelete[CreateUUID[], "-"]}]];
    kernelDir = CreateDirectory[FileNameJoin[{root, "Kernel"}]];
    initPath = FileNameJoin[{kernelDir, "init.wl"}];
    utilPath = FileNameJoin[{kernelDir, "Utilities.wl"}];

    Export[
      initPath,
      "PackageInitialize[\"StructuredTest`\", <|\"HiddenImports\" -> {\"ExternalCtx`\"}|>]\n",
      "Text"
    ];
    Export[
      utilPath,
      StringRiffle[{
        "PackageImport[\"ImportedCtx`\", {importedFn}]",
        "PackageExported[publicFn]",
        "PackageScoped[sharedHelper]",
        "privateHelper[x_] := x",
        "sharedHelper[x_] := privateHelper[x]",
        "publicFn[x_] := sharedHelper[x]"
      }, "\n"],
      "Text"
    ];

    Block[{
      LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <||>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False,
      LSPServer`PacletIndex`Private`$WorkspaceCacheRoot = root,
      LSPServer`PacletIndex`Private`$StructuredPackageLoaderCache = <||>
    },
      result = LSPServer`PacletIndex`Private`indexFilePure[utilPath];
      publicDef = SelectFirst[result["Definitions"], #["name"] === "publicFn" && #["kind"] === "function" &];
      scopedDef = SelectFirst[result["Definitions"], #["name"] === "sharedHelper" && #["kind"] === "function" &];
      privateDef = SelectFirst[result["Definitions"], #["name"] === "privateHelper" && #["kind"] === "function" &];
      {
        result["PackageContext"],
        result["PackageScopeContext"],
        result["PrivateContext"],
        Sort[result["Dependencies"]],
        SortBy[
          ({Lookup[#, "method", ""], Lookup[#, "context", ""], Sort[Lookup[#, "symbols", {}]]} & /@ result["ContextLoads"]),
          Identity
        ],
        publicDef["context"],
        scopedDef["context"],
        privateDef["context"],
        scopedDef["visibility"],
        privateDef["visibility"]
      }
    ]
  ],
  {
    "StructuredTest`",
    "StructuredTest`PackageScope`",
    "StructuredTest`Utilities`Private`",
    {"ExternalCtx`", "ImportedCtx`"},
    {
      {"PackageImport", "ImportedCtx`", {"importedFn"}},
      {"PackageInitialize", "ExternalCtx`", {}}
    },
    "StructuredTest`",
    "StructuredTest`PackageScope`",
    "StructuredTest`Utilities`Private`",
    "package",
    "private"
  },
  TestID -> "PacletIndex-Structured-Package-Contexts-And-Hidden-Imports"
]


VerificationTest[
  Module[{root, kernelDir, initPath, importedPath, plainPath, importedResult, plainResult, importedURI, plainURI},
    root = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "lsp-structured-imports-" <> StringDelete[CreateUUID[], "-"]}]];
    kernelDir = CreateDirectory[FileNameJoin[{root, "Kernel"}]];
    initPath = FileNameJoin[{kernelDir, "init.wl"}];
    importedPath = FileNameJoin[{kernelDir, "Imported.wl"}];
    plainPath = FileNameJoin[{kernelDir, "Plain.wl"}];

    Export[
      initPath,
      "PackageInitialize[\"StructuredImportTest`\"]\n",
      "Text"
    ];
    Export[
      importedPath,
      StringRiffle[{
        "PackageImport[\"ImportedCtx`\", {importedFn}]",
        "PackageExported[useImported]",
        "useImported[] := importedFn[]"
      }, "\n"],
      "Text"
    ];
    Export[
      plainPath,
      StringRiffle[{
        "PackageExported[usePlain]",
        "usePlain[] := importedFn[]"
      }, "\n"],
      "Text"
    ];

    Block[{
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`PacletIndex`$WorkspaceRoot = root,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <||>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False,
      LSPServer`PacletIndex`Private`$WorkspaceCacheRoot = root,
      LSPServer`PacletIndex`Private`$StructuredPackageLoaderCache = <||>,
      LSPServer`PacletIndex`Private`$LoadedExternalDependencies = <||>,
      LSPServer`PacletIndex`GetKernelContextsCached = Function[{}, {"System`", "Global`"}],
      LSPServer`PacletIndex`Private`loadExternalDependencies = Function[{deps}, Null]
    },
      importedResult = LSPServer`PacletIndex`Private`indexFilePure[importedPath];
      plainResult = LSPServer`PacletIndex`Private`indexFilePure[plainPath];
      LSPServer`PacletIndex`Private`mergeIndexResult[importedResult];
      LSPServer`PacletIndex`Private`mergeIndexResult[plainResult];
      importedURI = importedResult["URI"];
      plainURI = plainResult["URI"];
      {
        MemberQ[LSPServer`PacletIndex`GetLoadedDependencySymbols[importedURI], "importedFn"],
        MemberQ[LSPServer`PacletIndex`GetLoadedDependencySymbols[plainURI], "importedFn"],
        MemberQ[LSPServer`PacletIndex`GetFileLoadedContexts[importedURI], "ImportedCtx`"],
        MemberQ[LSPServer`PacletIndex`GetFileBareNameContexts[plainURI], "ImportedCtx`"]
      }
    ]
  ],
  {True, False, True, False},
  TestID -> "PacletIndex-Structured-PackageImport-Stays-File-Local"
]


VerificationTest[
  Module[{root, kernelDir, providerDir, consumerDir, providerInitPath, consumerInitPath,
    providerPath, plainConsumerPath, importedConsumerPath, providerURI, plainConsumerURI,
    importedConsumerURI, makeEntry, plainDiags, importedDiags},
    root = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "lsp-structured-diag-" <> StringDelete[CreateUUID[], "-"]}]];
    kernelDir = CreateDirectory[FileNameJoin[{root, "Kernel"}]];
    providerDir = CreateDirectory[FileNameJoin[{kernelDir, "Provider"}]];
    consumerDir = CreateDirectory[FileNameJoin[{kernelDir, "Consumer"}]];
    providerInitPath = FileNameJoin[{providerDir, "init.wl"}];
    consumerInitPath = FileNameJoin[{consumerDir, "init.wl"}];
    providerPath = FileNameJoin[{providerDir, "API.wl"}];
    plainConsumerPath = FileNameJoin[{consumerDir, "UseProvider.wl"}];
    importedConsumerPath = FileNameJoin[{consumerDir, "UseProviderImported.wl"}];

    Export[
      providerInitPath,
      "PackageInitialize[\"ProviderPkg`\"]\n",
      "Text"
    ];
    Export[
      consumerInitPath,
      "PackageInitialize[\"ConsumerPkg`\"]\n",
      "Text"
    ];
    Export[
      providerPath,
      StringRiffle[{
        "PackageExported[providedFn]",
        "providedFn[] := 1"
      }, "\n"],
      "Text"
    ];
    Export[
      plainConsumerPath,
      StringRiffle[{
        "PackageExported[useProvider]",
        "useProvider[] := providedFn[]"
      }, "\n"],
      "Text"
    ];
    Export[
      importedConsumerPath,
      StringRiffle[{
        "PackageImport[\"ProviderPkg`\", {providedFn}]",
        "PackageExported[useProviderImported]",
        "useProviderImported[] := providedFn[]"
      }, "\n"],
      "Text"
    ];

    providerURI = "file://" <> providerPath;
    plainConsumerURI = "file://" <> plainConsumerPath;
    importedConsumerURI = "file://" <> importedConsumerPath;

    makeEntry[path_String] := Module[{text, cst, agg, ast, fileFormat},
      text = ReadString[path];
      fileFormat = LSPServer`SourceFileFormat[path];
      cst = Quiet[CodeConcreteParse[text, "FileFormat" -> fileFormat]];
      cst[[1]] = File;
      agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
      ast = Quiet[CodeParser`Abstract`Abstract[agg]];
      <|
        "Text" -> text,
        "CST" -> cst,
        "Agg" -> agg,
        "AST" -> ast,
        "ScopingData" -> {},
        "LastChange" -> Now
      |>
    ];

    Block[{
      LSPServer`$ConfidenceLevel = 0.50,
      LSPServer`$WorkspaceRootPath = root,
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`PacletIndex`$WorkspaceRoot = root,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <||>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False,
      LSPServer`PacletIndex`Private`$WorkspaceCacheRoot = root,
      LSPServer`PacletIndex`Private`$StructuredPackageLoaderCache = <||>,
      LSPServer`PacletIndex`Private`$LoadedExternalDependencies = <||>,
      LSPServer`PacletIndex`GetKernelContextsCached = Function[{}, {"System`", "Global`"}],
      LSPServer`PacletIndex`Private`loadExternalDependencies = Function[{deps}, Null]
    },
      LSPServer`PacletIndex`UpdateFileIndex[providerURI, ReadString[providerPath]];
      LSPServer`PacletIndex`UpdateFileIndex[plainConsumerURI, ReadString[plainConsumerPath]];
      LSPServer`PacletIndex`UpdateFileIndex[importedConsumerURI, ReadString[importedConsumerPath]];

      LSPServer`$OpenFilesMap = <|
        plainConsumerURI -> makeEntry[plainConsumerPath],
        importedConsumerURI -> makeEntry[importedConsumerPath]
      |>;

      LSPServer`handleContent[
        <|
          "method" -> "textDocument/runWorkspaceDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> plainConsumerURI|>|>
        |>
      ];
      LSPServer`handleContent[
        <|
          "method" -> "textDocument/runWorkspaceDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> importedConsumerURI|>|>
        |>
      ];

      plainDiags = LSPServer`handleContent[
        <|
          "method" -> "textDocument/publishDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> plainConsumerURI|>|>
        |>
      ][[1, "params", "diagnostics"]];
      importedDiags = LSPServer`handleContent[
        <|
          "method" -> "textDocument/publishDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> importedConsumerURI|>|>
        |>
      ][[1, "params", "diagnostics"]];

      {
        Select[
          plainDiags,
          StringStartsQ[Lookup[#, "code", ""], "UnimportedSymbol"] &&
            StringContainsQ[Lookup[#, "code", ""], "providedFn"] &&
            StringContainsQ[Lookup[#, "message", ""], "ProviderPkg`"] &
        ],
        Select[
          importedDiags,
          StringStartsQ[Lookup[#, "code", ""], "UnimportedSymbol"] &
        ],
        Select[
          Join[plainDiags, importedDiags],
          StringStartsQ[Lookup[#, "code", ""], "UndefinedSymbol"] &&
            AnyTrue[
              {"PackageInitialize", "PackageExported", "PackageScoped", "PackageImport"},
              StringContainsQ[Lookup[#, "code", ""], #] &
            ] &
        ]
      }
    ]
  ],
  {{_Association}, {}, {}},
  SameTest -> MatchQ,
  TestID -> "StructuredPackage-Diagnostics-Warn-Only-When-External-Symbol-Is-Unimported"
]


VerificationTest[
  Module[{root, kernelDir, initPath, firstPath, secondPath, firstResult, secondResult, firstURI, secondURI},
    root = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "lsp-structured-context-choice-" <> StringDelete[CreateUUID[], "-"]}]];
    kernelDir = CreateDirectory[FileNameJoin[{root, "Kernel"}]];
    initPath = FileNameJoin[{kernelDir, "init.wl"}];
    firstPath = FileNameJoin[{kernelDir, "First.wl"}];
    secondPath = FileNameJoin[{kernelDir, "Second.wl"}];

    Export[
      initPath,
      "PackageInitialize[\"ContextChoicePkg`\"]\n",
      "Text"
    ];
    Export[
      firstPath,
      StringRiffle[{
        "PackageExported[firstPublic]",
        "privateHelper[] := 1",
        "firstPublic[] := privateHelper[]"
      }, "\n"],
      "Text"
    ];
    Export[
      secondPath,
      StringRiffle[{
        "PackageExported[secondPublic]",
        "privateHelper[] := 2",
        "secondPublic[] := privateHelper[]"
      }, "\n"],
      "Text"
    ];

    Block[{
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`PacletIndex`$WorkspaceRoot = root,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <||>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False,
      LSPServer`PacletIndex`Private`$WorkspaceCacheRoot = root,
      LSPServer`PacletIndex`Private`$StructuredPackageLoaderCache = <||>,
      LSPServer`PacletIndex`Private`$LoadedExternalDependencies = <||>,
      LSPServer`PacletIndex`GetKernelContextsCached = Function[{}, {"System`", "Global`"}],
      LSPServer`PacletIndex`Private`loadExternalDependencies = Function[{deps}, Null]
    },
      firstResult = LSPServer`PacletIndex`Private`indexFilePure[firstPath];
      secondResult = LSPServer`PacletIndex`Private`indexFilePure[secondPath];
      LSPServer`PacletIndex`Private`mergeIndexResult[firstResult];
      LSPServer`PacletIndex`Private`mergeIndexResult[secondResult];
      firstURI = firstResult["URI"];
      secondURI = secondResult["URI"];

      {
        LSPServer`PacletIndex`GetSymbolContext[firstURI, "privateHelper"] === "ContextChoicePkg`First`Private`",
        LSPServer`PacletIndex`GetSymbolContext[secondURI, "privateHelper"] === "ContextChoicePkg`Second`Private`",
        DeleteDuplicates[Lookup[LSPServer`PacletIndex`GetVisibleSymbolDefinitions[firstURI, "privateHelper"], "uri", {}]] === {firstURI},
        DeleteDuplicates[Lookup[LSPServer`PacletIndex`GetVisibleSymbolDefinitions[secondURI, "privateHelper"], "uri", {}]] === {secondURI}
      }
    ]
  ],
  {True, True, True, True},
  TestID -> "StructuredPackage-FileDerived-Private-Context-Is-Used-For-Symbol-Resolution"
]


VerificationTest[
  Module[{root, kernelDir, initPath, apiPath, uri, modTime, staleCacheData, result, publicDef},
    root = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "lsp-structured-cache-version-" <> StringDelete[CreateUUID[], "-"]}]];
    kernelDir = CreateDirectory[FileNameJoin[{root, "Kernel"}]];
    initPath = FileNameJoin[{kernelDir, "init.wl"}];
    apiPath = FileNameJoin[{kernelDir, "API.wl"}];
    uri = "file://" <> apiPath;

    Export[
      initPath,
      "PackageInitialize[\"CacheInvalidatePkg`\"]\n",
      "Text"
    ];
    Export[
      apiPath,
      StringRiffle[{
        "PackageExported[publicFn]",
        "privateHelper[] := 1",
        "publicFn[] := privateHelper[]"
      }, "\n"],
      "Text"
    ];

    modTime = Quiet[AbsoluteTime[FileDate[apiPath, "Modification"]]];
    staleCacheData = <|
      "Definitions" -> {
        <|
          "name" -> "publicFn",
          "uri" -> uri,
          "source" -> {{1, 1}, {1, 10}},
          "kind" -> "function",
          "context" -> None,
          "visibility" -> "private"
        |>
      },
      "Usages" -> {},
      "Dependencies" -> {},
      "ContextLoads" -> {},
      "ExplicitContextRefs" -> {},
      "ContextAliases" -> {}
    |>;

    Block[{
      LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <|
        uri -> <|
          "ModTime" -> modTime,
          "Data" -> staleCacheData
        |>
      |>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False,
      LSPServer`PacletIndex`Private`$WorkspaceCacheRoot = root,
      LSPServer`PacletIndex`Private`$StructuredPackageLoaderCache = <||>
    },
      result = LSPServer`PacletIndex`Private`indexFilePure[apiPath];
      publicDef = SelectFirst[result["Definitions"], # ["name"] === "publicFn" &];
      {
        TrueQ[result["FromCache"]],
        result["PackageContext"],
        result["PrivateContext"],
        Lookup[publicDef, "context", Missing["NotFound"]]
      }
    ]
  ],
  {False, "CacheInvalidatePkg`", "CacheInvalidatePkg`API`Private`", "CacheInvalidatePkg`"},
  TestID -> "WorkspaceCache-Missing-Schema-Version-Forces-SPF-Reparse"
]


VerificationTest[
  Module[{root, kernelDir, sourceDir, loaderPath, sourcePath, result, scopedDef, privateDef},
    root = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "lsp-structured-postfix-" <> StringDelete[CreateUUID[], "-"]}]];
    kernelDir = CreateDirectory[FileNameJoin[{root, "Kernel"}]];
    sourceDir = CreateDirectory[FileNameJoin[{kernelDir, "Source"}]];
    loaderPath = FileNameJoin[{kernelDir, "RAGTool.wl"}];
    sourcePath = FileNameJoin[{sourceDir, "CustomerStories.wl"}];

    Export[
      loaderPath,
      "PackageInitialize[\"RAGTool`\"];\n",
      "Text"
    ];
    Export[
      sourcePath,
      StringRiffle[{
        "PackageImport[\"ToneAr`ParseXML`\"];",
        "",
        "scrapeCustomerStoryData // PackageScoped;",
        "scrapeCustomerStoryData[] := getURLs[]",
        "",
        "getURLs[] := ParseXML[]"
      }, "\n"],
      "Text"
    ];

    Block[{
      LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <||>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False,
      LSPServer`PacletIndex`Private`$WorkspaceCacheRoot = root,
      LSPServer`PacletIndex`Private`$StructuredPackageLoaderCache = <||>
    },
      result = LSPServer`PacletIndex`Private`indexFilePure[sourcePath];
      scopedDef = SelectFirst[result["Definitions"], #["name"] === "scrapeCustomerStoryData" && #["kind"] === "function" &];
      privateDef = SelectFirst[result["Definitions"], #["name"] === "getURLs" && #["kind"] === "function" &];
      {
        result["PackageContext"],
        result["PackageScopeContext"],
        result["PrivateContext"],
        Sort[result["Dependencies"]],
        SortBy[
          ({Lookup[#, "method", ""], Lookup[#, "context", ""], Sort[Lookup[#, "symbols", {}]]} & /@ result["ContextLoads"]),
          Identity
        ],
        Lookup[scopedDef, "context", Missing["NotFound"]],
        Lookup[scopedDef, "visibility", Missing["NotFound"]],
        Lookup[privateDef, "context", Missing["NotFound"]],
        Lookup[privateDef, "visibility", Missing["NotFound"]]
      }
    ]
  ],
  {
    "RAGTool`",
    "RAGTool`PackageScope`",
    "RAGTool`Source`CustomerStories`Private`",
    {"ToneAr`ParseXML`"},
    {{"PackageImport", "ToneAr`ParseXML`", {}}},
    "RAGTool`PackageScope`",
    "package",
    "RAGTool`Source`CustomerStories`Private`",
    "private"
  },
  TestID -> "StructuredPackage-Postfix-And-Semicolon-Forms-Are-Discovered"
]


VerificationTest[
  Block[{
    LSPServer`PacletIndex`IsWorkspaceSymbol = Function[{name}, name === "crossFileFunc"]
  },
    LSPServer`SemanticTokens`Private`classifyGlobalSymbol["crossFileFunc"]
  ],
  {"class", {"definition"}},
  TestID -> "SemanticTokens-Workspace-Symbol-Classification-Uses-PacletIndex"
]


VerificationTest[
  Block[{
    LSPServer`PacletIndex`GetDependencyContexts = Function[{}, {"ToneAr`ParseXML`"}],
    LSPServer`PacletIndex`GetIndexedDependencySymbols = Function[{}, {"ParseXML"}],
    LSPServer`PacletIndex`GetContextAliases = Function[{}, <||>],
    LSPServer`SemanticTokens`Private`$depsCacheKey = None,
    LSPServer`SemanticTokens`Private`$depsSet = <||>,
    LSPServer`SemanticTokens`Private`$depsList = {},
    LSPServer`SemanticTokens`Private`$indexedDependencySymbolSet = <||>
  },
    LSPServer`SemanticTokens`Private`classifyGlobalSymbol["ParseXML"]
  ],
  {"class", {"definition", "defaultLibrary"}},
  TestID -> "SemanticTokens-Dependency-Symbol-Classification-Uses-PacletIndex"
]


VerificationTest[
  Module[{root, kernelDir, providerDir, consumerDir, providerPath, consumerPath,
    providerUri, consumerUri, providerText, consumerText, result, locations},
    root = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "lsp-structured-definition-" <> StringDelete[CreateUUID[], "-"]}]];
    kernelDir = CreateDirectory[FileNameJoin[{root, "Kernel"}]];
    providerDir = CreateDirectory[FileNameJoin[{kernelDir, "Provider"}]];
    consumerDir = CreateDirectory[FileNameJoin[{kernelDir, "Consumer"}]];

    Export[
      FileNameJoin[{providerDir, "init.wl"}],
      "PackageInitialize[\"ProviderPkg`\"]\n",
      "Text"
    ];
    Export[
      FileNameJoin[{consumerDir, "init.wl"}],
      "PackageInitialize[\"ConsumerPkg`\"]\n",
      "Text"
    ];

    providerPath = FileNameJoin[{providerDir, "API.wl"}];
    consumerPath = FileNameJoin[{consumerDir, "UseProviderImported.wl"}];

    providerText = StringRiffle[{
      "PackageExported[providedFn]",
      "providedFn[] := 1"
    }, "\n"];
    consumerText = StringRiffle[{
      "PackageImport[\"ProviderPkg`\", {providedFn}]",
      "PackageExported[useProviderImported]",
      "useProviderImported[] := providedFn[]"
    }, "\n"];

    Export[providerPath, providerText, "Text"];
    Export[consumerPath, consumerText, "Text"];

    providerUri = LocalObjects`PathToURI[providerPath];
    consumerUri = LocalObjects`PathToURI[consumerPath];

    Block[{
      LSPServer`$WorkspaceRootPath = root,
      LSPServer`PacletIndex`$WorkspaceRoot = root,
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <||>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False,
      LSPServer`PacletIndex`Private`$WorkspaceCacheRoot = root,
      LSPServer`PacletIndex`Private`$StructuredPackageLoaderCache = <||>,
      LSPServer`PacletIndex`Private`$LoadedExternalDependencies = <||>,
      LSPServer`PacletIndex`GetKernelContextsCached = Function[{}, {"System`", "Global`"}],
      LSPServer`PacletIndex`Private`loadExternalDependencies = Function[{deps}, Null]
    },
      LSPServer`handleContent[
        <|
          "method" -> "textDocument/didOpenFencepost",
          "params" -> <|"textDocument" -> <|"uri" -> providerUri, "text" -> providerText|>|>
        |>
      ];
      LSPServer`handleContent[
        <|
          "method" -> "textDocument/runOpenIndexUpdate",
          "params" -> <|"textDocument" -> <|"uri" -> providerUri|>|>
        |>
      ];
      LSPServer`handleContent[
        <|
          "method" -> "textDocument/didOpenFencepost",
          "params" -> <|"textDocument" -> <|"uri" -> consumerUri, "text" -> consumerText|>|>
        |>
      ];
      LSPServer`handleContent[
        <|
          "method" -> "textDocument/runOpenIndexUpdate",
          "params" -> <|"textDocument" -> <|"uri" -> consumerUri|>|>
        |>
      ];

      result = Check[
        LSPServer`handleContent[
          <|
            "method" -> "textDocument/definitionFencepost",
            "id" -> 1,
            "params" -> <|
              "textDocument" -> <|"uri" -> consumerUri|>,
              "position" -> <|"line" -> 2, "character" -> 26|>
            |>
          |>
        ],
        "MESSAGE"
      ];

      locations = If[result === "MESSAGE",
        {},
        Lookup[First[result], "result", {}]
      ];

      {
        result =!= "MESSAGE",
        AnyTrue[locations, Lookup[#, "uri", None] === providerUri &],
        AllTrue[locations, StringQ[Lookup[#, "uri", None]] &]
      }
    ]
  ],
  {True, True, True},
  TestID -> "StructuredPackage-DefinitionFencepost-Returns-Concrete-Locations"
]


VerificationTest[
  Module[{uri, text, validUri, result, locations},
    uri = "file:///tmp/test_definition_malformed_location.wl";
    text = "use[] := target[]\n";
    validUri = "file:///tmp/TargetDefinition.wl";

    Block[{
      LSPServer`$WorkspaceRootPath = "/tmp",
      LSPServer`$ContentQueue = {},
      LSPServer`$CancelMap = <||>,
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`PacletIndex`GetVisibleSymbolDefinitions = Function[{args}, {
        <|"uri" -> Missing["NotAvailable"], "source" -> Missing["NotAvailable"]|>,
        <|"uri" -> validUri, "source" -> {{1, 1}, {1, 7}}|>
      }]
    },
      LSPServer`handleContent[
        <|
          "method" -> "textDocument/didOpenFencepost",
          "params" -> <|"textDocument" -> <|"uri" -> uri, "text" -> text|>|>
        |>
      ];
      LSPServer`handleContent[
        <|
          "method" -> "textDocument/runOpenIndexUpdate",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      ];

      result = Check[
        LSPServer`handleContent[
          <|
            "method" -> "textDocument/definitionFencepost",
            "id" -> 1,
            "params" -> <|
              "textDocument" -> <|"uri" -> uri|>,
              "position" -> <|"line" -> 0, "character" -> 9|>
            |>
          |>
        ],
        "MESSAGE"
      ];

      locations = If[result === "MESSAGE",
        {},
        Lookup[First[result], "result", {}]
      ];

      {
        result =!= "MESSAGE",
        locations,
        Count[locations, KeyValuePattern["uri" -> validUri]]
      }
    ]
  ],
  {
    True,
    {
      <|
        "uri" -> "file:///tmp/TargetDefinition.wl",
        "range" -> <|
          "start" -> <|"line" -> 0, "character" -> 0|>,
          "end" -> <|"line" -> 0, "character" -> 6|>
        |>
      |>
    },
    1
  },
  TestID -> "DefinitionFencepost-Skips-Malformed-Paclet-Locations"
]


VerificationTest[
  Module[{root, kernelDir, apiPath, result, def},
    root = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "lsp-return-inference-local-bindings-" <> StringDelete[CreateUUID[], "-"]}]];
    kernelDir = CreateDirectory[FileNameJoin[{root, "Kernel"}]];
    apiPath = FileNameJoin[{kernelDir, "ReturnInference.wl"}];

    Export[
      apiPath,
      StringRiffle[{
        "makeSources[] := Block[{sources},",
        "  sources = Map[StringLength, {\"a\", \"bb\"}];",
        "  Join @@ sources",
        "]"
      }, "\n"],
      "Text"
    ];

    Block[{
      LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <||>,
      LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False,
      LSPServer`PacletIndex`Private`$WorkspaceCacheRoot = root,
      LSPServer`PacletIndex`Private`$StructuredPackageLoaderCache = <||>
    },
      result = LSPServer`PacletIndex`Private`indexFilePure[apiPath];
      def = SelectFirst[result["Definitions"], # ["name"] === "makeSources" && # ["kind"] === "function" &];
      Quiet[ToString[Lookup[def, "InferredReturnPattern", None], InputForm], {ToString::shdw}]
    ]
  ],
  "_List",
  TestID -> "InferredReturnPattern-Block-Local-Binding-Apply-Join"
]


VerificationTest[
  Block[{
    LSPServer`PacletIndex`Private`$WorkspaceSymbolSearchEntries = {
      <|"name" -> "alphaFn", "kind" -> "function", "location" -> <||>, "containerName" -> "Pkg`"|>,
      <|"name" -> "betaValue", "kind" -> "constant", "location" -> <||>, "containerName" -> "Pkg`"|>
    }
  },
    {
      LSPServer`PacletIndex`GetAllWorkspaceSymbols[],
      LSPServer`PacletIndex`SearchWorkspaceSymbols["alpha"],
      LSPServer`PacletIndex`SearchWorkspaceSymbols[""]
    }
  ],
  {
    {
      <|"name" -> "alphaFn", "kind" -> "function", "location" -> <||>, "containerName" -> "Pkg`"|>,
      <|"name" -> "betaValue", "kind" -> "constant", "location" -> <||>, "containerName" -> "Pkg`"|>
    },
    {
      <|"name" -> "alphaFn", "kind" -> "function", "location" -> <||>, "containerName" -> "Pkg`"|>
    },
    {
      <|"name" -> "alphaFn", "kind" -> "function", "location" -> <||>, "containerName" -> "Pkg`"|>,
      <|"name" -> "betaValue", "kind" -> "constant", "location" -> <||>, "containerName" -> "Pkg`"|>
    }
  },
  TestID -> "WorkspaceSymbols-Search-Uses-Cached-Entries"
]


VerificationTest[
  Module[{uri = "file:///tmp/WorkspaceSymbolCache.wl", defs},
    defs = {
      <|"name" -> "alphaFn", "uri" -> uri, "source" -> {{2, 1}, {2, 8}}, "kind" -> "function", "context" -> "Pkg`"|>,
      <|"name" -> "betaValue", "uri" -> uri, "source" -> {{4, 1}, {4, 10}}, "kind" -> "constant", "context" -> "Pkg`"|>
    };
    Block[{
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`PacletIndex`Private`$WorkspaceSymbolIndex = <||>,
      LSPServer`PacletIndex`Private`$WorkspaceSymbolSearchEntries = {}
    },
      LSPServer`PacletIndex`Private`addFileToIndex[
        uri,
        defs,
        {},
        {},
        {},
        {},
        {},
        "Pkg`",
        None,
        None,
        {}
      ];
      {
        Sort[Lookup[LSPServer`PacletIndex`SearchWorkspaceSymbols[""], "name", {}]],
        Lookup[First[LSPServer`PacletIndex`SearchWorkspaceSymbols["alpha"]], "containerName", Missing["NotFound"]]
      }
    ]
  ],
  {{"alphaFn", "betaValue"}, "Pkg`"},
  TestID -> "AddFileToIndex-Upserts-WorkspaceSymbol-Cache"
]
