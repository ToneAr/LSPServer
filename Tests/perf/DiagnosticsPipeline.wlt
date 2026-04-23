PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "..", "build", "paclet"}]]];
Needs["LSPServer`"];
LSPServer`LoadAllFeatureModules[];
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
    LSPServer`$OpenFilesMap = <||>;
    LSPServer`$OpenFilesMap[fakeURI] = <|
      "Text" -> "x = 1 + 1",
      "LastChange" -> Now
    |>;
    LSPServer`$DiagnosticsTask = None;
    LSPServer`$ContentQueue = {};
    LSPServer`$DiagnosticsKernel = $Failed;  (* disable slow tier for this test *)
    result = LSPServer`handleContent[<|
      "method" -> "textDocument/runFastDiagnostics",
      "params" -> <|"textDocument" -> <|"uri" -> fakeURI|>|>
    |>];
    AnyTrue[result, MatchQ[#, KeyValuePattern["method" -> "textDocument/publishDiagnostics"]]&]
  ],
  True,
  TestID -> "FastTierPublishesImmediately"
]

VerificationTest[
  Module[{fakeURI, result, diagnostics},
    fakeURI = "file:///tmp/testws/WorkspaceDiag.wl";

    Block[{
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$ContentQueue = {},
      LSPServer`$DiagnosticsKernel = $Failed,
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>
    },
      LSPServer`$OpenFilesMap[fakeURI] = <|
        "Text" -> "foo[]\n",
        "LastChange" -> Now
      |>;

      Block[{LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics = Function[uri, Null]},
        LSPServer`handleContent[<|
          "method" -> "textDocument/runFastDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> fakeURI|>|>
        |>]
      ];

      result = LSPServer`handleContent[<|
        "method" -> "textDocument/runWorkspaceDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> fakeURI|>|>
      |>];
      diagnostics = Lookup[Lookup[First[result], "params", <||>], "diagnostics", {}];

      {
        Lookup[First[result], "method", None],
        AnyTrue[diagnostics, StringStartsQ[Lookup[#, "code", ""], "UndefinedSymbol"] &]
      }
    ]
  ],
  {"textDocument/publishDiagnostics", True},
  TestID -> "WorkspaceDiagnosticsRepublishesMergedDiagnostics"
]

VerificationTest[
  Module[{fakeContent, result},
    fakeContent = <|
      "method" -> "textDocument/didChange",
      "params" -> <|
        "textDocument" -> <|"uri" -> "file:///test_didchange_fasttier.wl"|>,
        "contentChanges" -> {<|"text" -> "x = 1"|>}
      |>
    |>;
    result = LSPServer`expandContents[{fakeContent}];
    Lookup[result, "method", Missing["NotFound"]]
  ],
  {
    "textDocument/didChangeFencepost",
    "textDocument/runFastDiagnostics"
  },
  TestID -> "DidChangeExpandsToImmediateFastTierAndPublish"
]


VerificationTest[
  Module[{fakeContent, result},
    fakeContent = <|
      "method" -> "textDocument/didOpen",
      "params" -> <|
        "textDocument" -> <|
          "uri" -> "file:///test_didopen_deferred_fasttier.wl",
          "text" -> "x = 1"
        |>
      |>
    |>;
    result = LSPServer`expandContents[{fakeContent}];
    Lookup[result, "method", Missing["NotFound"]]
  ],
  {
    "textDocument/didOpenFencepost"
  },
  TestID -> "DidOpenExpandsToFencepostOnly"
]

VerificationTest[
  Module[{fakeContent},
    fakeContent = <|
      "method" -> "textDocument/didChange",
      "params" -> <|
        "textDocument" -> <|"uri" -> "file:///test_didchange_priority.wl"|>,
        "contentChanges" -> {<|"text" -> "x = 1"|>}
      |>
    |>;

    Block[{
      LSPServer`$ContentQueue = {
        <|"method" -> "textDocument/runWorkspaceDiagnostics"|>,
        <|"method" -> "textDocument/documentSymbolFencepost"|>
      }
    },
      LSPServer`expandContentsAndAppendToContentQueue[{fakeContent}];
      Take[Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]], 4]
    ]
  ],
  {
    "textDocument/didChangeFencepost",
    "textDocument/runFastDiagnostics",
    "textDocument/runWorkspaceDiagnostics",
    "textDocument/documentSymbolFencepost"
  },
  TestID -> "DidChangeImmediateDiagnosticsArePrioritized"
]

VerificationTest[
  Module[{fakeURI, result},
    fakeURI = "file:///test_fasttier_pending_semantic_tokens.wl";
    LSPServer`$SemanticTokens = True;
    LSPServer`$OpenFilesMap = <||>;
    LSPServer`$OpenFilesMap[fakeURI] = <|
      "Text" -> "x = 1\ny[z_] := z + 1\n",
      "LastChange" -> Now,
      "ScheduledJobs" -> {Function[{entry}, {{}, False}]},
      "IndexUpdatePending" -> True
    |>;
    LSPServer`$PendingSemanticTokenRequests = <|fakeURI -> {42}|>;
    LSPServer`$DiagnosticsTask = None;
    LSPServer`$ContentQueue = {};
    LSPServer`$DiagnosticsKernel = $Failed;
    result = Block[{LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics = Function[uri, Null]},
      LSPServer`handleContent[<|
        "method" -> "textDocument/runFastDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> fakeURI|>|>
      |>]
    ];
    {
      AnyTrue[result, MatchQ[#, KeyValuePattern["method" -> "textDocument/publishDiagnostics"]] &],
      Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
      Lookup[LSPServer`$PendingSemanticTokenRequests, fakeURI, Missing["NotFound"]],
      KeyExistsQ[LSPServer`$OpenFilesMap[fakeURI], "SemanticTokens"]
    }
  ],
  {False, {"textDocument/semanticTokens/fullFencepost"}, {42}, False},
  TestID -> "FastTierQueuesPendingSemanticTokenRequests"
]


VerificationTest[
  Module[{fakeURI, result},
    fakeURI = "file:///test_fasttier_parse_only_while_reindex_pending.wl";
    LSPServer`$SemanticTokens = False;
    LSPServer`$OpenFilesMap = <||>;
    LSPServer`$OpenFilesMap[fakeURI] = <|
      "Text" -> "x = 1\ny[z_] := z + 1\n",
      "LastChange" -> Now,
      "ScheduledJobs" -> {Function[{entry}, {{}, False}]},
      "IndexUpdatePending" -> True
    |>;
    LSPServer`$DiagnosticsTask = None;
    LSPServer`$ContentQueue = {};
    LSPServer`$DiagnosticsKernel = $Failed;
    result = Block[{LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics = Function[uri, Null]},
      LSPServer`handleContent[<|
        "method" -> "textDocument/runFastDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> fakeURI|>|>
      |>]
    ];
    {
      result,
      KeyExistsQ[LSPServer`$OpenFilesMap[fakeURI], "CST"],
      KeyExistsQ[LSPServer`$OpenFilesMap[fakeURI], "AST"],
      KeyExistsQ[LSPServer`$OpenFilesMap[fakeURI], "CSTLints"],
      Lookup[LSPServer`$OpenFilesMap[fakeURI], "WorkspaceLints", Missing["NotFound"]]
    }
  ],
  {{}, True, True, False, Null},
  TestID -> "FastTierParseOnlyWhileReindexPending"
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
    $OpenFilesMap = <|"file:///test.wl" -> <|"AST" -> HoldComplete[testAST]|>|>;
    LSPServer`Private`$IndexingWasActive = False;
    LSPServer`PacletIndex`$PendingIndexFiles = {};
    LSPServer`PacletIndex`Private`$PendingExternalDepFiles = {};
    snap = buildWorkerSnapshot["file:///test.wl"];
    KeyExistsQ[snap, "PacletIndex"] &&
    KeyExistsQ[snap, "BuiltinPatterns"] &&
    KeyExistsQ[snap, "WorkspaceRootPath"] &&
    KeyExistsQ[snap, "ConfidenceLevel"] &&
    KeyExistsQ[snap, "OpenFileEntry"] &&
    KeyExistsQ[snap, "IndexingWasActive"] &&
    KeyExistsQ[snap, "PendingIndexFiles"] &&
    KeyExistsQ[snap, "PendingExternalDepFiles"]
  ],
  True,
  TestID -> "BuildWorkerSnapshotHasRequiredKeys"
]

VerificationTest[
  Module[{closedURI, openURI, externalURI},
    closedURI = "file:///tmp/testws/Closed.wl";
    openURI = "file:///tmp/testws/Open.wl";
    externalURI = "file:///usr/share/External.wl";

    Block[{
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$OpenFilesMap = <|openURI -> <||>|>,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
      LSPServer`PacletIndex`$PacletIndex = <|
        "Files" -> <|closedURI -> <||>, openURI -> <||>, externalURI -> <||>|>
      |>
    },
      LSPServer`Private`queueWorkspaceDiagnosticsSweep[];
      LSPServer`$WorkspaceDiagnosticsSweepURIs
    ]
  ],
  {"file:///tmp/testws/Closed.wl"},
  TestID -> "WorkspaceDiagnosticsSweepQueuesClosedWorkspaceFilesOnly"
]

VerificationTest[
  Module[{workspaceRoot, indexed = {}},
    Internal`WithLocalSettings[
      workspaceRoot = CreateDirectory[];
      Export[FileNameJoin[{workspaceRoot, "Package.wl"}], "f[] := 1", "Text"];
      Export[FileNameJoin[{workspaceRoot, "Suite.wlt"}], "VerificationTest[1, 1]", "Text"];
      Export[FileNameJoin[{workspaceRoot, "Runner.mt"}], "Test[1, 1]", "Text"];
      CreateDirectory[FileNameJoin[{workspaceRoot, "nested"}]];
      Export[FileNameJoin[{workspaceRoot, "nested", "Nested.wlt"}], "VerificationTest[1, 1]", "Text"];
      Export[FileNameJoin[{workspaceRoot, "README.txt"}], "ignore", "Text"];
      ,
      Internal`InheritedBlock[{
        LSPServer`PacletIndex`Private`loadWorkspaceCache,
        LSPServer`PacletIndex`Private`loadExternalDependencies,
        LSPServer`PacletIndex`Private`saveWorkspaceCache,
        LSPServer`PacletIndex`Private`indexFile
      },
        LSPServer`PacletIndex`Private`loadWorkspaceCache[root_String] := (
          LSPServer`PacletIndex`Private`$WorkspaceIndexCache = <||>;
          LSPServer`PacletIndex`Private`$WorkspaceIndexCacheDirty = False;
          LSPServer`PacletIndex`Private`$WorkspaceCacheRoot = root
        );
        LSPServer`PacletIndex`Private`loadExternalDependencies[_] := Null;
        LSPServer`PacletIndex`Private`saveWorkspaceCache[] := Null;
        LSPServer`PacletIndex`Private`indexFile[filePath_String] := (
          AppendTo[indexed, FileNameTake[filePath]];
          LSPServer`PacletIndex`$PacletIndex["Files", "file://" <> filePath] = <||>
        );

        Block[{
          LSPServer`PacletIndex`$PacletIndex = <|
            "Symbols" -> <||>,
            "Files" -> <||>,
            "Contexts" -> <||>,
            "Dependencies" -> {},
            "ContextAliases" -> <||>
          |>,
          LSPServer`PacletIndex`$WorkspaceRoot = None,
          LSPServer`$PendingIndexFiles = {},
          LSPServer`$PendingReferenceFiles = {},
          LSPServer`$PendingExternalDepFiles = {},
          LSPServer`PacletIndex`$PendingIndexFiles = {},
          LSPServer`PacletIndex`$PendingReferenceFiles = {},
          LSPServer`PacletIndex`$PendingExternalDepFiles = {}
        },
          LSPServer`PacletIndex`InitializePacletIndex[workspaceRoot];
          {
            Sort[DeleteDuplicates[FileNameTake /@ Join[
              LSPServer`$PendingIndexFiles,
              LSPServer`PacletIndex`$PendingIndexFiles
            ]]],
            indexed
          }
        ]
      ],
      Quiet[DeleteDirectory[workspaceRoot, DeleteContents -> True]]
    ]
  ],
  {{"Nested.wlt", "Package.wl", "Runner.mt", "Suite.wlt"}, {}},
  TestID -> "InitializePacletIndexQueuesWLTAndMTFilesForBackgroundIndexing"
]

VerificationTest[
  Module[{uri},
    uri = "file:///tmp/testws/Closed.wl";

    Block[{
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$DiagnosticsTask = "fake-task",
      LSPServer`$DiagnosticsTaskURI = uri,
      LSPServer`$DiagnosticsTaskKind = "closed-file-sweep",
      LSPServer`$DiagnosticsTaskResult = <||>,
      LSPServer`$DiagnosticsKernel = $Failed,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {}
    },
      LSPServer`Private`cancelCurrentDiagnosticsTask[];
      {
        LSPServer`$DiagnosticsTask,
        LSPServer`$DiagnosticsTaskKind,
        LSPServer`$WorkspaceDiagnosticsSweepURIs
      }
    ]
  ],
  {None, None, {"file:///tmp/testws/Closed.wl"}},
  TestID -> "CancelCurrentDiagnosticsTaskRequeuesClosedFileSweep"
]

VerificationTest[
  Module[{uri, cachedNotification, result},
    uri = "file:///tmp/testws/Closed.wl";
    cachedNotification = <|
      "jsonrpc" -> "2.0",
      "method" -> "textDocument/publishDiagnostics",
      "params" -> <|
        "uri" -> uri,
        "diagnostics" -> {<|"code" -> "CachedDiag", "message" -> "cached"|>}
      |>
    |>;

    Block[{
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$ContentQueue = {},
      LSPServer`$ClosedFileDiagnosticsNotifications = <|uri -> cachedNotification|>
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/publishDiagnostics",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];
      result
    ]
  ],
  {
    <|
      "jsonrpc" -> "2.0",
      "method" -> "textDocument/publishDiagnostics",
      "params" -> <|
        "uri" -> "file:///tmp/testws/Closed.wl",
        "diagnostics" -> {<|"code" -> "CachedDiag", "message" -> "cached"|>}
      |>
    |>
  },
  TestID -> "PublishDiagnosticsFallsBackToClosedFileCache"
]

VerificationTest[
  Module[{fixturePath, uri, fileText, openResult, closedResult, openDiags, closedDiags},
    fixturePath = FileNameJoin[{DirectoryName[$TestFileName], "..", "hover", "DocCommentTest.wl"}];
    uri = LocalObjects`PathToURI[fixturePath];
    fileText = ReadString[fixturePath];

    Quiet[
      Block[{
        LSPServer`$WorkspaceRootPath = DirectoryName[fixturePath],
        LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
        LSPServer`$didCloseMethods = {"textDocument/publishDiagnostics"},
        LSPServer`$ClosedFileDiagnosticsNotifications = <||>,
        LSPServer`$ContentQueue = {},
        LSPServer`$OpenFilesMap = <||>,
        LSPServer`PacletIndex`$PacletIndex = <|
          "Symbols" -> <||>,
          "Files" -> <||>,
          "Contexts" -> <||>,
          "Dependencies" -> {},
          "ContextAliases" -> <||>
        |>
      },
        Scan[
          LSPServer`handleContent[<|
            "method" -> #,
            "params" -> <|
              "textDocument" -> <|
                "uri" -> uri,
                "languageId" -> "wolfram",
                "version" -> 1,
                "text" -> fileText
              |>
            |>
          |>] &,
          {
            "textDocument/didOpenFencepost",
            "textDocument/concreteParse",
            "textDocument/concreteTabsParse",
            "textDocument/aggregateParse",
            "textDocument/aggregateTabsParse",
            "textDocument/abstractParse"
          }
        ];

        LSPServer`PacletIndex`UpdateFileIndex[uri, fileText];
        LSPServer`handleContent[<|
          "method" -> "textDocument/runWorkspaceDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>];

        openResult = LSPServer`handleContent[<|
          "method" -> "textDocument/publishDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>];
        openDiags = Lookup[openResult[[1, "params"]], "diagnostics", {}];

        LSPServer`handleContent[<|
          "method" -> "textDocument/didCloseFencepost",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>];

        closedResult = LSPServer`handleContent[<|
          "method" -> "textDocument/publishDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>];
        closedDiags = Lookup[closedResult[[1, "params"]], "diagnostics", {}];

        {
          Length[openDiags] > 0,
          closedDiags === openDiags,
          KeyExistsQ[LSPServer`$OpenFilesMap, uri],
          MemberQ[LSPServer`$WorkspaceDiagnosticsSweepURIs, uri]
        }
      ],
      {EntityValue::conopen, EntityValue::nodat}
    ]
  ],
  {True, True, False, True},
  TestID -> "DidCloseSeedsClosedFileCacheAndQueuesSweep"
]

VerificationTest[
  Module[{uri},
    uri = "file:///tmp/testws/Closed.wl";

    Block[{
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$ContentQueue = {},
      LSPServer`$DiagnosticsKernel = $Failed
    },
      LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics[uri];
      Lookup[First[LSPServer`$ContentQueue], "method", Missing["NotFound"]]
    ]
  ],
  "textDocument/runClosedFileDiagnostics",
  TestID -> "DispatchClosedFileDiagnosticsQueuesSyncFallback"
]

VerificationTest[
  Module[{uri},
    uri = "file:///tmp/testws/BusyQueueClosed.wl";

    Block[{
      LSPServer`$ServerState = "running",
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {uri},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$ContentQueue = {<|"method" -> "textDocument/documentSymbolFencepost"|>},
      LSPServer`$QueueLastNonEmptyTime = AbsoluteTime[],
      LSPServer`$DiagnosticsKernel = "fake-kernel",
      LSPServer`$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 60,
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$DiagnosticsTaskStartTime = None,
      LSPServer`$IndexingWasActive = False,
      LSPServer`$PacletIndex = <|"Symbols" -> <||>, "Files" -> <||>, "Contexts" -> <||>, "Dependencies" -> {}, "ContextAliases" -> <||>|>,
      LSPServer`Diagnostics`Private`$BuiltinPatterns = <||>,
      LSPServer`$ConfidenceLevel = 0.50,
      LSPServer`PacletIndex`$PendingExternalDepFiles = {},
      LSPServer`PacletIndex`$PendingIndexFiles = {},
      LSPServer`PacletIndex`$PendingReferenceFiles = {},
      LSPServer`PacletIndex`ProcessPendingIndexFiles = Function[{}, False],
      LSPServer`Private`queueSemanticTokensRefresh = Function[args, Null],
      LSPServer`Private`launchDiagnosticsKernel = Function[{}, Null],
      ParallelSubmit = Function[{kernels, expr}, "fake-task"]
    },
      LSPServer`ProcessScheduledJobs[];
      {
        LSPServer`$DiagnosticsTask,
        LSPServer`$DiagnosticsTaskURI,
        LSPServer`$DiagnosticsTaskKind,
        LSPServer`$WorkspaceDiagnosticsSweepURIs
      }
    ]
  ],
  {None, None, None, {"file:///tmp/testws/BusyQueueClosed.wl"}},
  TestID -> "ProcessScheduledJobsDefersClosedSweepWhileQueueBusy"
]

VerificationTest[
  Module[{calls = 0},
    Block[{
      LSPServer`$ServerState = "running",
      LSPServer`$ContentQueue = {<|"method" -> "textDocument/documentSymbolFencepost"|>},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
      LSPServer`$DiagnosticsKernel = None,
      LSPServer`$DiagnosticsKernelLaunchAfter = None,
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$DiagnosticsTaskStartTime = None,
      LSPServer`$QueueLastNonEmptyTime = AbsoluteTime[],
      LSPServer`$IndexingWasActive = False,
      LSPServer`$WorkspaceIndexingQueued = False,
      LSPServer`PacletIndex`$PendingExternalDepFiles = {"/tmp/testws/External.wl"},
      LSPServer`PacletIndex`$PendingIndexFiles = {"/tmp/testws/Workspace.wl"},
      LSPServer`PacletIndex`$PendingReferenceFiles = {},
      LSPServer`PacletIndex`ProcessPendingIndexFiles = Function[{}, calls++; False],
      LSPServer`Private`queueSemanticTokensRefresh = Function[args, Null],
      LSPServer`Private`launchDiagnosticsKernel = Function[{}, Null]
    },
      LSPServer`ProcessScheduledJobs[];
      {
        calls,
        LSPServer`$IndexingWasActive,
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        LSPServer`$WorkspaceIndexingQueued
      }
    ]
  ],
  {0, True, {"textDocument/documentSymbolFencepost", "workspace/processIndexing"}, True},
  TestID -> "ProcessScheduledJobsQueuesCooperativeIndexingWhileQueueBusy"
]

VerificationTest[
  Module[{calls = 0},
    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$IndexingWasActive = True,
      LSPServer`$WorkspaceIndexingQueued = True,
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
      LSPServer`PacletIndex`$PendingExternalDepFiles = {"/tmp/testws/External.wl"},
      LSPServer`PacletIndex`$PendingIndexFiles = {"/tmp/testws/Workspace.wl"},
      LSPServer`PacletIndex`$PendingReferenceFiles = {},
      LSPServer`PacletIndex`Private`$PendingDepDiscovery = {},
      LSPServer`PacletIndex`ProcessPendingIndexFiles = Function[{}, calls++; True],
      LSPServer`Private`queueWorkspaceIndexing = Function[{reason},
        LSPServer`$WorkspaceIndexingQueued = True;
        AppendTo[LSPServer`$ContentQueue, <|"method" -> "workspace/processIndexing", "deferrable" -> True|>]
      ],
      LSPServer`Private`finishWorkspaceIndexing = Function[{}, AppendTo[LSPServer`$ContentQueue, <|"method" -> "finished"|>]]
    },
      {
        LSPServer`handleContent[<|"method" -> "workspace/processIndexing"|>],
        calls,
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        LSPServer`$WorkspaceIndexingQueued
      }
    ]
  ],
  {{}, 1, {"workspace/processIndexing"}, True},
  TestID -> "WorkspaceProcessIndexing-Requeues-Remaining-Work"
]

VerificationTest[
  Module[{uri},
    uri = "file:///tmp/testws/ClosedResult.wl";

    Block[{
      LSPServer`$ServerState = "running",
      LSPServer`$ContentQueue = {},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$DiagnosticsKernel = "fake-kernel",
      LSPServer`$DiagnosticsKernelLaunchAfter = None,
      LSPServer`$DiagnosticsTask = "fake-task",
      LSPServer`$DiagnosticsTaskURI = uri,
      LSPServer`$DiagnosticsTaskKind = "closed-file-sweep",
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$DiagnosticsTaskStartTime = None,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
      LSPServer`$IndexingWasActive = False,
      LSPServer`PacletIndex`$PendingExternalDepFiles = {},
      LSPServer`PacletIndex`$PendingIndexFiles = {},
      LSPServer`PacletIndex`$PendingReferenceFiles = {},
      LSPServer`PacletIndex`ProcessPendingIndexFiles = Function[{}, False],
      LSPServer`Private`queueSemanticTokensRefresh = Function[args, Null],
      TimeConstrained = Function[{expr, timeout, alt}, expr],
      WaitAll = Function[{task}, <|"URI" -> uri, "Notification" -> <||>|>]
    },
      LSPServer`ProcessScheduledJobs[];
      {
        LSPServer`$DiagnosticsTask,
        LSPServer`$DiagnosticsTaskURI,
        LSPServer`$DiagnosticsTaskKind,
        LSPServer`$DiagnosticsTaskResult,
        Lookup[First[LSPServer`$ContentQueue], "method", Missing["NotFound"]]
      }
    ]
  ],
  {None, None, None, <|"URI" -> "file:///tmp/testws/ClosedResult.wl", "Notification" -> <||>|>, "textDocument/publishClosedFileDiagnostics"},
  TestID -> "ProcessScheduledJobsPublishesCompletedClosedSweepTask"
]


VerificationTest[
  Module[{uri},
    uri = "file:///tmp/testws/HoverResult.wl";

    Block[{
      LSPServer`$ServerState = "running",
      LSPServer`$ContentQueue = {},
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> "Sin[x]\n", "AST" -> HoldComplete[Null], "LastChange" -> Now|>|>,
      LSPServer`$DiagnosticsKernel = "fake-kernel",
      LSPServer`$DiagnosticsKernelLaunchAfter = None,
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$DiagnosticsTaskStartTime = None,
      LSPServer`$HoverTask = "hover-task",
      LSPServer`$HoverTaskURI = uri,
      LSPServer`$HoverTaskID = 44,
      LSPServer`$HoverTaskResult = None,
      LSPServer`$HoverTaskStartTime = None,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
      LSPServer`$IndexingWasActive = False,
      LSPServer`$PendingExternalDepFiles = {},
      LSPServer`$PendingIndexFiles = {},
      LSPServer`$PendingReferenceFiles = {},
      LSPServer`PacletIndex`Private`$PendingDepDiscovery = {},
      LSPServer`Private`ProcessPendingIndexFiles = Function[{}, False],
      LSPServer`Private`queueSemanticTokensRefresh = Function[args, Null],
      TimeConstrained = Function[{expr, timeout, alt}, expr],
      WaitAll = Function[{task}, <|"URI" -> uri, "ID" -> 44, "Result" -> <|"contents" -> <|"kind" -> "markdown", "value" -> "hover"|>|>|>]
    },
      LSPServer`ProcessScheduledJobs[];
      {
        LSPServer`$HoverTask,
        LSPServer`$HoverTaskURI,
        LSPServer`$HoverTaskID,
        LSPServer`$HoverTaskResult,
        Lookup[First[LSPServer`$ContentQueue], "method", Missing["NotFound"]],
        Lookup[First[LSPServer`$ContentQueue], "id", Missing["NotFound"]]
      }
    ]
  ],
  {None, None, None, <|"URI" -> "file:///tmp/testws/HoverResult.wl", "ID" -> 44, "Result" -> <|"contents" -> <|"kind" -> "markdown", "value" -> "hover"|>|>|>, "textDocument/publishHoverResult", 44},
  TestID -> "ProcessScheduledJobsPublishesCompletedHoverTask"
]


VerificationTest[
  Module[{abortCalls = {}, closeCalls = {}, result},
    Block[{
      LSPServer`$CancelMap = <||>,
      LSPServer`$DiagnosticsKernel = "fake-kernel",
      LSPServer`$DiagnosticsKernelBin = "/tmp/fake/WolframKernel",
      LSPServer`$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 60,
      LSPServer`$DiagnosticsTask = "fake-task",
      LSPServer`$DiagnosticsTaskURI = "file:///tmp/testws/Open.wl",
      LSPServer`$DiagnosticsTaskKind = "open-file",
      LSPServer`$DiagnosticsTaskResult = <||>,
      LSPServer`$DiagnosticsTaskStartTime = AbsoluteTime[] - 1,
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$ServerState = "running",
      AbortKernels = Function[{kernel}, AppendTo[abortCalls, kernel]],
      CloseKernels = Function[{kernel}, AppendTo[closeCalls, kernel]]
    },
      result = LSPServer`handleContent[<|"method" -> "shutdown", "id" -> 7|>];
      {
        result,
        abortCalls,
        closeCalls,
        LSPServer`$DiagnosticsKernel,
        LSPServer`$DiagnosticsKernelBin,
        LSPServer`$DiagnosticsTask,
        LSPServer`$DiagnosticsTaskURI,
        LSPServer`$DiagnosticsTaskKind,
        LSPServer`$DiagnosticsTaskResult,
        LSPServer`$DiagnosticsTaskStartTime,
        LSPServer`$DiagnosticsKernelLaunchAfter,
        LSPServer`$ServerState
      }
    ]
  ],
  {
    {<|"jsonrpc" -> "2.0", "id" -> 7, "result" -> Null|>},
    {"fake-kernel"},
    {"fake-kernel"},
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    "shutdown"
  },
  TestID -> "ShutdownCleansUpDiagnosticsWorker"
]


VerificationTest[
  Module[{abortCalls = {}, closeCalls = {}, launchCount = 0, installDir},
    installDir = CreateDirectory[];
    Module[{addonsApps, kernelObjDir, kernelBin, result},
      addonsApps = FileNameJoin[{installDir, "AddOns", "Applications"}];
      kernelObjDir = FileNameJoin[{installDir, "SystemFiles", "Components", "KernelObjects", "Kernel"}];
      kernelBin = FileNameJoin[{installDir, "SystemFiles", "Kernel", "Binaries", $SystemID, "WolframKernel"}];
      Quiet[CreateDirectory[addonsApps, CreateIntermediateDirectories -> True]];
      Quiet[CreateDirectory[kernelObjDir, CreateIntermediateDirectories -> True]];
      Quiet[CreateDirectory[DirectoryName[kernelBin], CreateIntermediateDirectories -> True]];
      Quiet[Export[FileNameJoin[{kernelObjDir, "KernelObjectsStartup.wl"}], "", "Text"]];
      Quiet[Export[kernelBin, "", "Text"]];
      result = Quiet[
        Block[{
          LSPServer`$DiagnosticsKernel = "stale-kernel",
          LSPServer`$DiagnosticsKernelBin = "/tmp/stale/WolframKernel",
          LSPServer`$DiagnosticsTask = "stale-task",
          LSPServer`$DiagnosticsTaskURI = "file:///tmp/testws/Closed.wl",
          LSPServer`$DiagnosticsTaskKind = "closed-file-sweep",
          LSPServer`$DiagnosticsTaskResult = <||>,
          LSPServer`$DiagnosticsTaskStartTime = AbsoluteTime[] - 10,
          LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
          AbortKernels = Function[{kernel}, AppendTo[abortCalls, kernel]],
          CloseKernels = Function[{kernel}, AppendTo[closeCalls, kernel]],
          LaunchKernels = Function[{n}, launchCount++; {"new-kernel"}],
          ParallelEvaluate = Function[{expr, kernel}, Null, HoldAll],
          DistributeDefinitions = Function[args, Null, HoldAll],
          Get = Function[{path}, Null],
          Needs = Function[{ctx}, Null],
          $InstallationDirectory = installDir
        },
          LSPServer`Private`launchDiagnosticsKernel[];
          {
            launchCount,
            abortCalls,
            closeCalls,
            LSPServer`$DiagnosticsKernel,
            LSPServer`$DiagnosticsKernelBin,
            LSPServer`$DiagnosticsTask,
            LSPServer`$DiagnosticsTaskURI,
            LSPServer`$DiagnosticsTaskKind,
            LSPServer`$WorkspaceDiagnosticsSweepURIs
          }
        ],
        {$InstallationDirectory::strval}
      ];
      Quiet[DeleteDirectory[installDir, DeleteContents -> True]];
      result
    ]
  ],
  {
    1,
    {"stale-kernel"},
    {"stale-kernel"},
    "new-kernel",
    _String,
    None,
    None,
    None,
    {"file:///tmp/testws/Closed.wl"}
  },
  SameTest -> MatchQ,
  TestID -> "LaunchDiagnosticsKernelCleansUpExistingWorkerFirst"
]


VerificationTest[
  Module[{abortCalls = {}, closeCalls = {}, launchCount = 0, installDir},
    installDir = CreateDirectory[];
    Module[{addonsApps, kernelObjDir, kernelBin, result},
      addonsApps = FileNameJoin[{installDir, "AddOns", "Applications"}];
      kernelObjDir = FileNameJoin[{installDir, "SystemFiles", "Components", "KernelObjects", "Kernel"}];
      kernelBin = FileNameJoin[{installDir, "SystemFiles", "Kernel", "Binaries", $SystemID, "WolframKernel"}];
      Quiet[CreateDirectory[addonsApps, CreateIntermediateDirectories -> True]];
      Quiet[CreateDirectory[kernelObjDir, CreateIntermediateDirectories -> True]];
      Quiet[CreateDirectory[DirectoryName[kernelBin], CreateIntermediateDirectories -> True]];
      Quiet[Export[FileNameJoin[{kernelObjDir, "KernelObjectsStartup.wl"}], "", "Text"]];
      Quiet[Export[kernelBin, "", "Text"]];
      result = Quiet[
        Block[{
          LSPServer`$DiagnosticsKernel = None,
          LSPServer`$DiagnosticsKernelBin = None,
          LSPServer`$DiagnosticsTask = None,
          LSPServer`$DiagnosticsTaskURI = None,
          LSPServer`$DiagnosticsTaskKind = None,
          LSPServer`$DiagnosticsTaskResult = None,
          LSPServer`$DiagnosticsTaskStartTime = None,
          AbortKernels = Function[{kernel}, AppendTo[abortCalls, kernel]],
          CloseKernels = Function[{kernel}, AppendTo[closeCalls, kernel]],
          LaunchKernels = Function[{n}, launchCount++; {"new-kernel"}],
          ParallelEvaluate = Function[{expr, kernel}, $Failed, HoldAll],
          DistributeDefinitions = Function[args, Throw["should-not-run", "dist"]],
          Get = Function[{path}, Null],
          Needs = Function[{ctx}, Null],
          $InstallationDirectory = installDir
        },
          LSPServer`Private`launchDiagnosticsKernel[];
          {
            launchCount,
            abortCalls,
            closeCalls,
            LSPServer`$DiagnosticsKernel,
            LSPServer`$DiagnosticsKernelBin
          }
        ],
        {$InstallationDirectory::strval}
      ];
      Quiet[DeleteDirectory[installDir, DeleteContents -> True]];
      result
    ]
  ],
  {
    1,
    {"new-kernel"},
    {"new-kernel"},
    $Failed,
    $Failed
  },
  TestID -> "LaunchDiagnosticsKernelCleansUpFailedSetupKernel"
]

VerificationTest[
  Module[{uri, result},
    uri = "file:///tmp/testws/MergeOpen.wl";

    Block[{
      LSPServer`$OpenFilesMap = <|uri -> <|
        "Text" -> "foo[]\n",
        "CSTLints" -> {},
        "AggLints" -> {},
        "ASTLints" -> {},
        "ScopingLints" -> {},
        "WorkspaceLints" -> {
          CodeInspector`InspectionObject[
            "UndefinedSymbol",
            "bad symbol",
            "Remark",
            <|CodeParser`Source -> {{1, 1}, {1, 4}}, ConfidenceLevel -> 0.9, "Argument" -> "foo"|>
          ]
        },
        "IgnoreData" -> {}
      |>|>,
      LSPServer`$ContentQueue = {},
      LSPServer`$DiagnosticsTaskResult = <|
        "URI" -> uri,
        "WorkspaceLints" -> {
          CodeInspector`InspectionObject[
            "UndefinedSymbol",
            "bad symbol",
            "Remark",
            <|CodeParser`Source -> {{1, 1}, {1, 4}}, ConfidenceLevel -> 0.9, "Argument" -> "foo"|>
          ]
        }
      |>,
      LSPServer`$ConfidenceLevel = 0.50
    },
      result = LSPServer`handleContent[<|
        "method" -> "textDocument/mergeWorkspaceLints",
        "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
      |>];
      {
        Lookup[First[result], "method", Missing["NotFound"]],
        Length[Lookup[Lookup[First[result], "params", <||>], "diagnostics", {}]],
        ListQ[Lookup[LSPServer`$OpenFilesMap[uri], "WorkspaceLints", Null]],
        LSPServer`$DiagnosticsTaskResult
      }
    ]
  ],
  {"textDocument/publishDiagnostics", 1, True, None},
  TestID -> "MergeWorkspaceLintsPublishesMergedDiagnostics"
]

VerificationTest[
  Module[{badURI, goodURI, jobEvents = {}},
    badURI = "file:///tmp/testws/BadJob.wl";
    goodURI = "file:///tmp/testws/GoodJob.wl";

    Block[{
      LSPServer`$ServerState = "running",
      LSPServer`$ContentQueue = {},
      LSPServer`$OpenFilesMap = <|
        badURI -> <|
          "ScheduledJobs" -> {
            Function[{entry}, AppendTo[jobEvents, "bad"]; Throw["boom"]]
          }
        |>,
        goodURI -> <|
          "ScheduledJobs" -> {
            Function[{entry}, AppendTo[jobEvents, "good"]; {{}, True}]
          }
        |>
      |>,
      LSPServer`$DiagnosticsKernel = None,
      LSPServer`$DiagnosticsKernelLaunchAfter = None,
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
      LSPServer`$QueueLastNonEmptyTime = AbsoluteTime[],
      LSPServer`$IndexingWasActive = False,
      LSPServer`PacletIndex`$PendingExternalDepFiles = {},
      LSPServer`PacletIndex`$PendingIndexFiles = {},
      LSPServer`PacletIndex`$PendingReferenceFiles = {},
      LSPServer`PacletIndex`ProcessPendingIndexFiles = Function[{}, False],
      LSPServer`Private`queueSemanticTokensRefresh = Function[args, Null]
    },
      LSPServer`ProcessScheduledJobs[];
      {
        jobEvents,
        Lookup[LSPServer`$OpenFilesMap[badURI], "ScheduledJobs", Missing["NotFound"]],
        Lookup[LSPServer`$OpenFilesMap[goodURI], "ScheduledJobs", Missing["NotFound"]]
      }
    ]
  ],
  {{"bad", "good"}, {}, {}},
  TestID -> "ProcessScheduledJobsDropsFailingJobAndContinues"
]

VerificationTest[
  Module[{},
    Block[{
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$ContentQueue = {},
      LSPServer`$WorkspaceBootstrapAfter = None,
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`PacletIndex`$PacletIndex = <||>,
      LSPServer`PacletIndex`InitializePacletIndex = Function[{root},
        LSPServer`PacletIndex`$PacletIndex = <|
          "Files" -> <|"file:///tmp/testws/Closed.wl" -> <||>|>
        |>
      ],
      LSPServer`LoadProjectIgnoreConfig = Function[{root}, Null]
      },
      LSPServer`handleContent[<|"method" -> "initialized", "params" -> <||>|>];
      MatchQ[LSPServer`$WorkspaceBootstrapAfter, _?NumericQ]
    ]
  ],
  True,
  TestID -> "InitializedSchedulesBootstrapWorkspaceIndex"
]

VerificationTest[
  Module[{},
    Block[{
      LSPServer`$ServerState = "running",
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$ContentQueue = {},
      LSPServer`$WorkspaceBootstrapAfter = AbsoluteTime[] - 1,
      LSPServer`$QueueLastNonEmptyTime = AbsoluteTime[] - 5,
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$DiagnosticsTaskStartTime = None,
      LSPServer`$DiagnosticsKernel = None,
      LSPServer`$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 60,
      LSPServer`$IndexingWasActive = False,
      LSPServer`$PendingExternalDepFiles = {},
      LSPServer`$PendingIndexFiles = {},
      LSPServer`$PendingReferenceFiles = {},
      LSPServer`PacletIndex`Private`$PendingDepDiscovery = {},
      LSPServer`Private`ProcessPendingIndexFiles = Function[{}, False],
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`Private`launchDiagnosticsKernel = Function[{}, Null],
      LSPServer`Private`queueSemanticTokensRefresh = Function[args, Null]
    },
      LSPServer`ProcessScheduledJobs[];
      # ["method"] & /@ LSPServer`$ContentQueue
    ]
  ],
  {"workspace/bootstrapWorkspaceIndex"},
  TestID -> "ProcessScheduledJobsQueuesBootstrapWorkspaceIndexAfterIdle"
]

VerificationTest[
  Module[{uri},
    uri = "file:///tmp/testws/BootstrapClosed.wl";

    Block[{
      LSPServer`$ContentQueue = {},
      LSPServer`$WorkspaceRootPath = "/tmp/testws",
      LSPServer`$WorkspaceDiagnosticsSweepURIs = {uri},
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$DiagnosticsKernel = None,
      LSPServer`$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 60,
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`Private`launchDiagnosticsKernel = Function[{},
        LSPServer`$DiagnosticsKernel = $Failed
      ]
    },
      LSPServer`handleContent[<|"method" -> "workspace/bootstrapClosedFileDiagnostics"|>];
      {
        LSPServer`$DiagnosticsKernelLaunchAfter,
        LSPServer`$WorkspaceDiagnosticsSweepURIs,
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]]
      }
    ]
  ],
  {_?NumericQ, {}, {"textDocument/runClosedFileDiagnostics"}},
  SameTest -> MatchQ,
  TestID -> "BootstrapClosedFileDiagnosticsUsesSyncFallback"
]

VerificationTest[
  Module[{fixturePath, uri},
    fixturePath = FileNameJoin[{DirectoryName[$TestFileName], "..", "hover", "DocCommentTest.wl"}];
    uri = LocalObjects`PathToURI[fixturePath];

    Block[{
      LSPServer`$DiagnosticsKernel = Unique["Kernel"],
      LSPServer`$ContentQueue = {},
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$OpenFilesMap = <||>,
      LSPServer`$WorkspaceRootPath = DirectoryName[fixturePath],
      LSPServer`$ConfidenceLevel = 0.50,
      LSPServer`$ClosedFileDiagnosticsNotifications = <||>,
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`Diagnostics`Private`$BuiltinPatterns = <||>
    },
      LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics[uri];
      {
        Lookup[First[LSPServer`$ContentQueue], "method", Missing["NotFound"]],
        Lookup[
          Lookup[
            Lookup[First[LSPServer`$ContentQueue], "params", <||>],
            "textDocument",
            <||>
          ],
          "uri",
          Missing["NotFound"]
        ],
        LSPServer`$DiagnosticsTask
      }
    ]
  ],
  {"textDocument/runClosedFileDiagnostics", _String, None},
  SameTest -> MatchQ,
  TestID -> "DispatchClosedFileDiagnosticsQueuesSyncWork"
]

VerificationTest[
  Module[{uri, entry},
    uri = "file:///tmp/dispatch-workspace-diags.wl";
    entry = <|
      "Text" -> "f[x_] := x\n",
      "AST" -> HoldComplete[testAST],
      "LastChange" -> Now
    |>;

    Block[{
      LSPServer`$DiagnosticsKernel = Unique["Kernel"],
      LSPServer`$ContentQueue = {
        <|"method" -> "textDocument/runWorkspaceDiagnostics", "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>
      },
      LSPServer`$DiagnosticsTask = None,
      LSPServer`$DiagnosticsTaskURI = None,
      LSPServer`$DiagnosticsTaskKind = None,
      LSPServer`$DiagnosticsTaskResult = None,
      LSPServer`$DiagnosticsTaskStartTime = None,
      LSPServer`$OpenFilesMap = <|uri -> entry|>,
      LSPServer`$WorkspaceRootPath = "/tmp",
      LSPServer`$ConfidenceLevel = 0.50,
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`Diagnostics`Private`$BuiltinPatterns = <||>,
      ParallelSubmit = Function[{kernels, expr}, "fake-task", HoldAll]
    },
      LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics[uri];
      {
        Count[
          LSPServer`$ContentQueue,
          KeyValuePattern[{
            "method" -> "textDocument/runWorkspaceDiagnostics",
            "params" -> KeyValuePattern["textDocument" -> KeyValuePattern["uri" -> uri]]
          }]
        ],
        Lookup[LSPServer`$ContentQueue, "method", Missing["NotFound"]],
        LSPServer`$DiagnosticsTask,
        LSPServer`$DiagnosticsTaskKind,
        LSPServer`$DiagnosticsTaskURI
      }
    ]
  ],
  {0, Missing["NotFound"], "fake-task", "open-file", _String},
  SameTest -> MatchQ,
  TestID -> "DispatchWorkspaceDiagnosticsUsesBackgroundTask"
]
