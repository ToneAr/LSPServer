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
          LSPServer`PacletIndex`$PendingIndexFiles = {},
          LSPServer`PacletIndex`$PendingReferenceFiles = {},
          LSPServer`PacletIndex`$PendingExternalDepFiles = {}
        },
          LSPServer`PacletIndex`InitializePacletIndex[workspaceRoot];
          Sort[indexed]
        ]
      ],
      Quiet[DeleteDirectory[workspaceRoot, DeleteContents -> True]]
    ]
  ],
  {"Nested.wlt", "Package.wl", "Runner.mt", "Suite.wlt"},
  TestID -> "InitializePacletIndexScansWLTAndMTFiles"
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
      # ["method"] & /@ LSPServer`$ContentQueue
    ]
  ],
  {"workspace/bootstrapClosedFileDiagnostics"},
  TestID -> "InitializedQueuesBootstrapClosedFileDiagnostics"
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
  Module[{fixturePath, uri, fileText, entry, cst, agg, ast},
    fixturePath = FileNameJoin[{DirectoryName[$TestFileName], "..", "hover", "DocCommentTest.wl"}];
    uri = LocalObjects`PathToURI[fixturePath];
    fileText = ReadString[fixturePath];

    cst = Quiet[CodeConcreteParse[fileText, "FileFormat" -> "Package"]];
    agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
    ast = Quiet[CodeParser`Abstract`Abstract[agg]];
    entry = <|
      "Text" -> fileText,
      "CST" -> cst,
      "Agg" -> agg,
      "AST" -> ast,
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
      LSPServer`$OpenFilesMap = <|uri -> entry|>,
      LSPServer`$WorkspaceRootPath = DirectoryName[fixturePath],
      LSPServer`$ConfidenceLevel = 0.50,
      LSPServer`PacletIndex`$PacletIndex = <|
        "Symbols" -> <||>,
        "Files" -> <||>,
        "Contexts" -> <||>,
        "Dependencies" -> {},
        "ContextAliases" -> <||>
      |>,
      LSPServer`Diagnostics`Private`$BuiltinPatterns = <||>
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
        Lookup[Last[LSPServer`$ContentQueue], "method", Missing["NotFound"]],
        LSPServer`$DiagnosticsTask
      }
    ]
  ],
  {1, "textDocument/runWorkspaceDiagnostics", None},
  TestID -> "DispatchWorkspaceDiagnosticsQueuesSyncWork"
]
