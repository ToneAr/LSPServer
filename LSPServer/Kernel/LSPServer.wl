(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["LSPServer`"]

StartServer::usage = "StartServer[] puts the kernel into a state ready for traffic from the client.\
 StartServer[logDir] logs traffic to logDir."

RunServerDiagnostic

initializeLSPComm

expandContent
expandContents

expandContentsAndAppendToContentQueue

LSPEvaluate
readEvalWriteLoop

handleContent
handleContentAfterShutdown

LoadAllFeatureModules

ProcessScheduledJobs
SourceFileFormat

$ContentQueue
$PreExpandContentQueue
$OpenFilesMap
$CancelMap


exitHard
exitGracefully
exitSemiGracefully
shutdownLSPComm


(* Do not launch the diagnostics worker during Needs["LSPServer`"].
   StartServer[] schedules it lazily after initialize so package load stays
   below editor startup timeouts. *)


LSPServer`distributeDiagnosticsWorkerDefinitions[] :=
  Module[{},
    loadFeatureModule["Diagnostics"];
    loadFeatureModule["Hover"];
    Quiet[DistributeDefinitions[
      LSPServer`buildWorkerSnapshot,
      LSPServer`Diagnostics`Private`runWorkspaceDiagnosticsWorker,
      LSPServer`Diagnostics`Private`runClosedFileDiagnosticsWorker,
      LSPServer`Hover`Private`buildHoverWorkerSnapshot,
      LSPServer`Hover`Private`runHoverWorker,
      LSPServer`Hover`Private`finalizeHoverWorkerResponse
    ]]
  ]

$BracketMatcherUseDesignColors


$ConfidenceLevel

$HierarchicalDocumentSymbolSupport

(*
$InlayHints controls whether inlay hints are enabled.

Can be set via initializationOptions or workspace/didChangeConfiguration.
When False, the server does not advertise inlay hint capability
and returns empty results for inlay hint requests.
*)
$InlayHints

(*
$SemanticTokens is True if the client supports semantic tokens and the user has enabled them

If $SemanticTokens is False, then diagnostics are used as a fallback to indicate scoping issues such as unused variables and shadowed variables

*)
$SemanticTokens


$ML4CodeTimeLimit

$commProcess



$BracketMatcherDelayAfterLastChange
$DiagnosticsDelayAfterLastChange
$ImplicitTokensDelayAfterLastChange

$WorkspaceRootPath

$DiagnosticsKernel

$DiagnosticsKernelBin

$DiagnosticsTask

$DiagnosticsTaskURI

$DiagnosticsTaskKind

$DiagnosticsTaskResult

$DiagnosticsTaskStartTime

$DiagnosticsKernelLaunchAfter
$HoverTask
$HoverTaskURI
$HoverTaskID
$HoverTaskResult
$HoverTaskStartTime
$IndexingWasActive
$InternalRequestId
$PendingSemanticTokenRequests
$WorkspaceDiagnosticsSweepURIs
$ClosedFileDiagnosticsNotifications
$WorkspaceBootstrapAfter
$QueueLastNonEmptyTime
$PendingTokenRefresh
$PendingTokenRefreshTime
$WorkspaceIndexingQueued

$startupMessagesText


Begin["`Private`"]


(*
setup Startup Messages handling

There may be internal errors in LSPServer that emit messages during Needs["LSPServer`"]

These messages are exceptionally hard to handle because any code for handling has not yet been loaded

The messages may cause unexplained hangs in clients

So manually set $Messages to a tmp file and then handle the messages later
*)
$startupMessagesText = "";
$startupMessagesFile = OpenWrite[]

If[!FailureQ[$startupMessagesFile],
  $oldMessages = $Messages;
  $Messages = {$startupMessagesFile}
  ,
  $startupMessagesText = "OpenWrite[] failed while setting up Startup Messages handling"
]



Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

Needs["PacletManager`"] (* for PacletInformation *)


LSPServer`SourceFileFormat[pathOrURI_String] :=
Module[{path},
  path = StringReplace[pathOrURI, StartOfString ~~ "file://" -> ""];
  If[StringEndsQ[path, ".wls"], "Script", "Package"]
]


(*
TODO: when targeting 12.1 as a minimum, then use paclet["AssetLocation", "BuiltInFunctions"]
*)
location = "Location" /. PacletInformation["LSPServer"]

(*
Load data files FIRST - these are needed by several modules at load time
Modules like Completion.wl, Diagnostics.wl, and Hover.wl use these data variables
*)
(* wl-disable *)
WolframLanguageSyntax`Generate`$options :=
	WolframLanguageSyntax`Generate`$options =
	EntityClass["WolframLanguageSymbol", "OptionName"]["Name"]

WolframLanguageSyntax`Generate`$experimentalSymbols =
	Get[FileNameJoin[{location, "Resources", "Data", "ExperimentalSymbols.wl"}]]

WolframLanguageSyntax`Generate`$constants =
	Get[FileNameJoin[{location, "Resources", "Data", "Constants.wl"}]]

WolframLanguageSyntax`Generate`$builtinFunctions :=
	WolframLanguageSyntax`Generate`$builtinFunctions =
	(* Complement[ *)
		Names["System`*"];
		(* WolframLanguageSyntax`Generate`$options, *)
		(* WolframLanguageSyntax`Generate`$experimentalSymbols, *)
		(* WolframLanguageSyntax`Generate`$constants *)
	(* ] *)

WolframLanguageSyntax`Generate`$obsoleteSymbols =
    Get[FileNameJoin[{location, "Resources", "Data", "ObsoleteSymbols.wl"}]]

WolframLanguageSyntax`Generate`$sessionSymbols =
	Get[FileNameJoin[{location, "Resources", "Data", "SessionSymbols.wl"}]]

WolframLanguageSyntax`Generate`$badSymbols =
	Get[FileNameJoin[{location, "Resources", "Data", "BadSymbols.wl"}]]

WolframLanguageSyntax`Generate`$systemCharacters =
	Get[FileNameJoin[{location, "Resources", "Data", "SystemCharacters.wl"}]]

WolframLanguageSyntax`Generate`$undocumentedSymbols =
	Get[FileNameJoin[{location, "Resources", "Data", "UndocumentedSymbols.wl"}]]

WolframLanguageSyntax`Generate`$systemLongNames =
	Get[FileNameJoin[{location, "Resources", "Data", "SystemLongNames.wl"}]]
(* wl-enable *)


workspaceSourceFilePatterns[] := {
  "*.wl",
  "*.m",
  "*.wls",
  "*.wlt",
  "*.mt",
  "*.ipwl"
}

(*
Load LSPServer submodules using Get with explicit paths
This ensures all modules are loaded regardless of the paclet cache state
Data files must be loaded above BEFORE these modules
*)
Get[FileNameJoin[{location, "Kernel", "Utils.wl"}]]
Get[FileNameJoin[{location, "Kernel", "Library.wl"}]]
Get[FileNameJoin[{location, "Kernel", "StdIO.wl"}]]
Get[FileNameJoin[{location, "Kernel", "Socket.wl"}]]
Get[FileNameJoin[{location, "Kernel", "ListenSocket.wl"}]]
Get[FileNameJoin[{location, "Kernel", "ServerDiagnostics.wl"}]]
Get[FileNameJoin[{location, "Kernel", "CST.wl"}]]
Get[FileNameJoin[{location, "Kernel", "PacletIndex.wl"}]]
Get[FileNameJoin[{location, "Kernel", "IgnorePatterns.wl"}]]
Get[FileNameJoin[{location, "Kernel", "TypeWL.wl"}]]

$FeatureModuleFiles = <|
  "Diagnostics" -> FileNameJoin[{location, "Kernel", "Diagnostics.wl"}],
  "DocumentSymbol" -> FileNameJoin[{location, "Kernel", "DocumentSymbol.wl"}],
  "BracketMismatches" -> FileNameJoin[{location, "Kernel", "BracketMismatches.wl"}],
  "CodeAction" -> FileNameJoin[{location, "Kernel", "CodeAction.wl"}],
  "Color" -> FileNameJoin[{location, "Kernel", "Color.wl"}],
  "Completion" -> FileNameJoin[{location, "Kernel", "Completion.wl"}],
  "Definitions" -> FileNameJoin[{location, "Kernel", "Definitions.wl"}],
  "FoldingRange" -> FileNameJoin[{location, "Kernel", "FoldingRange.wl"}],
  "Formatting" -> FileNameJoin[{location, "Kernel", "Formatting.wl"}],
  "Hover" -> FileNameJoin[{location, "Kernel", "Hover.wl"}],
  "ImplicitTokens" -> FileNameJoin[{location, "Kernel", "ImplicitTokens.wl"}],
  "InlayHints" -> FileNameJoin[{location, "Kernel", "InlayHints.wl"}],
  "References" -> FileNameJoin[{location, "Kernel", "References.wl"}],
  "SelectionRange" -> FileNameJoin[{location, "Kernel", "SelectionRange.wl"}],
  "SemanticTokens" -> FileNameJoin[{location, "Kernel", "SemanticTokens.wl"}],
  "Workspace" -> FileNameJoin[{location, "Kernel", "Workspace.wl"}]
|>;

$FeatureModulesLoaded = <||>;

$FeatureExpandMethods = <|
  "textDocument/runDiagnostics" -> {"Diagnostics"},
  "textDocument/documentSymbol" -> {"DocumentSymbol"},
  "textDocument/runBracketMismatches" -> {"BracketMismatches"},
  "textDocument/codeAction" -> {"CodeAction"},
  "textDocument/documentColor" -> {"Color"},
  "textDocument/completion" -> {"Completion"},
  "textDocument/definition" -> {"Definitions"},
  "textDocument/foldingRange" -> {"FoldingRange"},
  "textDocument/hover" -> {"Hover"},
  "textDocument/runImplicitTokens" -> {"ImplicitTokens"},
  "textDocument/inlayHint" -> {"InlayHints"},
  "textDocument/references" -> {"References"},
  "textDocument/selectionRange" -> {"SelectionRange"},
  "textDocument/semanticTokens/full" -> {"SemanticTokens"}
|>;

$FeatureHandleMethods = <|
  "textDocument/runFastDiagnostics" -> {"Diagnostics"},
  "textDocument/mergeWorkspaceLints" -> {"Diagnostics"},
  "textDocument/runClosedFileDiagnostics" -> {"Diagnostics"},
  "textDocument/suppressedRegions" -> {"Diagnostics"},
  "textDocument/parseIgnoreComments" -> {"Diagnostics"},
  "textDocument/runConcreteDiagnostics" -> {"Diagnostics"},
  "textDocument/runAggregateDiagnostics" -> {"Diagnostics"},
  "textDocument/runAbstractDiagnostics" -> {"Diagnostics"},
  "textDocument/runScopingDiagnostics" -> {"Diagnostics"},
  "textDocument/runWorkspaceDiagnostics" -> {"Diagnostics"},
  "textDocument/clearDiagnostics" -> {"Diagnostics"},
  "textDocument/publishDiagnostics" -> {"Diagnostics"},
  "textDocument/documentNodeList" -> {"DocumentSymbol"},
  "textDocument/documentSymbolFencepost" -> {"DocumentSymbol"},
  "textDocument/runBracketMismatchesFencepost" -> {"BracketMismatches"},
  "textDocument/suggestBracketEdits" -> {"BracketMismatches"},
  "textDocument/clearBracketMismatches" -> {"BracketMismatches"},
  "textDocument/publishBracketMismatches" -> {"BracketMismatches"},
  "textDocument/codeActionFencepost" -> {"CodeAction"},
  "textDocument/documentColorFencepost" -> {"Color"},
  "textDocument/colorPresentation" -> {"Color"},
  "textDocument/completionFencepost" -> {"Completion"},
  "completionItem/resolve" -> {"Completion"},
  "textDocument/definitionFencepost" -> {"Definitions"},
  "textDocument/foldingRangeFencepost" -> {"FoldingRange"},
  "textDocument/formatting" -> {"Formatting"},
  "textDocument/rangeFormatting" -> {"Formatting"},
  "textDocument/hoverFencepost" -> {"Hover"},
  "textDocument/publishHoverResult" -> {"Hover"},
  "textDocument/runImplicitTokensFencepost" -> {"ImplicitTokens"},
  "textDocument/clearImplicitTokens" -> {"ImplicitTokens"},
  "textDocument/publishImplicitTokens" -> {"ImplicitTokens"},
  "textDocument/inlayHintFencepost" -> {"InlayHints"},
  "textDocument/referencesFencepost" -> {"References"},
  "textDocument/selectionRangeFencepost" -> {"SelectionRange"},
  "textDocument/semanticTokens/fullFencepost" -> {"SemanticTokens"},
  "textDocument/runScopingData" -> {"SemanticTokens"},
  "workspace/executeCommand" -> {"Workspace"},
  "workspace/didChangeWatchedFiles" -> {"Workspace"},
  "workspace/didChangeConfiguration" -> {"Workspace"},
  "workspace/symbol" -> {"Workspace"},
  "workspace/didChangeWorkspaceFolders" -> {"Workspace"}
|>;

loadFeatureModule[name_String] :=
  If[!TrueQ[Lookup[$FeatureModulesLoaded, name, False]],
    Get[$FeatureModuleFiles[name]];
    $FeatureModulesLoaded[name] = True
  ];

loadFeatureModulesForMethod[method_String] :=
  Scan[
    loadFeatureModule,
    Lookup[
      $FeatureHandleMethods,
      method,
      Lookup[$FeatureExpandMethods, method, {}]
    ]
  ];

LSPServer`LoadAllFeatureModules[] :=
  Scan[loadFeatureModule, Keys[$FeatureModuleFiles]];

expandContent[content : KeyValuePattern["method" -> method_String], pos_] /;
    KeyExistsQ[$FeatureExpandMethods, method] &&
    !TrueQ[Lookup[$FeatureModulesLoaded, First[$FeatureExpandMethods[method]], False]] :=
  Module[{},
    loadFeatureModulesForMethod[method];
    expandContent[content, pos]
  ];


(*
This uses func := func = def idiom and is fast
*)
LSPServer`Library`loadAllFuncs[]


$DefaultConfidenceLevel = 0.50

$CodeActionLiteralSupport = False

$AllowedImplicitTokens = {}

(*
if $BracketMatcher, then load ML4Code` and use ML bracket matching tech
*)
$BracketMatcher = False

$BracketMatcherUseDesignColors = True


$InlayHints = True

$SemanticTokens = False

$HierarchicalDocumentSymbolSupport = False


(*
$BracketMatcherDisplayInsertionText = False
*)

(*
Bracket suggestions from ML4Code can take O(n^2) time in the size of the chunk, so make sure to
have a time limit

Related issues: CODETOOLS-71
*)
$ML4CodeTimeLimit = 0.4


$ExecuteCommandProvider = <|
  "commands" -> {
    (*
    Toggle inlay hints on/off at runtime.
    When toggled, the server sends a workspace/inlayHint/refresh request
    to notify the client to re-request hints.
    *)
    "toggle_inlay_hints",
    (*
    roundtrip_responsiveness_test is an undocumented, debug command
    *)
    "roundtrip_responsiveness_test",
    (*
    ping_pong_responsiveness_test is an undocumented, debug command
    *)
    "ping_pong_responsiveness_test",
    (*
    payload_responsiveness_test is an undocumented, debug command
    *)
    "payload_responsiveness_test"
  }
|>




(*
lint objects may be printed to log files and we do not want to include ANSI control codes
*)
CodeInspector`Format`Private`$UseANSI = False


(*
The counter that is used for creating unique hrefs
*)
$hrefIdCounter = 0



$ErrorCodes = <|
  (*
  Defined by JSON RPC
  *)
  "ParseError" -> -32700,
  "InvalidRequest" -> -32600,
  "MethodNotFound" -> -32601,
  "InvalidParams" -> -32602,
  "InternalError" -> -32603,
  (* "jsonrpcReservedErrorRangeStart" -> -32099, *)
  "ServerNotInitialized" -> -32002,
  "UnknownErrorCode" -> -32001,
  (* "jsonrpcReservedErrorRangeEnd" -> -32000, *)
  (* "lspReservedErrorRangeStart" -> -32899, *)
  "ContentModified" -> -32801,
  "RequestCancelled" -> -32800
  (* "lspReservedErrorRangeEnd" -> -32800, *)
|>


$TextDocumentSyncKind = <|
  "None" -> 0,
  "Full" -> 1,
  "Incremental" -> 2
|>

$MessageType = <|
  "Error" -> 1,
  "Warning" -> 2,
  "Info" -> 3,
  "Log" -> 4
|>



$ContentQueue = {}
$WorkspaceBootstrapAfter = None

(*
Thunks set by readEvalWriteLoop so that yield points inside long-running
handlers can drain the transport queue and write responses without knowing
the transport type.
*)
$TryQueueThunk = Function[Null]
$WriteLSPResultThunk = Function[{contentsArg}, Null]


$PriorityContentQueueMethods = {
  "textDocument/didOpenFencepost",
  "textDocument/didChangeFencepost",
  "textDocument/didCloseFencepost",
  "textDocument/semanticTokens/fullFencepost"
}

(*
Interactive methods that a user is waiting on — these should be served before
background diagnostics items whenever possible. The takeFirstContentQueueItem
function promotes any interactive item to the front of the queue.
*)
$InteractiveContentQueueMethods = {
  "textDocument/hoverFencepost",
  "textDocument/hover",
  "textDocument/completion",
  "textDocument/completionFencepost",
  "textDocument/signatureHelp",
  "textDocument/definition",
  "textDocument/definitionFencepost",
  "textDocument/references",
  "textDocument/referencesFencepost",
  "textDocument/documentHighlight",
  "textDocument/documentSymbol",
  "textDocument/codeAction",
  "textDocument/rename",
  "textDocument/prepareRename",
  "textDocument/formatting",
  "textDocument/rangeFormatting",
  "textDocument/declaration",
  "textDocument/typeDefinition",
  "textDocument/implementation"
}

interactiveMethodQ[content_] :=
  AssociationQ[content] &&
  MemberQ[$InteractiveContentQueueMethods, Lookup[content, "method", None]]

(*
Diagnostic-tier methods that can be deferred when interactive requests are waiting.
*)
$DeferrableDiagnosticMethods = {
  "textDocument/runFastDiagnostics",
  "textDocument/runWorkspaceDiagnostics",
  "textDocument/mergeWorkspaceLints",
  "textDocument/publishClosedFileDiagnostics",
  "textDocument/runIndexUpdate",
  "textDocument/runOpenIndexUpdate",
  "workspace/processIndexing",
  "workspace/semanticTokens/refresh"
}

deferrableDiagnosticQ[content_] :=
  Module[{deferrable},
    deferrable = Lookup[content, "deferrable", Automatic];
    AssociationQ[content] &&
    If[
      deferrable === Automatic,
      MemberQ[$DeferrableDiagnosticMethods, Lookup[content, "method", None]],
      TrueQ[deferrable]
    ]
  ]


contentURI[content_] :=
  Lookup[
    Lookup[Lookup[content, "params", <||>], "textDocument", <||>],
    "uri",
    None
  ]
contiguousRequestPipelineRange[contents_List, idx_Integer] :=
  Module[{id, uri, start = idx, end = idx},
    id = Lookup[contents[[idx]], "id", Missing["NotFound"]];
    If[MissingQ[id],
      Return[{idx}]
    ];

    uri = contentURI[contents[[idx]]];

    While[
      start > 1 &&
      Lookup[contents[[start - 1]], "id", Missing["NotFound"]] === id &&
      contentURI[contents[[start - 1]]] === uri,
      start--
    ];

    While[
      end < Length[contents] &&
      Lookup[contents[[end + 1]], "id", Missing["NotFound"]] === id &&
      contentURI[contents[[end + 1]]] === uri,
      end++
    ];

    Range[start, end]
  ]


firstNonDeferrableQueueRange[contents_List] :=
  Module[{idx},
    If[contents === {} || !deferrableDiagnosticQ[First[contents]],
      Return[{}]
    ];

    idx = SelectFirst[
      Range[2, Length[contents]],
      !deferrableDiagnosticQ[contents[[#]]] &,
      0
    ];

    If[idx == 0,
      {},
      contiguousRequestPipelineRange[contents, idx]
    ]
  ]


promoteFirstNonDeferrableQueueRange[] :=
  Module[{range, block},
    range = firstNonDeferrableQueueRange[$ContentQueue];
    If[range === {} || First[range] <= 1,
      Return[False]
    ];

    block = $ContentQueue[[range]];
    $ContentQueue = Join[
      block,
      Delete[$ContentQueue, List /@ range]
    ];

    True
  ]


contentQueuePriorityMethodQ[content_] :=
  Module[{priority},
    priority = Lookup[content, "priority", Automatic];
    AssociationQ[content] &&
    If[
      priority === Automatic,
      MemberQ[$PriorityContentQueueMethods, Lookup[content, "method", None]],
      TrueQ[priority]
    ]
  ]


(*
  prioritizeContentQueueContents groups items so that each fencepost
  is immediately followed by its dependent items (runDiagnostics,
  publishDiagnostics, etc.) for the same URI, rather than pulling ALL
  fenceposts to the front. This prevents diagnostic starvation when
  many files are opened simultaneously.
*)
prioritizeContentQueueContents[contents_List] :=
  Module[{priorityGroups, nonGrouped, result, uri, method, i,
          fencepostIndices, claimed, group},

    (* Find indices of all priority (fencepost) items *)
    fencepostIndices = Select[Range[Length[contents]],
      contentQueuePriorityMethodQ[contents[[#]]] &];

    (* For each fencepost, greedily claim the immediately following
       non-priority items that share the same URI (these are the
       runDiagnostics / publishDiagnostics that were expanded together
       with the fencepost). *)
    claimed = <||>;  (* index -> True for items claimed by a group *)
    priorityGroups = {};

    Do[
      uri = Lookup[Lookup[Lookup[contents[[idx]], "params", <||>], "textDocument", <||>], "uri", None];
      group = {idx};
      i = idx + 1;
      While[i <= Length[contents] && !KeyExistsQ[claimed, i] &&
            !contentQueuePriorityMethodQ[contents[[i]]] &&
            Lookup[Lookup[Lookup[contents[[i]], "params", <||>], "textDocument", <||>], "uri", None] === uri,
        AppendTo[group, i];
        i++
      ];
      Do[claimed[g] = True, {g, group}];
      AppendTo[priorityGroups, group],
      {idx, fencepostIndices}
    ];

    (* Collect remaining unclaimed non-priority items *)
    nonGrouped = Select[Range[Length[contents]],
      !KeyExistsQ[claimed, #] &];

    (* Result: priority groups first (each fencepost + its diagnostics),
       then any remaining items *)
    result = Join[
      Flatten[Map[contents[[#]] &, priorityGroups, {2}], 1],
      contents[[nonGrouped]]
    ];

    result
  ]


appendContentsToContentQueue[contents_List] :=
  If[contents =!= {},
    (* Keep fencepost items grouped with their per-URI diagnostic
       pipeline items so diagnostics are not starved behind a wall
       of fenceposts from other URIs. *)
    $ContentQueue = prioritizeContentQueueContents[Join[$ContentQueue, contents]]
  ]


contentQueueEmptyQ[] := $ContentQueue === {}


takeFirstContentQueueItem[] :=
  If[contentQueueEmptyQ[],
    None,
    Module[{content},
      (* If the queue is blocked on deferrable diagnostics, promote the earliest
         later non-deferrable pipeline as a unit so parse steps stay attached to
         their fencepost/request response. *)
      If[deferrableDiagnosticQ[First[$ContentQueue]],
        promoteFirstNonDeferrableQueueRange[]
      ];
      content = First[$ContentQueue];
      $ContentQueue = Rest[$ContentQueue];
      content
    ]
  ]

(*
yieldToInteractiveRequests[currentURI]

Called at yield points inside long-running handlers (e.g. runFastDiagnostics,
runIndexUpdate) to cooperatively serve queued non-deferrable work without
waiting for the heavy computation to finish.

Flow:
  1. Drain the transport queue (TryQueue) so newly-arrived messages enter $ContentQueue
  2. While the queue has a non-deferrable item available ahead of the current
     deferrable work, dequeue it, evaluate it, and write the response
  3. Return True only when the caller's URI has become stale.
*)
yieldToInteractiveRequests[currentURI_:None] :=
Module[{content, contents},
  (* Drain transport → $ContentQueue *)
  $TryQueueThunk[];

  While[
    !contentQueueEmptyQ[] &&
    If[
      deferrableDiagnosticQ[First[$ContentQueue]],
      promoteFirstNonDeferrableQueueRange[],
      True
    ]
    ,
    content = First[$ContentQueue];
    $ContentQueue = Rest[$ContentQueue];
    log[1, "yield: serving queued work ", Lookup[content, "method", Missing["NotFound"]]];
    contents = LSPEvaluate[content];
    $WriteLSPResultThunk[contents];

    If[StringQ[currentURI] && isStale[$ContentQueue, currentURI],
      Return[True]
    ];

    $TryQueueThunk[]
  ];

  StringQ[currentURI] && isStale[$ContentQueue, currentURI]
]

(*
An assoc of uri -> entry

entry is an assoc of various key/values such as "Text" -> text and "CST" -> cst

*)
$OpenFilesMap = <||>

(*
An assoc of id -> True|False
*)
$CancelMap = <||>



(*
Expands contents and appends to $ContentQueue

Returns Null
*)
expandContentsAndAppendToContentQueue[contentsIn_] :=
Module[{contents, ignoredResponses},

  contents = contentsIn;

  ignoredResponses = Select[contents, AssociationQ[#] && !KeyExistsQ[#, "method"] &];
  If[ignoredResponses =!= {},
    log[1, "Ignoring client responses without method: ids=",
      InputForm[Lookup[ignoredResponses, "id", Missing["NotFound"]]]]
  ];
  contents = Select[contents, KeyExistsQ[#, "method"] &];

  If[contents === {},
    Return[Null]
  ];

  log[1, "**************************************** Message Cycle ****************************************** \n"];
  log[1, "$ContentQueue Methods(before expansion):> ", InputForm[#["method"]& /@ $ContentQueue]];
  log[1, "New message (before expansion):> ", InputForm[#["method"]& /@ contents]];

  If[!MatchQ[contents, {_?AssociationQ ...}],
    log[0, "\n\n"];
    log[0, "Internal assert 1 failed: list of Associations: ", contents];
    log[0, "\n\n"];

    exitHard[]
  ];

  preScanForCancels[contents];

  (*
  Now expand new contents
  *)

  contents = expandContents[contents];

  appendContentsToContentQueue[contents];

  log[1, "$ContentQueue methods (after expansion & joining new content) :> ", InputForm[#["method"]& /@ $ContentQueue]];
  log[3, "$ContentQueue (after expansion & joining new content):> ", InputForm[$ContentQueue], "\n"];


]


(*

Use 0.4 seconds, same as default value of spelling squiggly in FE

In[7]:= CurrentValue[$FrontEnd, {SpellingOptions, "AutoSpellCheckDelay"}]

Out[7]= 0.4
*)
$DiagnosticsDelayAfterLastChange = 0.4

$ImplicitTokensDelayAfterLastChange = 3.0

$BracketMatcherDelayAfterLastChange = 4.0



StartServer::notebooks = "LSPServer cannot be started inside of a notebook session."

Options[StartServer] = {
  ConfidenceLevel -> Automatic,
  CommunicationMethod -> "StdIO"
}

(*
setup the REPL to handle traffic from client
*)
StartServer[logDir_String:"", OptionsPattern[]] :=
Catch[
Catch[
Module[{logFile, logFileStream,
  logFileName, logFileCounter, oldLogFiles, now, quantity30days, dateStr, readEvalWriteCycle},

  $kernelStartTime = Now;

  If[$Notebooks,
    (*
    OK to return here without killing the kernel
    This is in a notebook session
    *)
    Message[StartServer::notebooks];
    Throw[$Failed]
  ];

  (*
  This is NOT a notebook session, so it is ok to kill the kernel
  *)

  $ConfidenceLevelOption = OptionValue[ConfidenceLevel];
  $commProcess = OptionValue[CommunicationMethod];


  $MessagePrePrint =.;

  (*
  Ensure that no messages are printed to stdout
  *)
  $Messages = Streams["stderr"];

  (*
  Ensure that no Print output is printed to stdout

  There may have been messages printed from doing Needs["LSPServer`"], and we can't do anything about those
  But they will be detected when doing RunServerDiagnostic[]
  *)
  $Output = Streams["stderr"];

  (* Background kernel for async workspace diagnostics. *)
  (* Kernel is launched lazily ~5s after "initialized" to avoid blocking startup. *)
  $DiagnosticsTask              = None;
  $DiagnosticsTaskURI           = None;
  $DiagnosticsTaskKind          = None;
  $DiagnosticsTaskResult        = None;
  $DiagnosticsTaskStartTime     = None;
  $DiagnosticsKernel            = None;
  $DiagnosticsKernelBin         = None;
  $DiagnosticsKernelLaunchAfter = None;
  $HoverTask                    = None;
  $HoverTaskURI                 = None;
  $HoverTaskID                  = None;
  $HoverTaskResult              = None;
  $HoverTaskStartTime           = None;
  $WorkspaceBootstrapAfter      = None;
  $IndexingWasActive            = False;
  $InternalRequestId            = -1;
  $PendingSemanticTokenRequests = <||>;
  $WorkspaceDiagnosticsSweepURIs = {};
  $ClosedFileDiagnosticsNotifications = <||>;
  $QueueLastNonEmptyTime        = 0;
  $PendingTokenRefresh          = False;
  $PendingTokenRefreshTime      = None;
  $WorkspaceIndexingQueued      = False;


  If[(logDir != ""),

    (
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::BackwardsCompatibility:: *)
    Quiet[CreateDirectory[logDir], {CreateDirectory::eexist, CreateDirectory::filex}];
    (* :!CodeAnalysis::EndBlock:: *)
    );

    (*
    Cleanup existing log files
    *)
    oldLogFiles = FileNames["kernelLog*", logDir];
    now = Now;
    (*
    Was using ":" as a time separator
    But obviously cannot use ":" character in file names on Windows!!
    *)
    dateStr = DateString[now, {"Year", "-", "Month", "-", "Day", "_", "Hour24", "-", "Minute", "-", "Second"}];
    quantity30days = Quantity[30, "Days"];
    Do[
      (*
      Delete oldLogFile if not modified for 30 days
      *)
      If[(now - Information[File[oldLogFile]]["LastModificationDate"]) > quantity30days,
        DeleteFile[oldLogFile]
      ]
      ,
      {oldLogFile, oldLogFiles}
    ];

    logFileName = "kernelLog-" <> dateStr;
    logFile = FileNameJoin[{logDir, logFileName <> ".txt"}];

    logFileCounter = 1;
    While[True,
      If[FileExistsQ[logFile],
        logFile = FileNameJoin[{logDir, logFileName <> "-" <> ToString[logFileCounter] <> ".txt"}];
        logFileCounter++;
        ,
        Break[]
      ]
    ];

    logFileStream = OpenWrite[logFile, CharacterEncoding -> "UTF-8"];

    If[FailureQ[logFileStream],

      log["\n\n"];
      log["opening log file failed: ", logFileStream];
      log["\n\n"];

      exitHard[]
    ];

    $Messages = $Messages ~Join~ { logFileStream };

    $Output = $Output ~Join~ { logFileStream }
  ];

  (*
  Previously tried setting CharacterEncoding -> "UTF-8", but seems to have no effect

  Maybe because stream is already open and being written to?

  TODO: look into any bug reports about setting CharacterEncoding for $Messages
  *)
  SetOptions[$Messages, PageWidth -> Infinity];

  (*
  There may be messages that we want to see

  TODO: investigate resetting the General::stop counter at the start of each eval loop
  *)
  Off[General::stop];


  log[0, "$CommandLine: ", $CommandLine];
  log[0, "\n\n"];

  log[0, "$commProcess: ", $commProcess];
  log[0, "\n\n"];

  log[0, "$ProcessID: ", $ProcessID];
  log[0, "\n\n"];

  log[0, "$ParentProcessID: ", $ParentProcessID];
  log[0, "\n\n"];

  log[0, "Directory[]: ", Directory[]];
  log[0, "\n\n"];


  log[0, "Starting server... (If this is the last line you see, then StartServer[] may have been called in an unexpected way and the server is hanging.)"];
  log[0, "\n\n"];


  If[$startupMessagesText =!= "",
    log["\n\n"];
    log["There were messages when loading LSPServer` package: ", $startupMessagesText];
    log["\n\n"];

    exitHard[]
  ];


  (*
  This is the first use of LSPServer library, so this is where the library is initialized.
  Handle any initialization failures or other errors.
  *)

  $initializedComm = initializeLSPComm[$commProcess];

  If[FailureQ[$initializedComm],
    log[0,"\n\n"];
    (*
    //InputForm to work-around bug 411375
    *)
    log["Initialization failed: ", $initializedComm //InputForm];
    log["\n\n"];

    exitHard[]
  ];

  readEvalWriteCycle = readEvalWriteLoop[$commProcess, $initializedComm];

  If[FailureQ[readEvalWriteCycle],
    log["\n\n"];
    log["Read-Eval-Write-Loop failed: ", readEvalWriteCycle];
    log["\n\n"];

    exitHard[]
  ];

]],(*Module, 1-arg Catch*)
_,
(
  log["\n\n"];
  log["uncaught Throw: ", #1];
  log["\n\n"];

  exitHard[]

  )&
]


preScanForCancels[contents:{_?AssociationQ ...}] :=
Module[{cancels, params, id},

  cancels = Cases[contents, KeyValuePattern["method" -> "$/cancelRequest"]];

  Scan[
    Function[{content},
      params = content["params"];

      id = params["id"];

      $CancelMap[id] = True
    ], cancels];


    log[2, "after preScanForCancels"];
    log[2, "$CancelMap: ", $CancelMap];

]


(*
Input: list of Associations
Returns: list of Associations
*)
expandContents[contentsIn_] :=
Module[{contents, lastContents},

  contents = contentsIn;
  (*
  This log can be used to know time to handle a feature.
  Time taken for a feature (x feature timing) = (feature exit log timing - new message entry timing)
  As we are changing the message queue to prioritise cumpletion message,
  it is important to know the completion feature timing.
  *)
  If[$Debug2,
    log["New message (before expansion):> ", InputForm[#["method"]& /@ contents], "\n"]
  ];

  log[2, "before expandContent"];

  Block[{$PreExpandContentQueue},

    $PreExpandContentQueue = contents;

    lastContents = $PreExpandContentQueue;

    $PreExpandContentQueue = Flatten[MapIndexed[expandContent, $PreExpandContentQueue] /. expandContent[c_, _] :> {c}];

    log[2, "$PreExpandContentQueue (up to 20): ", #["method"]& /@ Take[$PreExpandContentQueue, UpTo[20]]];
    log[2, "..."];

    While[$PreExpandContentQueue =!= lastContents,

      log[2, "expanded (up to 20): ", #["method"]& /@ Take[$PreExpandContentQueue, UpTo[20]]];
      log[2, "..."];

      lastContents = $PreExpandContentQueue;

      $PreExpandContentQueue = Flatten[MapIndexed[expandContent, $PreExpandContentQueue] /. expandContent[c_, _] :> {c}];

      log[2, "$PreExpandContentQueue (up to 20): ", #["method"]& /@ Take[$PreExpandContentQueue, UpTo[20]]];
      log[2, "..."];
    ];

    log[2, "after expandContent"];

    contents = $PreExpandContentQueue;
  ];

  If[!MatchQ[contents, {_?AssociationQ ...}],
    log[0, "\n\n"];
    log[0, "Internal assert 2 failed: list of Associations: ", contents];
    log[0, "\n\n"];

    exitHard[]
  ];

  contents
]


launchDiagnosticsKernel[] :=
Module[{addonsApps, kernelObjDir, kernelBin, startupWl, ok, kernel = $Failed, setupResult = $Failed},
  If[$DiagnosticsKernel =!= None || $DiagnosticsTask =!= None,
    cleanupDiagnosticsWorker[True]
  ];

  addonsApps   = FileNameJoin[{$InstallationDirectory, "AddOns", "Applications"}];
  kernelObjDir = FileNameJoin[{$InstallationDirectory, "SystemFiles",
                                "Components", "KernelObjects", "Kernel"}];
  kernelBin    = FileNameJoin[{$InstallationDirectory, "SystemFiles", "Kernel",
                                "Binaries", $SystemID, "WolframKernel"}];
  startupWl    = FileNameJoin[{kernelObjDir, "KernelObjectsStartup.wl"}];
  ok = DirectoryQ[addonsApps] && DirectoryQ[kernelObjDir] &&
       FileExistsQ[kernelBin] && FileExistsQ[startupWl];
  If[ok,
    If[!MemberQ[$Path, kernelObjDir], PrependTo[$Path, kernelObjDir]];
    If[!MemberQ[$Path, addonsApps],   PrependTo[$Path, addonsApps]];
    Quiet[Get[startupWl]];
    Quiet[Needs["Parallel`"]];
    CheckAbort[
      kernel = Quiet[Check[
        Module[{kernels = LaunchKernels[1]},
          If[ListQ[kernels] && Length[kernels] > 0, First[kernels], $Failed]
        ],
        $Failed
      ]];
      If[kernel =!= $Failed,
        setupResult = Quiet[Check[
          ParallelEvaluate[
            Needs["CodeParser`"];
            Needs["CodeInspector`"];
            Needs["CodeFormatter`"]
            ,
            kernel
          ],
          $Failed
        ]];
        If[setupResult =!= $Failed && FreeQ[setupResult, $Failed],
          setupResult = Quiet[Check[
            DistributeDefinitions["LSPServer`", "LSPServer`Private`", "LSPServer`Utils`",
              "LSPServer`PacletIndex`", "LSPServer`Diagnostics`", "LSPServer`Diagnostics`Private`",
              kernel],
            $Failed
          ]]
        ];
        If[setupResult =!= $Failed,
          $DiagnosticsKernelBin = kernelBin;
          $DiagnosticsKernel = kernel
          ,
          Quiet[AbortKernels[kernel]];
          Quiet[CloseKernels[kernel]];
          kernel = $Failed;
          $DiagnosticsKernelBin = $Failed;
          $DiagnosticsKernel    = $Failed
        ]
        ,
        $DiagnosticsKernelBin = $Failed;
        $DiagnosticsKernel    = $Failed
      ]
      ,
      If[kernel =!= $Failed,
        Quiet[AbortKernels[kernel]];
        Quiet[CloseKernels[kernel]];
        kernel = $Failed
      ];
      $DiagnosticsKernelBin = $Failed;
      $DiagnosticsKernel    = $Failed
    ]
    ,
    $DiagnosticsKernelBin = $Failed;
    $DiagnosticsKernel    = $Failed
  ];
  If[kernel =!= $Failed && $DiagnosticsKernel =!= kernel,
    Quiet[AbortKernels[kernel]];
    Quiet[CloseKernels[kernel]]
  ];
  If[$DiagnosticsKernel === None,
    $DiagnosticsKernelBin = $Failed;
    $DiagnosticsKernel    = $Failed
  ];
  If[$DiagnosticsKernel === $Failed,
    log[0, "WARNING: LaunchKernels failed - workspace diagnostics will run synchronously"]
  ]
]


handleContent[content:KeyValuePattern["method" -> "workspace/processIndexing"]] :=
Module[{moreWork},


  log[1, "workspace/processIndexing: enter"];


  $WorkspaceIndexingQueued = False;

  If[!workspaceIndexingPendingQ[],
    finishWorkspaceIndexing[];
    log[1, "workspace/processIndexing: exit"];
    Return[{}]
  ];

  moreWork = LSPServer`PacletIndex`ProcessPendingIndexFiles[];

  If[moreWork,
    $IndexingWasActive = True;
    queueWorkspaceIndexing[
      "workspace/processIndexing: re-queued remaining index work"
    ]
  ,
    finishWorkspaceIndexing[]
  ];

  log[1, "workspace/processIndexing: exit"];

  {}
]


clearDiagnosticsTaskState[requeueClosedFileSweep_:False] :=
Module[{taskKind, taskURI},
  taskKind = $DiagnosticsTaskKind;
  taskURI = $DiagnosticsTaskURI;

  If[TrueQ[requeueClosedFileSweep] && taskKind === "closed-file-sweep" && StringQ[taskURI],
    requeueWorkspaceDiagnosticsSweepURI[taskURI]
  ];

  $DiagnosticsTask          = None;
  $DiagnosticsTaskURI       = None;
  $DiagnosticsTaskKind      = None;
  $DiagnosticsTaskResult    = None;
  $DiagnosticsTaskStartTime = None;

  Null
]


clearHoverTaskState[] :=
(
  $HoverTask = None;
  $HoverTaskURI = None;
  $HoverTaskID = None;
  $HoverTaskResult = None;
  $HoverTaskStartTime = None;
  Null
)


abortHoverTask[] :=
Module[{},
  If[$HoverTask =!= None && $DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
    Quiet[AbortKernels[$DiagnosticsKernel]]
  ];

  Null
]


cancelCurrentHoverTask[] :=
Module[{},
  If[$HoverTask =!= None,
    clearHoverTaskState[];
    abortHoverTask[]
  ];

  Null
]


abortDiagnosticsKernel[] :=
Module[{kernel},
  kernel = $DiagnosticsKernel;

  If[kernel =!= $Failed && kernel =!= None,
    Quiet[AbortKernels[kernel]]
  ];

  Null
]


closeDiagnosticsKernel[] :=
Module[{kernel},
  kernel = $DiagnosticsKernel;

  abortDiagnosticsKernel[];

  If[kernel =!= $Failed && kernel =!= None,
    Quiet[CloseKernels[kernel]]
  ];

  $DiagnosticsKernel = None;
  $DiagnosticsKernelBin = None;

  Null
]


cleanupDiagnosticsWorker[requeueClosedFileSweep_:False] :=
Module[{},
  clearDiagnosticsTaskState[requeueClosedFileSweep];
  closeDiagnosticsKernel[]
]


workspaceDiagnosticsSweepURIQ[uri_String] :=
  StringQ[$WorkspaceRootPath] &&
  StringStartsQ[uri, "file://"] &&
  StringStartsQ[normalizeURI[uri], $WorkspaceRootPath] &&
  !KeyExistsQ[$OpenFilesMap, uri]


queueWorkspaceDiagnosticsSweep[uris_List] :=
Module[{filtered},
  If[!ListQ[$WorkspaceDiagnosticsSweepURIs],
    $WorkspaceDiagnosticsSweepURIs = {}
  ];

  filtered = Select[
    DeleteDuplicates[uris],
    workspaceDiagnosticsSweepURIQ
  ];

  If[filtered =!= {},
    $WorkspaceDiagnosticsSweepURIs =
      DeleteDuplicates[Join[$WorkspaceDiagnosticsSweepURIs, filtered]]
  ];

  Null
]


queueWorkspaceDiagnosticsSweep[] :=
Module[{files},
  files = Replace[Lookup[$PacletIndex, "Files", <||>], Except[_Association] -> <||>];
  queueWorkspaceDiagnosticsSweep[Keys[files]]
]


requeueWorkspaceDiagnosticsSweepURI[uri_String] :=
Module[{},
  If[workspaceDiagnosticsSweepURIQ[uri],
    If[!ListQ[$WorkspaceDiagnosticsSweepURIs],
      $WorkspaceDiagnosticsSweepURIs = {}
    ];
    $WorkspaceDiagnosticsSweepURIs =
      DeleteDuplicates[Join[{uri}, $WorkspaceDiagnosticsSweepURIs]]
  ];

  Null
]


openFilesAffectedByDefinitions[symbols_List] :=
Module[{referencingURIs},
  If[symbols === {},
    Return[{}]
  ];

  referencingURIs = DeleteDuplicates @ Flatten[
    Function[{sym},
      Lookup[
        Replace[LSPServer`PacletIndex`GetSymbolReferences[sym], Except[_List] -> {}],
        "uri",
        {}
      ]
    ] /@ symbols
  ];

  Select[referencingURIs, KeyExistsQ[$OpenFilesMap, #] &]
]


workspaceIndexingPendingQ[] :=
  Length[LSPServer`PacletIndex`Private`$PendingExternalDepFiles] > 0 ||
  Length[LSPServer`PacletIndex`$PendingIndexFiles] > 0 ||
  Length[LSPServer`PacletIndex`$PendingReferenceFiles] > 0 ||
  Length[LSPServer`PacletIndex`Private`$PendingDepDiscovery] > 0


queueWorkspaceIndexing[reason_String:""] :=
  If[workspaceIndexingPendingQ[] && !TrueQ[$WorkspaceIndexingQueued],
    If[reason =!= "",
      log[1, reason]
    ];
    $WorkspaceIndexingQueued = True;
    appendContentsToContentQueue[{
      <|"method" -> "workspace/processIndexing", "deferrable" -> True|>
    }]
  ]


finishWorkspaceIndexing[] :=
  If[$IndexingWasActive,
    $IndexingWasActive = False;
    log[1, "Indexing complete. Invalidating WorkspaceLints for ", Length[$OpenFilesMap], " open files and re-dispatching workspace diagnostics."];
    (* Invalidate cached WorkspaceLints for all open files so that
       runWorkspaceDiagnostics actually re-runs instead of returning
       the stale results computed before external deps were indexed. *)
    Scan[
      Function[{uri},
        Module[{entry},
          entry = Lookup[$OpenFilesMap, uri, Null];
          If[AssociationQ[entry],
            entry["WorkspaceLints"] = Null;
            $OpenFilesMap[uri] = entry
          ]
        ]
      ],
      Keys[$OpenFilesMap]
    ];
    (* Re-dispatch workspace diagnostics for all open files now that
       $PacletIndex is fully populated with external dep symbols.
       dispatchWorkspaceDiagnostics handles both the parallel-kernel path
       and the sync-fallback path, so no kernel-availability guard needed. *)
    Scan[
      loadFeatureModule["Diagnostics"];
      LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics,
      Keys[$OpenFilesMap]
    ];
    queueWorkspaceDiagnosticsSweep[];
    (* Tell VS Code to re-request semantic tokens for all open files so
       newly-indexed dependency symbols are classified correctly. *)
    queueSemanticTokensRefresh[
      "DBG-ST: indexing done; queuing workspace/semanticTokens/refresh"
    ]
  ]


cancelCurrentDiagnosticsTask[] :=
Module[{},
  If[$DiagnosticsTask =!= None,
    clearDiagnosticsTaskState[True];
    abortDiagnosticsKernel[]
  ];

  Null
]


ProcessScheduledJobs[] :=
Catch[
Module[{openFilesMapCopy, entryCopy, jobs, res, methods, contents, toRemove, job, toRemoveIndices, contentsToAdd},

  (*
  Do not process any scheduled jobs after shutdown
  *)
  If[$ServerState == "shutdown",
    Throw[Null]
  ];

  (*
  Queue workspace indexing as deferrable content so it can make progress while
  the server is busy, but still yield to interactive requests.
  *)
  Module[{hadIndexWork},
    hadIndexWork = workspaceIndexingPendingQ[];

    (* Mark indexing active as soon as we observe pending work, even if the
       current call drains the queues completely. Without this, single-batch
       dependency/workspace indexing never triggers the completion refresh. *)
    If[hadIndexWork,
      $IndexingWasActive = True;
      queueWorkspaceIndexing[
        "ProcessScheduledJobs: queued cooperative workspace indexing"
      ]
    ,
      finishWorkspaceIndexing[]
    ]
  ];

  (*
  Track the last time the queue was non-empty, for the kernel launch idle guard.
  *)
  If[Length[$ContentQueue] > 0,
    $QueueLastNonEmptyTime = AbsoluteTime[]
  ];

  If[
    $WorkspaceBootstrapAfter =!= None &&
    AbsoluteTime[] >= $WorkspaceBootstrapAfter &&
    Length[$ContentQueue] == 0 &&
    AbsoluteTime[] - $QueueLastNonEmptyTime >= 1,
    $WorkspaceBootstrapAfter = None;
    appendContentsToContentQueue[{<|"method" -> "workspace/bootstrapWorkspaceIndex", "deferrable" -> True|>}]
  ];

  (*
  Launch the background diagnostics kernel deferred from startup.
  Only fire when the queue has been idle for at least 3 seconds — this ensures
  VS Code has received all pending responses (tokens, diagnostics) before the
  blocking LaunchKernels + DistributeDefinitions call stalls the event loop.
  If the queue is busy or recently busy, defer by 2 seconds and retry.
  *)
  If[$DiagnosticsKernelLaunchAfter =!= None && AbsoluteTime[] >= $DiagnosticsKernelLaunchAfter,
    If[Length[$ContentQueue] == 0 && AbsoluteTime[] - $QueueLastNonEmptyTime >= 3,
      $DiagnosticsKernelLaunchAfter = None;
      launchDiagnosticsKernel[]
    ,
      (* Queue busy or recently busy; defer by 2 seconds and try again *)
      $DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 2
    ]
  ];

  If[$HoverTask =!= None && $DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
    If[NumberQ[$HoverTaskStartTime] &&
       AbsoluteTime[] - $HoverTaskStartTime > 45,
      log[0, "WARNING: hover task timed out after 45s; restarting worker kernel"];
      clearHoverTaskState[];
      cleanupDiagnosticsWorker[True];
      $DiagnosticsKernelBin = $Failed;
      $DiagnosticsKernel    = $Failed
    ];
    If[$HoverTask =!= None,
      Module[{taskResult, taskURI, taskID},
        taskResult = Quiet[Check[
          TimeConstrained[WaitAll[$HoverTask], 0.001, Missing["StillRunning"]],
          $Failed
        ]];
        If[!MatchQ[taskResult, _Missing],
          taskURI = $HoverTaskURI;
          taskID = $HoverTaskID;
          clearHoverTaskState[];
          If[AssociationQ[taskResult] &&
             Lookup[taskResult, "URI", None] === taskURI &&
             Lookup[taskResult, "ID", None] === taskID,
            $HoverTaskResult = taskResult;
            AppendTo[$ContentQueue,
              <|"method" -> "textDocument/publishHoverResult",
                "params" -> <|"textDocument" -> <|"uri" -> taskURI|>|>,
                "id" -> taskID,
                "priority" -> True|>]
          ]
        ]
      ]
    ]
  ];

  (*
  Consume the background diagnostics task once the event loop is idle.

  TimeConstrained[WaitAll[task], t, alt] is the correct non-blocking pattern:
  - If the task is already done WaitAll returns immediately (microseconds) and
    TimeConstrained passes the result through.
  - If the task is still running WaitAll blocks; TimeConstrained fires an
    Interrupt[] after t seconds and returns the sentinel alt.
  Unlike WaitAll[{task}, t] (the timed WaitAll form), a TimeConstrained-
  interrupted WaitAll does NOT invalidate the EvaluationObject, so we can
  safely retry on the next event-loop iteration.

  A 45-second timeout guard detects crashed worker kernels (which would cause
  WaitAll to block permanently) and kills the kernel so the fallback sync path
  can take over.
  *)
  If[$DiagnosticsTask =!= None && $DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
    (* Timeout guard: kill the worker kernel if it has been stuck for too long.
       NumberQ guards against an unbound or None $DiagnosticsTaskStartTime. *)
    If[NumberQ[$DiagnosticsTaskStartTime] &&
       AbsoluteTime[] - $DiagnosticsTaskStartTime > 45,
      log[0, "WARNING: diagnostics task timed out after 45s; restarting worker kernel"];
      cleanupDiagnosticsWorker[True];
      $DiagnosticsKernelBin = $Failed;
      $DiagnosticsKernel    = $Failed;
      $DiagnosticsTaskStartTime = None
    ];
    If[$DiagnosticsTask =!= None,
      Module[{taskResult, taskKind, taskURI},
        (*
        Poll with a 1 ms TimeConstrained.  If the worker is done the result
        arrives in microseconds (well within 1 ms); if it is still running we
        get Missing["StillRunning"] and retry on the next iteration (~10 ms).
        Previously this poll was guarded by Length[$ContentQueue] == 0, which
        meant a busy queue (e.g. rapid hover/completion requests) could prevent
        the diagnostics result from ever being collected, permanently stalling
        the diagnostics pipeline.  The poll is cheap enough to run always.
        *)
        taskResult = Quiet[Check[
          TimeConstrained[WaitAll[$DiagnosticsTask], 0.001, Missing["StillRunning"]],
          $Failed
        ]];
        If[!MatchQ[taskResult, _Missing],
          (* Task completed (with a result) or failed ($Failed) *)
          taskKind = $DiagnosticsTaskKind;
          taskURI = $DiagnosticsTaskURI;
          $DiagnosticsTask          = None;
          $DiagnosticsTaskURI       = None;
          $DiagnosticsTaskKind      = None;
          $DiagnosticsTaskStartTime = None;
          If[AssociationQ[taskResult] &&
             Lookup[taskResult, "URI", None] === taskURI,
            $DiagnosticsTaskResult = taskResult;
            Switch[taskKind,
              "open-file",
                AppendTo[$ContentQueue,
                  <|"method" -> "textDocument/mergeWorkspaceLints",
                    "params" -> <|"textDocument" -> <|"uri" -> taskURI|>|>|>]
              ,
              "closed-file-sweep",
                AppendTo[$ContentQueue,
                  <|"method" -> "textDocument/publishClosedFileDiagnostics",
                    "params" -> <|"textDocument" -> <|"uri" -> taskURI|>|>|>]
              ,
              _,
                $DiagnosticsTaskResult = None
            ]
          ]
        ]
        (* If Missing["StillRunning"]: task still in flight — leave $DiagnosticsTask
           intact and retry on the next event-loop iteration. *)
      ]
    ]
  ];

  (*
  When the queue is idle, scan one closed workspace file at a time so the
  Problems view can populate for files that are not currently open. Closed-file
  diagnostics now run on the main-kernel queue, so only inject more sweep work
  after the event loop has been genuinely idle for a short period.
  *)
  Module[{canStartSweep},
    canStartSweep =
      $DiagnosticsTask === None &&
      ListQ[$WorkspaceDiagnosticsSweepURIs] &&
      Length[$WorkspaceDiagnosticsSweepURIs] > 0 &&
      Length[$ContentQueue] == 0 &&
      AbsoluteTime[] - $QueueLastNonEmptyTime >= 1;

    If[canStartSweep,
      Module[{nextPos, nextURI},
        nextPos = SelectFirst[
          Range[Length[$WorkspaceDiagnosticsSweepURIs]],
          workspaceDiagnosticsSweepURIQ[$WorkspaceDiagnosticsSweepURIs[[#]]] &,
          Missing["NotFound"]
        ];
        If[IntegerQ[nextPos],
          nextURI = $WorkspaceDiagnosticsSweepURIs[[nextPos]];
          $WorkspaceDiagnosticsSweepURIs = Delete[$WorkspaceDiagnosticsSweepURIs, nextPos];
          loadFeatureModule["Diagnostics"];
          LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics[nextURI]
        ]
      ]
    ]
  ];

  (*
  If the client never acknowledged the workspace/semanticTokens/refresh request
  (no response received, or the response arrived with an unexpected id), the
  $PendingTokenRefresh flag stays True forever and blocks all future refreshes.
  Reset it after a 10-second timeout so the server can recover automatically.
  *)
  If[TrueQ[$PendingTokenRefresh] &&
     NumberQ[$PendingTokenRefreshTime] &&
     AbsoluteTime[] - $PendingTokenRefreshTime > 10,
    log[0, "DBG-ST: workspace/semanticTokens/refresh ack not received within 10s; resetting $PendingTokenRefresh"];
    $PendingTokenRefresh = False;
    $PendingTokenRefreshTime = None
  ];

  openFilesMapCopy = $OpenFilesMap;

  contents = {};
  KeyValueMap[
    Function[{uri, entry},
      jobs = Lookup[entry, "ScheduledJobs", {}];
      toRemoveIndices = {};
      Do[
        job = jobs[[j]];
        res = Catch[job[entry]];
        If[!MatchQ[res, {{___String}, True | False}],
          log[0, "WARNING: dropping scheduled job with invalid result for ", uri, ": ", res];
          AppendTo[toRemoveIndices, {j}];
          Continue[]
        ];
        {methods, toRemove} = res;

        contentsToAdd = <| "method" -> #, "params" -> <| "textDocument" -> <| "uri" -> uri |> |> |>& /@ methods;

        contents = contents ~Join~ contentsToAdd;

        If[toRemove,
          AppendTo[toRemoveIndices, {j}]
        ]
        ,
        {j, 1, Length[jobs]}
      ];
      If[toRemoveIndices =!= {},
        jobs = Delete[jobs, toRemoveIndices];
        entryCopy = entry;
        entryCopy["ScheduledJobs"] = jobs;
        $OpenFilesMap[uri] = entryCopy
      ]
    ]
    ,
    openFilesMapCopy
  ];

  If[contents =!= {},

    contents = expandContents[contents];

    appendContentsToContentQueue[contents];
  ]
]]


(*
input: JSON RPC assoc

returns: a list of JSON RPC assocs
*)
LSPEvaluate[content_(*no Association here, allow everything*)] :=
Catch[
Module[{contents},

  (*
  (*
  Figuring out what to with UTF-16 surrogates...

  Related bugs: 382744, 397941

  Related issues: https://github.com/microsoft/language-server-protocol/issues/376
  *)

  (*
  Coming in as JSON, so non-ASCII characters are using \uXXXX escaping
  So safe to treat bytes as ASCII
  *)
  str = FromCharacterCode[Normal[bytes], "ASCII"];

  escapes = StringCases[str, "\\u" ~~ ds : (_ ~~ _ ~~ _ ~~ _) :> ds];
  If[escapes != {},
    surrogates = Select[escapes, (
        (* high and low surrogates *)
        16^^d800 <= FromDigits[#, 16] <= 16^^dfff
      )&];
    If[surrogates != {},
      (*
      surrogates have been detected
      *)
      Null
    ]
  ];

  content = ImportString[str, "RawJSON"];
  *)

  Which[
    TrueQ[$ServerState == "shutdown"],
      contents = handleContentAfterShutdown[content]
    ,
    True,
      contents = handleContent[content]
  ];

  If[MatchQ[contents, Failure["URINotFound", _]],

    (*
    This can happen under some circumstances

    A file is closed, and something like publishDiagnostics is after the close in the queue

    Do not kill the kernel for this
    *)

    log[1, "\n\n"];
    log[1, "Internal assert 3 failed: list of Associations: ", contents];
    log[1, "\n\n"];

    Throw[{}]
  ];

  If[!MatchQ[contents, {_?AssociationQ ...}],
    log[0, "\n\n"];
    log[0, "Internal assert 4 failed: list of Associations: ", contents];
    log[0, "\n\n"];

    exitHard[]
  ];

  contents
]]



(*
  runDiagnostics expands to runFastDiagnostics which already publishes
  partial diagnostics.  A separate publishDiagnostics was re-reading the
  entry before workspace lints arrived, overwriting real results with an
  empty array.  Removed the redundant publishDiagnostics here;
  the workspace-diagnostics slow tier publishes a final update itself
  once it completes.
*)
$didOpenMethods = {
}


$didCloseMethods = {
  "textDocument/publishDiagnostics"
}


$didSaveMethods = {}


$didChangeMethods = {
  "textDocument/runDiagnostics"
}

$didChangeScheduledJobs = {}


RegisterDidOpenMethods[meths_] := ($didOpenMethods = Join[$didOpenMethods, meths])

RegisterDidCloseMethods[meths_] := ($didCloseMethods = Join[$didCloseMethods, meths])

RegisterDidSaveMethods[meths_] := ($didSaveMethods = Join[$didSaveMethods, meths])

RegisterDidChangeMethods[meths_] := ($didChangeMethods = Join[$didChangeMethods, meths])

RegisterDidOpenScheduledJobs[jobs_] := ($didOpenScheduledJobs = Join[$didOpenScheduledJobs, jobs])

RegisterDidCloseScheduledJobs[jobs_] := ($didCloseScheduledJobs = Join[$didCloseScheduledJobs, jobs])

RegisterDidSaveScheduledJobs[jobs_] := ($didSaveScheduledJobs = Join[$didSaveScheduledJobs, jobs])

RegisterDidChangeScheduledJobs[jobs_] := ($didChangeScheduledJobs = Join[$didChangeScheduledJobs, jobs])




(*
content: JSON-RPC Association

returns: a list of associations (possibly empty), each association represents JSON-RPC
*)
handleContent[content:KeyValuePattern["method" -> "initialize"]] :=
Module[{id, params, capabilities, textDocument, codeAction, codeActionLiteralSupport, codeActionKind, valueSet,
  codeActionProviderValue, initializationOptions, implicitTokens,
  bracketMatcher, debugBracketMatcher, clientName, semanticTokensProviderValue, inlayHintProviderValue,
  semanticTokens, contents, documentSymbol, hierarchicalDocumentSymbolSupport},

  log[1, "initialize: Enter"];

  id = content["id"];
  params = content["params"];

  If[KeyExistsQ[params, "initializationOptions"],

    initializationOptions = params["initializationOptions"];

    log[2, "initializationOptions: ", initializationOptions];

    (*
    initializationOptions may be Null, such as from Jupyter Lab LSP
    *)
    If[AssociationQ[initializationOptions],

      (*

      "confidenceLevel" initialization option is deprecated

      Use ConfidenceLevel option for StartServer

      If[KeyExistsQ[initializationOptions, "confidenceLevel"],
        $ConfidenceLevelInitialization = initializationOptions["confidenceLevel"]
      ];
      *)

      If[KeyExistsQ[initializationOptions, "implicitTokens"],
        implicitTokens = initializationOptions["implicitTokens"];

        $AllowedImplicitTokens = implicitTokens
      ];
      If[KeyExistsQ[initializationOptions, "bracketMatcher"],
        bracketMatcher = initializationOptions["bracketMatcher"];

        $BracketMatcher = bracketMatcher
      ];
      If[KeyExistsQ[initializationOptions, "debugBracketMatcher"],
        debugBracketMatcher = initializationOptions["debugBracketMatcher"];

        $DebugBracketMatcher = debugBracketMatcher
      ];
      If[KeyExistsQ[initializationOptions, "semanticTokens"],
        semanticTokens = initializationOptions["semanticTokens"];

        $SemanticTokens = semanticTokens
      ];
      If[KeyExistsQ[initializationOptions, "inlayHints"],
        $InlayHints = TrueQ[initializationOptions["inlayHints"]]
      ];
    ];

  log[1, "initialize: Exit"];
  ];

  (*
  Only use confidenceLevel from initializationOptions if no ConfidenceLevel option was passed to StartServer[]
  *)
  Which[
    NumberQ[$ConfidenceLevelOption],
      $ConfidenceLevel = $ConfidenceLevelOption
    ,
    (* NumberQ[$ConfidenceLevelInitialization],
      $ConfidenceLevel = $ConfidenceLevelInitialization
    , *)
    True,
      $ConfidenceLevel = $DefaultConfidenceLevel
  ];


  If[$Debug2,
    log["$AllowedImplicitTokens: ", $AllowedImplicitTokens];
    log["$BracketMatcher: ", $BracketMatcher];
    log["$DebugBracketMatcher: ", $DebugBracketMatcher];
    log["$ConfidenceLevel: ", $ConfidenceLevel];
    log["$SemanticTokens: ", $SemanticTokens];
    log["$InlayHints: ", $InlayHints]
  ];

  (*
  Extract workspace root path from params
  Try workspaceFolders first (LSP 3.6+), then fall back to rootUri/rootPath
  *)
  $WorkspaceRootPath = None;

  If[KeyExistsQ[params, "workspaceFolders"] && ListQ[params["workspaceFolders"]] && Length[params["workspaceFolders"]] > 0,
    $WorkspaceRootPath = normalizeURI[params["workspaceFolders"][[1]]["uri"]];
    If[$Debug2,
      log["workspace root from workspaceFolders: ", $WorkspaceRootPath]
    ]
    ,
    If[KeyExistsQ[params, "rootUri"] && StringQ[params["rootUri"]],
      $WorkspaceRootPath = normalizeURI[params["rootUri"]];
      If[$Debug2,
        log["workspace root from rootUri: ", $WorkspaceRootPath]
      ]
      ,
      If[KeyExistsQ[params, "rootPath"] && StringQ[params["rootPath"]],
        $WorkspaceRootPath = params["rootPath"];
        If[$Debug2,
          log["workspace root from rootPath: ", $WorkspaceRootPath]
        ]
      ]
    ]
  ];

  $ColorProvider = True;

  If[KeyExistsQ[params, "clientName"],
    clientName = params["clientName"];

    (*
    There are multiple problems with Eclipse here:

    Eclipse, or possibly the LSP4E plugin, has strange behavior where 100s or 1000s of documentColor messages
    are sent to the server.

    So we need to disable colorProvider for Eclipse

    Also, Eclipse sends the NON-STANDARD clientName as identification

    VERY ANNOYING!!
    *)
    If[clientName == "Eclipse IDE",
      $ColorProvider = False
    ]
  ];

  log[2, "$ColorProvider: ", $ColorProvider];


  capabilities = Lookup[params, "capabilities", <||>];
  textDocument = Lookup[capabilities, "textDocument", <||>];
  codeAction = Lookup[textDocument, "codeAction", <||>];

  If[KeyExistsQ[codeAction, "codeActionLiteralSupport"],
    $CodeActionLiteralSupport = True;
    codeActionLiteralSupport = codeAction["codeActionLiteralSupport"];
    codeActionKind = codeActionLiteralSupport["codeActionKind"];
    valueSet = codeActionKind["valueSet"]
  ];

  If[$CodeActionLiteralSupport,
    codeActionProviderValue = <| "codeActionKinds" -> {"quickfix"} |>
    ,
    codeActionProviderValue = True
  ];

  If[$AllowedImplicitTokens != {},

    RegisterDidOpenMethods[{
      "textDocument/runImplicitTokens",
      "textDocument/publishImplicitTokens"
    }];

    RegisterDidCloseMethods[{
      "textDocument/publishImplicitTokens"
    }];

    RegisterDidSaveMethods[{}];

    RegisterDidChangeMethods[{
      "textDocument/clearImplicitTokens",
      "textDocument/publishImplicitTokens"
    }];

    RegisterDidChangeScheduledJobs[{
      Function[{entry}, If[Now - entry["LastChange"] > Quantity[$ImplicitTokensDelayAfterLastChange, "Seconds"],
        {{
          "textDocument/runImplicitTokens",
          "textDocument/publishImplicitTokens"
        }, True},
        {{}, False}]
      ]
    }]
  ];

  If[$BracketMatcher,

    RegisterDidOpenMethods[{
      "textDocument/runBracketMismatches",
      "textDocument/suggestBracketEdits",
      "textDocument/publishBracketMismatches"
    }];

    RegisterDidCloseMethods[{
      "textDocument/publishBracketMismatches"
    }];

    RegisterDidSaveMethods[{}];

    RegisterDidChangeMethods[{
      "textDocument/clearBracketMismatches",
      "textDocument/publishBracketMismatches"
    }];

    RegisterDidChangeScheduledJobs[{
      Function[{entry}, If[Now - entry["LastChange"] > Quantity[$BracketMatcherDelayAfterLastChange, "Seconds"],
        {{
          "textDocument/runBracketMismatches",
          "textDocument/suggestBracketEdits",
          "textDocument/publishBracketMismatches"
        }, True},
        {{}, False}]
      ]
    }];

    $ExecuteCommandProvider =
      Merge[{$ExecuteCommandProvider, <|
        "commands" -> {
          (*
          enable_bracket_matcher_debug_mode is an undocumented, debug command
          *)
          "enable_bracket_matcher_debug_mode",
          (*
          disable_bracket_matcher_debug_mode is an undocumented, debug command
          *)
          "disable_bracket_matcher_debug_mode",
          (*
          enable_bracket_matcher_design_colors is an undocumented, debug command
          *)
          "enable_bracket_matcher_design_colors",
          (*
          disable_bracket_matcher_design_colors is an undocumented, debug command
          *)
          "disable_bracket_matcher_design_colors",
          (*
          enable_bracket_matcher_display_insertion_text is an undocumented, debug command
          *)
          "enable_bracket_matcher_display_insertion_text",
          (*
          disable_bracket_matcher_display_insertion_text is an undocumented, debug command
          *)
          "disable_bracket_matcher_display_insertion_text"
        }
      |>}, Flatten]
  ];

  If[$SemanticTokens,
    If[KeyExistsQ[textDocument, "semanticTokens"],
      loadFeatureModule["SemanticTokens"];
      semanticTokensProviderValue = <|
        "legend" -> <|
          "tokenTypes" -> Keys[LSPServer`SemanticTokens`$SemanticTokenTypes],
          "tokenModifiers" -> Keys[LSPServer`SemanticTokens`$SemanticTokenModifiers]
        |>,
        "range" -> False,
        "full" -> <| "delta" -> False |>
      |>
      ,
      (*
      if client does not advertise semantic token support, then do not respond with any support
      *)
      semanticTokensProviderValue = Null
    ];
    ,
    semanticTokensProviderValue = Null
  ];

  inlayHintProviderValue = If[TrueQ[$InlayHints],
    <| "resolveProvider" -> False |>,
    Null
  ];

  If[KeyExistsQ[textDocument, "documentSymbol"],
    documentSymbol = textDocument["documentSymbol"];
    hierarchicalDocumentSymbolSupport = Lookup[documentSymbol, "hierarchicalDocumentSymbolSupport", False];
    $HierarchicalDocumentSymbolSupport = TrueQ[hierarchicalDocumentSymbolSupport]
  ];

  $kernelInitializeTime = Now;

  log[2, "time to intialize: ", $kernelInitializeTime - $kernelStartTime];

  contents = {<| "jsonrpc" -> "2.0", "id" -> id,
    "result" -> <|
      "capabilities"-> <|
        "referencesProvider" -> True,
        "textDocumentSync" -> <|
          "openClose" -> True,
          "save" -> <| "includeText" -> False |>,
          "change" -> $TextDocumentSyncKind["Full"]
        |>,
        (* "completionProvider" -> <|
          "resolveProvider" -> False,
          "triggerCharacters" -> {}
        |>, *)
        "codeActionProvider" -> codeActionProviderValue,
        "colorProvider" -> $ColorProvider,
        "hoverProvider" -> True,
        "definitionProvider" -> True,
        "documentFormattingProvider" -> True,
        "documentRangeFormattingProvider" -> True,
        "executeCommandProvider" -> $ExecuteCommandProvider,
        "documentSymbolProvider" -> True,
        "selectionRangeProvider" -> True,
        "semanticTokensProvider" -> semanticTokensProviderValue,
        "foldingRangeProvider" -> True,
        (*
        Completion support
        triggerCharacters:
          $ - for system variables like $Version
          ` - for context paths like Developer`
          [ - for function arguments
          " - for association string keys like data["
        *)
        "completionProvider" -> <|
          "triggerCharacters" -> {"$", "`", "[", "\""},
          "resolveProvider" -> True
        |>,
        "inlayHintProvider" -> inlayHintProviderValue,
        (*
        Workspace symbol search support
        *)
        "workspaceSymbolProvider" -> True,
        (*
        Workspace folders support
        *)
        "workspace" -> <|
          "workspaceFolders" -> <|
            "supported" -> True,
            "changeNotifications" -> True
          |>
        |>
      |>
    |>
  |>};

  contents
]


handleContent[content:KeyValuePattern["method" -> "initialized"]] :=
Module[{warningMessages},


  log[1, "initialized: Enter"];


  (*
  Some simple thing to warm-up
  *)
  CodeParse["1+1"];

  If[$BracketMatcher,

    Block[{$ContextPath}, Needs["ML4Code`"]];

    (*
    Some simple thing to warm-up
    *)
    ML4Code`SuggestBracketEdits["f["];
  ];

  If[$SemanticTokens,
    loadFeatureModule["SemanticTokens"];
    (* Warm semantic-token symbol-category caches so the first highlight request
       does not pay the lazy initialization cost. *)
    LSPServer`SemanticTokens`Private`isSystemSymbol["Plot"];
    LSPServer`SemanticTokens`Private`isSystemConstant["Pi"];
    LSPServer`SemanticTokens`Private`isSystemOption["PlotRange"];
    LSPServer`SemanticTokens`Private`isExperimentalSymbol["Iconize"];
    LSPServer`SemanticTokens`Private`isObsoleteSymbol["DirectedInfinity"];
    LSPServer`SemanticTokens`Private`isUndocumentedSymbol["SequenceHold"];
    LSPServer`SemanticTokens`Private`isSessionSymbol["Print"];
    LSPServer`SemanticTokens`Private`isBadSymbol["SetDelayedDelayed"]
  ];

  (*
  Initialize paclet index if workspace root is set
  *)
  If[StringQ[$WorkspaceRootPath],
    (* Keep workspace bootstrap off the foreground queue until the loop has been
       idle briefly so startup didOpen and semantic-token requests land first. *)
    $WorkspaceBootstrapAfter = AbsoluteTime[] + 1
  ];

  warningMessages = ServerDiagnosticWarningMessages[];

  log[2, "warningMessages: ", warningMessages];

  res = <|
    "jsonrpc" -> "2.0",
    "method" -> "window/showMessage",
    "params" ->
      <|
        "type" -> $MessageType["Warning"],
        "message" -> #
      |>
  |>& /@ warningMessages;

  (* Schedule background kernel launch for 5 seconds from now.
     Deferring avoids blocking during VS Code's critical startup window. *)
  $DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 5;

  log[1, "initialized: Exit"];

  res
]


handleContent[content:KeyValuePattern["method" -> "workspace/bootstrapClosedFileDiagnostics"]] :=
Module[{nextPos, nextURI},


  log[1, "workspace/bootstrapClosedFileDiagnostics: Enter"];


  If[$DiagnosticsTask =!= None || !ListQ[$WorkspaceDiagnosticsSweepURIs] || $WorkspaceDiagnosticsSweepURIs === {},
    log[1, "workspace/bootstrapClosedFileDiagnostics: Exit"];
    Return[{}]
  ];

  If[$DiagnosticsKernel === None || $DiagnosticsKernel === $Failed,
    log[1, "workspace/bootstrapClosedFileDiagnostics: diagnostics kernel not ready; using synchronous fallback"]
  ];

  nextPos = SelectFirst[
    Range[Length[$WorkspaceDiagnosticsSweepURIs]],
    workspaceDiagnosticsSweepURIQ[$WorkspaceDiagnosticsSweepURIs[[#]]] &,
    Missing["NotFound"]
  ];

  If[IntegerQ[nextPos],
    nextURI = $WorkspaceDiagnosticsSweepURIs[[nextPos]];
    $WorkspaceDiagnosticsSweepURIs = Delete[$WorkspaceDiagnosticsSweepURIs, nextPos];
    loadFeatureModule["Diagnostics"];
    LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics[nextURI]
  ];

  log[1, "workspace/bootstrapClosedFileDiagnostics: Exit"];

  {}
]


handleContent[content:KeyValuePattern["method" -> "workspace/bootstrapWorkspaceIndex"]] :=
Module[{},


  log[1, "workspace/bootstrapWorkspaceIndex: Enter"];


  If[!StringQ[$WorkspaceRootPath],
    log[1, "workspace/bootstrapWorkspaceIndex: Exit"];
    Return[{}]
  ];

  If[$Debug2,
    log["initializing paclet index for: ", $WorkspaceRootPath]
  ];
  InitializePacletIndex[$WorkspaceRootPath];

  (*
  Load project-level ignore configuration (.wllintrc)
  *)
  If[$Debug2,
    log["loading project ignore config"]
  ];
  LoadProjectIgnoreConfig[$WorkspaceRootPath];
  queueWorkspaceDiagnosticsSweep[];

  log[1, "workspace/bootstrapWorkspaceIndex: Exit"];

  {}
]


handleContent[content:KeyValuePattern["method" -> "shutdown"]] :=
Catch[
Module[{id},


  log[1, "shutdown: Enter"];


  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["$CancelMap: ", $CancelMap]
    ];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  $DiagnosticsKernelLaunchAfter = None;
  $WorkspaceBootstrapAfter = None;
  clearHoverTaskState[];
  cleanupDiagnosticsWorker[];

  $OpenFilesMap =.;

  $ServerState = "shutdown";

  log[1, "shutdown: Exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}
]]

(*
Unexpected call to exit
*)
handleContent[content:KeyValuePattern["method" -> "exit"]] :=
Module[{},


  log[1, "exit: Enter"];
  log[1, "exit: Exit"];


  exitSemiGracefully[]
]


handleContent[content:KeyValuePattern["method" -> "$/cancelRequest"]] :=
Catch[
Module[{params, id},

  If[$Debug2,
    log["$/cancelRequest: enter"]
  ];

  params = content["params"];

  id = params["id"];

  If[!KeyExistsQ[$CancelMap, id],
    Throw[{}]
  ];

  log[2, "cancel was not handled: ", id];

  $CancelMap[id] =.;

  log[2, "$CancelMap: ", $CancelMap];

  log[1, "$/cancelRequest: exit"];

  {}
]]

(*
$ Notifications and Requests

Notification and requests whose methods start with "$/" are messages which are protocol
implementation dependent and might not be implementable in all clients or servers.
For example if the server implementation uses a single threaded synchronous programming
language then there is little a server can do to react to a "$/cancelRequest" notification.
If a server or client receives notifications starting with "$/" it is free to ignore the
notification.
If a server or client receives a requests starting with "$/" it must error the request with
error code MethodNotFound (e.g. -32601).
*)
handleContent[content:KeyValuePattern["method" -> meth_ /; StringMatchQ[meth, "$/" ~~ __]]] :=
Module[{id},


  log[1, meth <> ": enter"];


  If[KeyExistsQ[content, "id"],
    (*
    has id, so this is a request
    *)
    id = content["id"];
    {<| "jsonrpc" -> "2.0", "id" -> id,
      "error" -> <|
        "code" -> $ErrorCodes["MethodNotFound"],
        "message"->"Method Not Found" |> |>}
    ,
    (*
    does not have id, so this is a notification
    something like: $/setTraceNotification
    $/cancelRequest is handled elsewhere
    just ignore
    *)
    {}
  ]
]


handleContent[content : KeyValuePattern["method" -> method_String]] /;
    KeyExistsQ[$FeatureHandleMethods, method] &&
    !TrueQ[Lookup[$FeatureModulesLoaded, First[$FeatureHandleMethods[method]], False]] :=
  Module[{},
    loadFeatureModulesForMethod[method];
    handleContent[content]
  ]


(*
Handle responses to server-initiated requests (e.g. workspace/semanticTokens/refresh).
These have no "method" key — just "id" and "result" (or "error").
Without this handler, LSPEvaluate would see an unevaluated handleContent[...] and call exitHard[].
*)
handleContent[content_?AssociationQ] /; !KeyExistsQ[content, "method"] :=
Module[{id},
  id = Lookup[content, "id", None];
  If[IntegerQ[id] && id < 0,
    $PendingTokenRefresh = False;
    $PendingTokenRefreshTime = None
  ];
  {}
]


rememberPendingSemanticTokenRequest[uri_String, id_Integer] :=
Module[{ids, supersededIDs},
  If[!AssociationQ[$PendingSemanticTokenRequests],
    $PendingSemanticTokenRequests = <||>
  ];
  ids = Lookup[$PendingSemanticTokenRequests, uri, {}];
  supersededIDs = DeleteCases[ids, id];
  $PendingSemanticTokenRequests[uri] = {id};
  supersededIDs
]


dropQueuedSemanticTokenFenceposts[uri_String, ids_List] :=
  If[ids =!= {},
    $ContentQueue = Select[
      Replace[$ContentQueue, Except[_List] -> {}],
      !(
        AssociationQ[#] &&
        Lookup[#, "method", None] === "textDocument/semanticTokens/fullFencepost" &&
        MemberQ[ids, Lookup[#, "id", None]] &&
        Lookup[Lookup[Lookup[#, "params", <||>], "textDocument", <||>], "uri", None] === uri
      ) &
    ]
  ]


supersededSemanticTokenFencepostContents[uri_String, ids_List] :=
  <|
    "method" -> "textDocument/semanticTokens/fullFencepost",
    "id" -> #,
    "params" -> <|"textDocument" -> <|"uri" -> uri|>|>,
    "superseded" -> True,
    "priority" -> True
  |>& /@ ids


forgetPendingSemanticTokenRequest[uri_String, id_Integer] :=
Module[{ids},
  If[!AssociationQ[$PendingSemanticTokenRequests],
    $PendingSemanticTokenRequests = <||>
  ];
  ids = DeleteCases[Lookup[$PendingSemanticTokenRequests, uri, {}], id];
  If[ids === {},
    $PendingSemanticTokenRequests = KeyDrop[$PendingSemanticTokenRequests, uri]
  ,
    $PendingSemanticTokenRequests[uri] = ids
  ]
]


pendingSemanticTokenFencepostIDsToRecover[uri_String] :=
Module[{pendingIDs, queuedFencepostIDs},
  If[!AssociationQ[$PendingSemanticTokenRequests],
    $PendingSemanticTokenRequests = <||>
  ];

  pendingIDs = Lookup[$PendingSemanticTokenRequests, uri, {}];
  queuedFencepostIDs = Cases[
    $ContentQueue,
    KeyValuePattern[{
      "method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> pendingID_,
      "params" -> KeyValuePattern["textDocument" -> KeyValuePattern["uri" -> uri]]
    }] :> pendingID
  ];

  Complement[pendingIDs, queuedFencepostIDs]
]


pendingSemanticTokenFencepostResponses[uri_String, ids_List] :=
  Flatten[
    Function[{pendingID},
      handleContent[
        <|
          "method" -> "textDocument/semanticTokens/fullFencepost",
          "id" -> pendingID,
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      ]
    ] /@ ids,
    1
  ]


recoverPendingSemanticTokenFenceposts[uri_String, reason_String:""] :=
Module[{idsToRecover},
  If[!TrueQ[$SemanticTokens],
    Return[{}]
  ];

  idsToRecover = pendingSemanticTokenFencepostIDsToRecover[uri];

  If[idsToRecover === {},
    Return[{}]
  ];

  If[reason =!= "",
    log[0, reason, " recovered=", Length[idsToRecover], " uri=", uri]
  ];

  pendingSemanticTokenFencepostResponses[uri, idsToRecover]
]


queuePendingSemanticTokenFenceposts[uri_String, reason_String:""] :=
Module[{idsToRecover},
  If[!TrueQ[$SemanticTokens],
    Return[0]
  ];

  idsToRecover = pendingSemanticTokenFencepostIDsToRecover[uri];

  If[idsToRecover === {},
    Return[0]
  ];

  If[reason =!= "",
    log[0, reason, " recovered=", Length[idsToRecover], " uri=", uri]
  ];

  appendContentsToContentQueue[
    <|
      "method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> #,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>,
      "priority" -> False,
      "deferrable" -> True
    |>& /@ idsToRecover
  ];

  Length[idsToRecover]
]


(*
semanticTokensRefreshQueuedQ[] was a linear scan of $ContentQueue used to avoid
double-queuing a workspace/semanticTokens/refresh.  It has been replaced by the
$PendingTokenRefresh flag (see queueSemanticTokensRefresh below), so this
function is kept only as a no-op compatibility stub.
*)
semanticTokensRefreshQueuedQ[] := False


queueSemanticTokensRefresh[reason_String:""] :=
  If[$SemanticTokens && !TrueQ[$PendingTokenRefresh],
    If[reason =!= "",
      log[0, reason]
    ];
    (* Set the flag immediately so subsequent calls within the same event-loop
       tick don't enqueue a second refresh.  The flag stays True until the
       client acknowledges the request (or the 10-second timeout fires). *)
    $PendingTokenRefresh = True;
    $PendingTokenRefreshTime = AbsoluteTime[];
    AppendTo[$ContentQueue, <|"method" -> "workspace/semanticTokens/refresh"|>]
  ]


(*
Send workspace/semanticTokens/refresh to tell VS Code to re-fetch tokens for all
open files.

This handler must stay cheap. It should only invalidate cached tokens and queue
pending semantic-token fenceposts; expensive token computation happens later in
the normal semanticTokens/fullFencepost path or when the client re-requests.
Uses a negative server-generated id to avoid colliding with client request ids.
*)
handleContent[content:KeyValuePattern["method" -> "workspace/semanticTokens/refresh"]] :=
Module[{id, invalidated = 0, recovered = 0},
  Scan[
    Function[{uri},
      Module[{entry = Lookup[$OpenFilesMap, uri, Null], recoveredCount = 0},
        If[AssociationQ[entry],
          If[KeyExistsQ[entry, "SemanticTokens"],
            (* Clear cached tokens so VS Code fetches fresh ones. *)
            $OpenFilesMap[uri] = KeyDrop[entry, "SemanticTokens"];
            invalidated += 1
          ];

          recoveredCount = queuePendingSemanticTokenFenceposts[
            uri,
            "DBG-ST: refresh queued pending semantic-token fenceposts"
          ];
          recovered += recoveredCount
        ]
      ]
    ],
    Keys[$OpenFilesMap]
  ];

  $InternalRequestId -= 1;
  id = $InternalRequestId;
  $PendingTokenRefresh = True;
  $PendingTokenRefreshTime = AbsoluteTime[];
  log[0, "DBG-ST: sending workspace/semanticTokens/refresh id=", id,
    " invalidated=", invalidated,
    " recovered=", recovered];
  {<| "jsonrpc" -> "2.0", "id" -> id, "method" -> "workspace/semanticTokens/refresh" |>}
]


handleContentAfterShutdown[content:KeyValuePattern["method" -> "exit"]] :=
Module[{},

  log[1, "exit after shutdown: enter"];

  log[1, "exit after shutdown: exit"];

  exitGracefully[]
]

(*
Called if any requests or notifications come in after shutdown
*)
handleContentAfterShutdown[content_?AssociationQ] :=
Module[{id},

  log[1, "message after shutdown: enter: ", #["method"]&[content]];

  log[1, "message after shutdown: exit: "];

  If[KeyExistsQ[content, "id"],
    (*
    has id, so this is a request
    *)
    id = content["id"];
    {<| "jsonrpc" -> "2.0", "id" -> id,
      "error" -> <|
        "code" -> $ErrorCodes["InvalidRequest"],
        "message" -> "Invalid request" |> |>}
    ,
    (*
    does not have id, so this is a notification
    just ignore
    *)
    {}
  ]
]


expandContent[content:KeyValuePattern["method" -> "textDocument/didOpen"], pos_] :=
Catch[
Module[{params, doc, uri, res},

  log[1, "textDocument/didOpen: enter expand"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/didOpenFencepost", "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ ({
      "textDocument/didOpenFencepost"
    } ~Join~ $didOpenMethods);

  log[1, "textDocument/didOpen: Exit"];

  res
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/didOpenFencepost"]] :=
Catch[
Module[{params, doc, uri, text, entry},

  If[$Debug2,
    log["textDocument/didOpenFencepost: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  text = doc["text"];

  entry = <|
    "Text" -> text,
    "LastChange" -> Now,
    "ScheduledJobs" -> {},
    "IndexUpdatePending" -> True
  |>;

  (* Pre-process .ipwl files so the parse handlers use annotation-free source *)
  If[StringEndsQ[uri, ".ipwl"],
    entry["PreprocessedText"] = LSPServer`TypeWL`PreprocessIPWL[text][[1]]
  ];

  $OpenFilesMap[uri] = entry;

  appendContentsToContentQueue[{
    <|"method" -> "textDocument/runOpenIndexUpdate", "params" -> <|"textDocument" -> <|"uri" -> uri|>|>|>
  }];

  log[1, "textDocument/didOpenFencepost: Exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/runOpenIndexUpdate"]] :=
Catch[
Module[{params, doc, uri, entry, text, parseResult, curEntry},

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  entry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
  If[!AssociationQ[entry],
    Throw[{}]
  ];

  text = Lookup[entry, "Text", Missing["NotAvailable"]];
  If[!StringQ[text],
    Throw[{}]
  ];

  If[isStale[$ContentQueue, uri],
    Throw[{}]
  ];

  If[TrueQ[yieldToInteractiveRequests[uri]],
    log[1, "runOpenIndexUpdate: stale after yield, aborting"];
    Throw[{}]
  ];
  If[isStale[$ContentQueue, uri], Throw[{}]];

  parseResult = UpdateFileIndex[uri, text];

  yieldToInteractiveRequests[uri];
  If[isStale[$ContentQueue, uri], Throw[{}]];

  If[ListQ[parseResult] && Length[parseResult] == 3,
    curEntry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
    If[AssociationQ[curEntry] && Lookup[curEntry, "LastChange", Missing["NotAvailable"]] === entry["LastChange"],
      curEntry = KeyDrop[curEntry, "IndexUpdatePending"];
      curEntry["CST"] = parseResult[[1]];
      If[!StringContainsQ[text, "\t"], curEntry["CSTTabs"] = parseResult[[1]]];
      curEntry["Agg"] = parseResult[[2]];
      curEntry["AST"] = parseResult[[3]];
      curEntry["PreviousAST"] = parseResult[[3]];
      With[{syms = findAllUserSymbols[parseResult[[3]]]},
        curEntry["UserSymbols"]         = syms;
        curEntry["PreviousUserSymbols"] = syms
      ];
      $OpenFilesMap[uri] = curEntry
    ];

    If[AssociationQ[Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]]] &&
       Lookup[$OpenFilesMap[uri], "LastChange", Missing["NotAvailable"]] === entry["LastChange"],
      If[$SemanticTokens,
        queuePendingSemanticTokenFenceposts[
          uri,
          "DBG-ST: didOpen indexed; queuing pending semantic-token fenceposts"
        ]
      ];
      appendContentsToContentQueue[{
        <|
          "method" -> "textDocument/runFastDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      }]
    ]
  ,
    curEntry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
    If[AssociationQ[curEntry] && Lookup[curEntry, "LastChange", Missing["NotAvailable"]] === entry["LastChange"],
      $OpenFilesMap[uri] = KeyDrop[curEntry, "IndexUpdatePending"]
    ]
  ];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/concreteParse"]] :=
Catch[
Module[{params, doc, uri, cst, text, entry, fileFormat},

  log[1, "textDocument/concreteParse: Enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  cst = Lookup[entry, "CST", Null];

  If[cst =!= Null,
    Throw[{}]
  ];

  text = Lookup[entry, "PreprocessedText", entry["Text"]];

  If[$Debug2,
    log["text: ", stringLineTake[StringTake[ToString[text, InputForm], UpTo[1000]], UpTo[20]]];
    log["...\n"]
  ];

  If[$Debug2,
    log["before CodeConcreteParse"]
  ];

  fileFormat = LSPServer`SourceFileFormat[uri];

  cst = CodeConcreteParse[text, "FileFormat" -> fileFormat];

  log[2, "after CodeConcreteParse"];

  If[FailureQ[cst],

    (*
    It is possible that a file is open in an editor, the actual file system contents get deleted,
    but the editor still has a stale window open.
    Focusing on that window could trigger a textDocument/didOpen notification, but the file does not exist!
    TODO: is this a bug in Sublime / LSP package?
    *)
    If[MatchQ[cst, Failure["FindFileFailed", _]],
      Throw[{}]
    ];

    Throw[cst]
  ];

  cst[[1]] = File;

  entry["CST"] = cst;

  (*
  save time if the file has no tabs
  *)
  If[!StringContainsQ[text, "\t"],
    entry["CSTTabs"] = cst
  ];

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/concreteParse: Exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/concreteTabsParse"]] :=
Catch[
Module[{params, doc, uri, text, entry, cstTabs, fileFormat},


  log[1, "textDocument/concreteTabsParse: enter"];


  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  cstTabs = Lookup[entry, "CSTTabs", Null];

  If[cstTabs =!= Null,
    Throw[{}]
  ];

  text = Lookup[entry, "PreprocessedText", entry["Text"]];

  (*
  Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
  FIXME: Must use the tab width from the editor
  *)

  log[2, "before CodeConcreteParse (TabWidth 4)"];

  fileFormat = LSPServer`SourceFileFormat[uri];

  cstTabs = CodeConcreteParse[text, "TabWidth" -> 4, "FileFormat" -> fileFormat];

  log[2, "after CodeConcreteParse (TabWidth 4)"];

  If[FailureQ[cstTabs],

    (*
    It is possible that a file is open in an editor, the actual file system contents get deleted,
    but the editor still has a stale window open.
    Focusing on that window could trigger a textDocument/didOpen notification, but the file does not exist!
    TODO: is this a bug in Sublime / LSP package?
    *)
    If[MatchQ[cstTabs, Failure["FindFileFailed", _]],
      Throw[{}]
    ];

    Throw[cstTabs]
  ];

  cstTabs[[1]] = File;

  entry["CSTTabs"] = cstTabs;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/concreteTabsParse: exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/aggregateParse"]] :=
Catch[
Module[{params, doc, uri, cst, text, entry, agg},


  log[1, "textDocument/aggregateParse: Enter"];


  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  text = entry["Text"];

  agg = Lookup[entry, "Agg", Null];

  If[agg =!= Null,
    Throw[{}]
  ];

  cst = Lookup[entry, "CST", Null];

  If[cst === Null || MissingQ[cst],
    Throw[{}]
  ];

  If[$Debug2,
    log["before Aggregate"]
  ];

  agg = CodeParser`Abstract`Aggregate[cst];

  log[2, "after Aggregate"];

  entry["Agg"] = agg;

  (*
  save time if the file has no tabs
  *)
  If[!StringContainsQ[text, "\t"],
    entry["AggTabs"] = agg
  ];

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/aggregateParse: Exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/aggregateTabsParse"]] :=
Catch[
Module[{params, doc, uri, entry, cstTabs, aggTabs},


  log[1, "textDocument/aggregateTabsParse: enter"];


  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  aggTabs = Lookup[entry, "AggTabs", Null];

  If[aggTabs =!= Null,
    Throw[{}]
  ];

  cstTabs = entry["CSTTabs"];

  (*
  Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
  FIXME: Must use the tab width from the editor
  *)

  log[2, "before Aggregate"];

  aggTabs = CodeParser`Abstract`Aggregate[cstTabs];

  log[2, "after Aggregate"];

  If[FailureQ[aggTabs],
    Throw[aggTabs]
  ];

  entry["AggTabs"] = aggTabs;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/aggregateTabsParse: exit"];

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/abstractParse"]] :=
Catch[
Module[{params, doc, uri, entry, agg, ast, userSymbols},


  log[1, "textDocument/abstractParse: enter"];


  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  ast = Lookup[entry, "AST", Null];

  If[ast =!= Null,
    Throw[{}]
  ];

  agg = Lookup[entry, "Agg", Null];

  If[agg === Null || MissingQ[agg] || FailureQ[agg],
    Throw[{}]
  ];

  If[$Debug2,
    log["before Abstract"]
  ];

  ast = CodeParser`Abstract`Abstract[agg];

  userSymbols = findAllUserSymbols[ast];

  log[2, "after Abstract"];

  entry["AST"] = ast;
  entry["PreviousAST"] = ast;

  entry["UserSymbols"] = userSymbols;
  entry["PreviousUserSymbols"] = userSymbols;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/abstractParse: exit"];

  {}
]]


findAllUserSymbols[ast_] := DeleteDuplicates[
  Cases[ast,
    {
      CallNode[
        LeafNode[Symbol, "SetDelayed" | "Set", <||>],
        {CallNode[LeafNode[Symbol, sym_, _], _, _], rhs : _} |
        {LeafNode[Symbol, sym_, _], rhs : _},
      _],
    _} :> sym,
  8] (* Same depth used in finding function call pattern in Hover feature *)
]


expandContent[content:KeyValuePattern["method" -> "textDocument/didClose"], pos_] :=
Catch[
Module[{params, doc, uri, res},


  log[1, "textDocument/didClose: enter expand"];


  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/didCloseFencepost", "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ ({
      "textDocument/didCloseFencepost"
    } ~Join~ $didCloseMethods);

  log[1, "textDocument/didClose: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/didCloseFencepost"]] :=
Module[{params, doc, uri, beforeQueueLen, dropped, entry, notification},


  log[1, "textDocument/didCloseFencepost: Enter"];


  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[AssociationQ[entry],
    loadFeatureModule["Diagnostics"];
    notification = LSPServer`Diagnostics`Private`buildPublishNotification[
      uri,
      entry,
      LSPServer`Diagnostics`Private`allEntryDiagnosticsLints[entry]
    ];
    If[!AssociationQ[$ClosedFileDiagnosticsNotifications],
      $ClosedFileDiagnosticsNotifications = <||>
    ];
    $ClosedFileDiagnosticsNotifications[uri] = notification
  ];

  $OpenFilesMap[uri] =.;

  queueWorkspaceDiagnosticsSweep[{uri}];

  beforeQueueLen = Length[$ContentQueue];
  $ContentQueue = Select[
    $ContentQueue,
    !(
      Lookup[Lookup[Lookup[#, "params", <||>], "textDocument", <||>], "uri", None] === uri &&
      !MemberQ[$didCloseMethods, Lookup[#, "method", None]]
    ) &
  ];
  dropped = beforeQueueLen - Length[$ContentQueue];

  If[AssociationQ[$PendingSemanticTokenRequests],
    $PendingSemanticTokenRequests = KeyDrop[$PendingSemanticTokenRequests, uri]
  ];

  If[dropped > 0,
    log[0, "DBG-ST: didClose purged queued uri work dropped=", dropped, " uri=", uri]
  ];

  (*
  Clean up ignore pattern data for this file to prevent memory leaks
  *)
  ClearIgnoreData[uri];

  (*
  Note: We don't remove from paclet index on close, because the file still exists
  and we want workspace-wide features to still work for closed files.
  The index is only updated when files are actually modified.
  *)

  log[1, "textDocument/didCloseFencepost: Exit"];

  {}
]



expandContent[content:KeyValuePattern["method" -> "textDocument/didSave"], pos_] :=
Catch[
Module[{params, doc, uri},


  log[1, "textDocument/didSave: Enter"];


  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/didSaveFencepost", "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ ({
      "textDocument/didSaveFencepost"
    } ~Join~ $didSaveMethods);

  log[1, "textDocument/didSave: Exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/didSaveFencepost"]] :=
Module[{},


    log[1, "textDocument/didSaveFencepost: Enter"];

    log[1, "textDocument/didSaveFencepost: Exit"];


  {}
]



expandContent[content:KeyValuePattern["method" -> "textDocument/didChange"], pos_] :=
Catch[
Module[{params, doc, uri, res},


  log[1, "textDocument/didChange: enter expand"];


  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/didChangeFencepost", "params" -> params, "stale" -> True |>}]
  ];

  res = Join[
    {<| "method" -> "textDocument/didChangeFencepost", "params" -> params |>},
    Map[
      Function[{method},
        If[MemberQ[{"textDocument/runDiagnostics", "textDocument/publishDiagnostics"}, method],
          <| "method" -> method, "params" -> params, "priority" -> True |>,
          <| "method" -> method, "params" -> params |>
        ]
      ],
      $didChangeMethods
    ]
  ];

  log[1, "textDocument/didChange: Exit"];

  res

]]


handleContent[content:KeyValuePattern["method" -> "textDocument/didChangeFencepost"]] :=
Catch[
Module[{params, doc, uri, text, lastChange, entry, changes, oldEntry,
  previousUserSymbols, changedSymbols, affectedOpenURIs},

  If[$Debug2,
    log["textDocument/didChangeFencepost: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],

    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  (* Cancel any in-flight slow-tier diagnostics task — its content is now stale *)
  cancelCurrentDiagnosticsTask[];

  If[$HoverTask =!= None && $HoverTaskURI === uri,
    cancelCurrentHoverTask[]
  ];

  changes = params["contentChanges"];

  (*
  Currently only supporting full text, so always only apply the last change
  *)
  lastChange = changes[[-1]];

  text = lastChange["text"];

  oldEntry = Lookup[$OpenFilesMap, uri, <||>];
  previousUserSymbols = Replace[
    Lookup[oldEntry, "PreviousUserSymbols", Lookup[oldEntry, "UserSymbols", {}]],
    Except[_List] -> {}
  ];

  (*
      We do not want to keep entry["AST"] here. As the text is changed, AST needs to be re-evaluated.

      But for fast response to the Completion messages, we can use the backdated AST.

      If there are multiple didChangeFencepost messages in the queue,
          "PreviousAST" -> entry["AST"]
      would break because from the second message onwards, entry["AST"] would be Missing.

      So it's better to assign newly evaluated AST to entry["PreviousAST"] and use it as long as new AST is is not re-evaluated.
  *)

  entry = <|
    "Text" -> text,
    "LastChange" -> Now,
    "ScheduledJobs" -> $didChangeScheduledJobs,
    "IndexUpdatePending" -> True,
    "PreviousAST" -> Lookup[oldEntry, "PreviousAST", Lookup[oldEntry, "AST", Missing["NotAvailable"]]],
    "PreviousUserSymbols" -> Lookup[oldEntry, "PreviousUserSymbols", Lookup[oldEntry, "UserSymbols", Missing["NotAvailable"]]]
  |>;

  (* Pre-process .ipwl files so the parse handlers use annotation-free source *)
  If[StringEndsQ[uri, ".ipwl"],
    entry["PreprocessedText"] = LSPServer`TypeWL`PreprocessIPWL[text][[1]]
  ];

  $OpenFilesMap[uri] = entry;

  (*
  Schedule paclet index update (debounced with other scheduled jobs).
  The job queues a "textDocument/runIndexUpdate" content message instead of
  running UpdateFileIndex inline, so the main loop can interleave interactive
  requests (hover, completion) between the fencepost and the index update.
  *)
  AppendTo[entry["ScheduledJobs"],
    Function[{e}, If[Now - e["LastChange"] > Quantity[$DiagnosticsDelayAfterLastChange, "Seconds"],
      {{"textDocument/runIndexUpdate"}, True},
      {{}, False}]
    ]
  ];
  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/didChangeFencepost: Exit"];

  {}
]]


(*
handleContent for the deferred index update. This runs as a normal queue item
so interactive requests (hover, completion) can be promoted ahead of it by
takeFirstContentQueueItem.
*)
handleContent[content:KeyValuePattern["method" -> "textDocument/runIndexUpdate"]] :=
Catch[
Module[{params, doc, uri, entry, text, parseResult, curEntry,
  previousUserSymbols, changedSymbols, affectedOpenURIs},

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  entry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
  If[!AssociationQ[entry],
    Throw[{}]
  ];

  text = Lookup[entry, "Text", Missing["NotAvailable"]];
  If[!StringQ[text],
    Throw[{}]
  ];

  previousUserSymbols = Replace[
    Lookup[entry, "PreviousUserSymbols", Lookup[entry, "UserSymbols", {}]],
    Except[_List] -> {}
  ];

  (* If there is a newer didChange for this URI queued, skip — it will trigger
     its own index update. *)
  If[isStale[$ContentQueue, uri],
    Throw[{}]
  ];

  (* ── YIELD POINT: serve any interactive requests before heavy index work ── *)
  If[TrueQ[yieldToInteractiveRequests[uri]],
    log[1, "runIndexUpdate: stale after yield, aborting"];
    Throw[{}]
  ];
  If[isStale[$ContentQueue, uri], Throw[{}]];

  parseResult = UpdateFileIndex[uri, text];

  (* ── YIELD POINT: serve interactive requests after index work ── *)
  yieldToInteractiveRequests[uri];
  If[isStale[$ContentQueue, uri], Throw[{}]];

  If[ListQ[parseResult] && Length[parseResult] == 3,
    curEntry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
    If[AssociationQ[curEntry] && Lookup[curEntry, "LastChange", Missing["NotAvailable"]] === entry["LastChange"],
      curEntry = KeyDrop[curEntry, "IndexUpdatePending"];
      curEntry["CST"] = parseResult[[1]];
      If[!StringContainsQ[text, "\t"], curEntry["CSTTabs"] = parseResult[[1]]];
      curEntry["Agg"] = parseResult[[2]];
      curEntry["AST"] = parseResult[[3]];
      curEntry["PreviousAST"] = parseResult[[3]];
      With[{syms = findAllUserSymbols[parseResult[[3]]]},
        curEntry["UserSymbols"]         = syms;
        curEntry["PreviousUserSymbols"] = syms
      ];
      $OpenFilesMap[uri] = curEntry
    ,
      log[0, "DBG-ST: didChange index result stale; skipping refresh for ", uri]
    ];
    If[AssociationQ[Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]]] &&
       Lookup[$OpenFilesMap[uri], "LastChange", Missing["NotAvailable"]] === entry["LastChange"],
      changedSymbols = DeleteDuplicates[Join[
        previousUserSymbols,
        Replace[Lookup[$OpenFilesMap[uri], "UserSymbols", {}], Except[_List] -> {}]
      ]];
      affectedOpenURIs = DeleteDuplicates[Join[
        {uri},
        openFilesAffectedByDefinitions[changedSymbols]
      ]];
      Scan[
        loadFeatureModule["Diagnostics"];
        LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics,
        affectedOpenURIs
      ];
      queueWorkspaceDiagnosticsSweep[];
      If[$SemanticTokens,
        queuePendingSemanticTokenFenceposts[
          uri,
          "DBG-ST: didChange indexed; queuing pending semantic-token fenceposts"
        ]
      ];
      appendContentsToContentQueue[{
        <|
          "method" -> "textDocument/runFastDiagnostics",
          "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
        |>
      }]
    ]
  ,
    curEntry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
    If[AssociationQ[curEntry] && Lookup[curEntry, "LastChange", Missing["NotAvailable"]] === entry["LastChange"],
      $OpenFilesMap[uri] = KeyDrop[curEntry, "IndexUpdatePending"]
    ]
  ];

  {}
]]


exitGracefully[] := (
  log[0, "\n\n"];
  log[0, "KERNEL IS EXITING GRACEFULLY"];
  log[0, "\n\n"];
  cleanupDiagnosticsWorker[];
  shutdownLSPComm[$commProcess, $initializedComm];
  (
  (* :!CodeAnalysis::BeginBlock:: *)
  (* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
  Exit[0]
  (* :!CodeAnalysis::EndBlock:: *)
  )
)

exitSemiGracefully[] := (
  log[0, "Language Server kernel did not shutdown properly."];
  log[0, ""];
  log[0, "This is the command that was used:"];
  log[0, $CommandLine];
  log[0, ""];
  log[0, "To help diagnose the problem, run this in a notebook:\n" <>
  "Needs[\"LSPServer`\"]\n" <>
  "LSPServer`RunServerDiagnostic[{" <>
    StringJoin[Riffle[("\"" <> # <> "\"")& /@ StringReplace[$CommandLine, "\"" -> "\\\""], ", "]] <>
    "}]"];
  log[0, ""];
  log[0, "Fix any problems then restart and try again."];
  log[0, "\n\n"];
  log[0, "KERNEL IS EXITING SEMI-GRACEFULLY"];
  log[0, "\n\n"];
  cleanupDiagnosticsWorker[];
  shutdownLSPComm[$commProcess, $initializedComm];
  (
  (* :!CodeAnalysis::BeginBlock:: *)
  (* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
  Exit[1]
  (* :!CodeAnalysis::EndBlock:: *)
  )
)

exitHard[] := (
  log[0, "Language Server kernel did not shutdown properly."];
  log[0, ""];
  log[0, "This is the command that was used:"];
  log[0, $CommandLine];
  log[0, ""];
  log[0, "To help diagnose the problem, run this in a notebook:\n" <>
  "Needs[\"LSPServer`\"]\n" <>
  "LSPServer`RunServerDiagnostic[{" <>
    StringJoin[Riffle[("\"" <> # <> "\"")& /@ StringReplace[$CommandLine, "\"" -> "\\\""], ", "]] <>
    "}]"];
  log[0, ""];
  log[0, "Fix any problems then restart and try again."];
  log[0, "\n\n"];
  log[0, "KERNEL IS EXITING HARD"];
  log[0, "\n\n"];
  cleanupDiagnosticsWorker[];
  shutdownLSPComm[$commProcess, $initializedComm];
  (
  (* :!CodeAnalysis::BeginBlock:: *)
  (* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
  Exit[1]
  (* :!CodeAnalysis::EndBlock:: *)
  )
)


(*
now cleanup Startup Messages handling
*)
Module[{name, startupMessagesText},

  If[!FailureQ[$startupMessagesFile],

    name = Quiet[Check[Close[$startupMessagesFile], $Failed]];

    startupMessagesText = If[StringQ[name] && FileExistsQ[name],
      Replace[Quiet[Check[Import[name, "Text"], ""]], Except[_String] -> ""],
      ""
    ];

    If[StringQ[name] && FileExistsQ[name],
      Quiet[Check[DeleteFile[name], Null]]
    ];

    $startupMessagesText = startupMessagesText;
    $Messages = $oldMessages
  ]
]


End[]

EndPackage[]
