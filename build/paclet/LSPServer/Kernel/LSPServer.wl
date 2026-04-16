(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["LSPServer`"]

StartServer::usage = "StartServer[] puts the kernel into a state ready for traffic from the client.\
 StartServer[logDir] logs traffic to logDir."

RunServerDiagnostic

initializeLSPComm

expandContent

expandContentsAndAppendToContentQueue

LSPEvaluate
readEvalWriteLoop

handleContent
handleContentAfterShutdown

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
  Quiet[DistributeDefinitions[
    LSPServer`buildWorkerSnapshot,
    LSPServer`Diagnostics`Private`runWorkspaceDiagnosticsWorker,
    LSPServer`Diagnostics`Private`runClosedFileDiagnosticsWorker
  ]]

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
$IndexingWasActive
$InternalRequestId
$PendingSemanticTokenRequests
$WorkspaceDiagnosticsSweepURIs
$ClosedFileDiagnosticsNotifications
$QueueLastNonEmptyTime
$PendingTokenRefresh

$startupMessagesText


Begin["`Private`"]


(*
setup Startup Messages handling

There may be internal errors in LSPServer that emit messages during Needs["LSPServer`"]

These messages are exceptionally hard to handle because any code for handling has not yet been loaded

The messages may cause unexplained hangs in clients

So manually set $Messages to a tmp file and then handle the messages later
*)
$startupMessagesFile = OpenWrite[]

If[!FailureQ[$startupMessagesFile],
  $oldMessages = $Messages;
  $Messages = {$startupMessagesFile}
  ,
  $startupMessagesText = "OpenWrite[] failed while setting up Startup Messages handling"
]



Quiet[Needs["CodeFormatter`"], {MessageName[CompileUtilities`Symbols`SystemSymbolQ, "shdw"]}]
Quiet[Needs["CodeInspector`"], {MessageName[CompileUtilities`Symbols`SystemSymbolQ, "shdw"]}]
Quiet[Needs["CodeInspector`Format`"], {MessageName[CompileUtilities`Symbols`SystemSymbolQ, "shdw"]}]
Quiet[Needs["CodeInspector`ImplicitTokens`"], {MessageName[CompileUtilities`Symbols`SystemSymbolQ, "shdw"]}]
Quiet[Needs["CodeInspector`BracketMismatches`"], {MessageName[CompileUtilities`Symbols`SystemSymbolQ, "shdw"]}]
Quiet[Needs["CodeInspector`Utils`"], {MessageName[CompileUtilities`Symbols`SystemSymbolQ, "shdw"]}]
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
Get[FileNameJoin[{location, "Kernel", "DocumentSymbol.wl"}]]
Get[FileNameJoin[{location, "Kernel", "PacletIndex.wl"}]]
Get[FileNameJoin[{location, "Kernel", "BracketMismatches.wl"}]]
Get[FileNameJoin[{location, "Kernel", "CodeAction.wl"}]]
Get[FileNameJoin[{location, "Kernel", "Color.wl"}]]
Get[FileNameJoin[{location, "Kernel", "Completion.wl"}]]
Get[FileNameJoin[{location, "Kernel", "Definitions.wl"}]]
Get[FileNameJoin[{location, "Kernel", "IgnorePatterns.wl"}]]
Get[FileNameJoin[{location, "Kernel", "Diagnostics.wl"}]]
Get[FileNameJoin[{location, "Kernel", "FoldingRange.wl"}]]
Get[FileNameJoin[{location, "Kernel", "Formatting.wl"}]]
Get[FileNameJoin[{location, "Kernel", "Hover.wl"}]]
Get[FileNameJoin[{location, "Kernel", "TypeWL.wl"}]]
Get[FileNameJoin[{location, "Kernel", "ImplicitTokens.wl"}]]
Get[FileNameJoin[{location, "Kernel", "InlayHints.wl"}]]
Get[FileNameJoin[{location, "Kernel", "References.wl"}]]
Get[FileNameJoin[{location, "Kernel", "SelectionRange.wl"}]]
Get[FileNameJoin[{location, "Kernel", "SemanticTokens.wl"}]]
Get[FileNameJoin[{location, "Kernel", "Workspace.wl"}]]


(*
This uses func := func = def idiom and is fast
*)
loadAllFuncs[]


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


$PriorityContentQueueMethods = {
  "textDocument/didOpenFencepost",
  "textDocument/didChangeFencepost",
  "textDocument/didCloseFencepost",
  "textDocument/semanticTokens/fullFencepost"
}


contentQueuePriorityMethodQ[content_] :=
  AssociationQ[content] &&
  (
    TrueQ[Lookup[content, "priority", False]] ||
    MemberQ[$PriorityContentQueueMethods, Lookup[content, "method", None]]
  )


prioritizeContentQueueContents[contents_List] :=
  Join[
    Select[contents, contentQueuePriorityMethodQ],
    Select[contents, !contentQueuePriorityMethodQ[#] &]
  ]


appendContentsToContentQueue[contents_List] :=
  If[contents =!= {},
    (* Keep semantic-token and lifecycle fenceposts ahead of slower
       diagnostics / code-action work so token responses are not starved
       behind large backlogs after didOpen or didChange. *)
    $ContentQueue = prioritizeContentQueueContents[Join[$ContentQueue, contents]]
  ]


contentQueueEmptyQ[] := empty[$ContentQueue]


takeFirstContentQueueItem[] :=
  If[contentQueueEmptyQ[],
    None,
    Module[{content = First[$ContentQueue]},
      $ContentQueue = Rest[$ContentQueue];
      content
    ]
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
  $IndexingWasActive            = False;
  $InternalRequestId            = -1;
  $PendingSemanticTokenRequests = <||>;
  $WorkspaceDiagnosticsSweepURIs = {};
  $ClosedFileDiagnosticsNotifications = <||>;
  $QueueLastNonEmptyTime        = 0;
  $PendingTokenRefresh          = False;


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
Module[{addonsApps, kernelObjDir, kernelBin, startupWl, ok},
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
    $DiagnosticsKernelBin = kernelBin;
    $DiagnosticsKernel = Quiet[Check[
      Module[{kernels = LaunchKernels[1]},
        If[ListQ[kernels] && Length[kernels] > 0, First[kernels], $Failed]
      ],
      $Failed
    ]]
    ,
    $DiagnosticsKernelBin = $Failed;
    $DiagnosticsKernel    = $Failed
  ];
  If[$DiagnosticsKernel =!= $Failed,
    Quiet[ParallelEvaluate[
      Needs["CodeParser`"];
      Needs["CodeInspector`"];
      Needs["CodeFormatter`"]
      ,
      $DiagnosticsKernel
    ]];
    Quiet[DistributeDefinitions["LSPServer`", "LSPServer`Private`", "LSPServer`Utils`",
      "LSPServer`PacletIndex`", "LSPServer`Diagnostics`", "LSPServer`Diagnostics`Private`",
      $DiagnosticsKernel]]
    ,
    log[0, "WARNING: LaunchKernels failed - workspace diagnostics will run synchronously"]
  ]
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


cancelCurrentDiagnosticsTask[] :=
Module[{},
  If[$DiagnosticsTask =!= None,
    If[$DiagnosticsTaskKind === "closed-file-sweep" && StringQ[$DiagnosticsTaskURI],
      requeueWorkspaceDiagnosticsSweepURI[$DiagnosticsTaskURI]
    ];

    $DiagnosticsTask          = None;
    $DiagnosticsTaskURI       = None;
    $DiagnosticsTaskKind      = None;
    $DiagnosticsTaskResult    = None;
    $DiagnosticsTaskStartTime = None;

    Quiet[If[$DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
      AbortKernels[$DiagnosticsKernel]
    ]]
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
  Process a small batch of pending workspace index files on each idle iteration.
  This lets InitializePacletIndex return immediately while indexing completes
  in the background between requests.
  Track when external dep indexing transitions from active → idle so that
  diagnostics can be re-dispatched once the $PacletIndex is fully populated.
  *)
  Module[{hadIndexWork, moreWork},
    hadIndexWork =
      Length[$PendingExternalDepFiles] > 0 ||
      Length[$PendingIndexFiles] > 0 ||
      Length[$PendingReferenceFiles] > 0;

    moreWork = ProcessPendingIndexFiles[];

    (* Mark indexing active as soon as we observe pending work, even if the
       current call drains the queues completely. Without this, single-batch
       dependency/workspace indexing never triggers the completion refresh. *)
    If[hadIndexWork,
      $IndexingWasActive = True
    ];

    If[$IndexingWasActive && !moreWork,
      $IndexingWasActive = False;
      (* Re-dispatch workspace diagnostics for all open files now that
         $PacletIndex is fully populated with external dep symbols.
         dispatchWorkspaceDiagnostics handles both the parallel-kernel path
         and the sync-fallback path, so no kernel-availability guard needed. *)
      Scan[
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
  ];

  (*
  Track the last time the queue was non-empty, for the kernel launch idle guard.
  *)
  If[Length[$ContentQueue] > 0,
    $QueueLastNonEmptyTime = AbsoluteTime[]
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
      cancelCurrentDiagnosticsTask[];
      Quiet[CloseKernels[$DiagnosticsKernel]];
      $DiagnosticsKernel    = $Failed;
      $DiagnosticsTaskStartTime = None
    ];
    If[$DiagnosticsTask =!= None && Length[$ContentQueue] == 0,
      Module[{taskResult, taskKind, taskURI},
        (*
        Poll with a 1 ms TimeConstrained.  If the worker is done the result
        arrives in microseconds (well within 1 ms); if it is still running we
        get Missing["StillRunning"] and retry on the next iteration (~100 ms).
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
          LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics[nextURI]
        ]
      ]
    ]
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
      If[!empty[toRemoveIndices],
        jobs = Delete[jobs, toRemoveIndices];
        entryCopy = entry;
        entryCopy["ScheduledJobs"] = jobs;
        $OpenFilesMap[uri] = entryCopy
      ]
    ]
    ,
    openFilesMapCopy
  ];

  If[!empty[contents],

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



$didOpenMethods = {
  "textDocument/runDiagnostics",
  "textDocument/publishDiagnostics"
}


$didCloseMethods = {
  "textDocument/publishDiagnostics"
}


$didSaveMethods = {}


$didChangeMethods = {}

$didChangeScheduledJobs = {
  Function[{entry}, If[Now - entry["LastChange"] > Quantity[$DiagnosticsDelayAfterLastChange, "Seconds"],
    {{
      "textDocument/runDiagnostics",
      "textDocument/publishDiagnostics"
    }, True},
    {{}, False}]
  ]
}


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
      semanticTokensProviderValue = <|
        "legend" -> <|
          "tokenTypes" -> Keys[$SemanticTokenTypes],
          "tokenModifiers" -> Keys[$SemanticTokenModifiers]
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

  (*
  Initialize paclet index if workspace root is set
  *)
  If[StringQ[$WorkspaceRootPath],
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
    appendContentsToContentQueue[{<|"method" -> "workspace/bootstrapClosedFileDiagnostics"|>}]
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
    LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics[nextURI]
  ];

  log[1, "workspace/bootstrapClosedFileDiagnostics: Exit"];

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


(*
Handle responses to server-initiated requests (e.g. workspace/semanticTokens/refresh).
These have no "method" key — just "id" and "result" (or "error").
Without this handler, LSPEvaluate would see an unevaluated handleContent[...] and call exitHard[].
*)
handleContent[content_?AssociationQ] /; !KeyExistsQ[content, "method"] :=
{}


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
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>& /@ idsToRecover
  ];

  Length[idsToRecover]
]


semanticTokensRefreshQueuedQ[] :=
  AnyTrue[$ContentQueue,
    AssociationQ[#] && Lookup[#, "method", None] === "workspace/semanticTokens/refresh" &
  ]


queueSemanticTokensRefresh[reason_String:""] :=
  If[$SemanticTokens && !semanticTokensRefreshQueuedQ[],
    If[reason =!= "",
      log[0, reason]
    ];
    AppendTo[$ContentQueue, <|"method" -> "workspace/semanticTokens/refresh"|>]
  ]


(*
Send workspace/semanticTokens/refresh to tell VS Code to re-fetch tokens for all
open files. For URIs that do not already have a pending semanticTokens/full
request id, clear cached token arrays so the later client re-fetch recomputes
classification instead of replaying stale results from before indexing or
didOpen finished. If a semanticTokens/full request is already pending for a
URI, recompute and answer that request inline instead of just invalidating the
cache and leaving the in-flight request waiting for a later re-request.
Uses a negative server-generated id to avoid colliding with client request ids.
*)
handleContent[content:KeyValuePattern["method" -> "workspace/semanticTokens/refresh"]] :=
Module[{id, invalidated = 0, precomputed = 0, recovered = 0, responses = {}},
  Scan[
    Function[{uri},
      Module[{entry = Lookup[$OpenFilesMap, uri, Null], idsToRecover, recoveredResponses = {}},
        If[AssociationQ[entry],
          idsToRecover = pendingSemanticTokenFencepostIDsToRecover[uri];

          If[idsToRecover =!= {},
            (* If VS Code already has an in-flight semanticTokens/full request
               for this URI, answer it now with fresh tokens rather than only
               invalidating the cache and waiting for a later re-request. *)
            If[KeyExistsQ[entry, "SemanticTokens"],
              invalidated += 1
            ];

            If[LSPServer`SemanticTokens`computeAndCacheSemanticTokens[uri],
              precomputed += 1
            ];

            recoveredResponses = pendingSemanticTokenFencepostResponses[uri, idsToRecover];
            If[recoveredResponses =!= {},
              recovered += Length[idsToRecover];
              responses = Join[responses, recoveredResponses]
            ]
          ,
            If[KeyExistsQ[entry, "SemanticTokens"],
              (* Clear cached tokens so VS Code fetches fresh ones. *)
              $OpenFilesMap[uri] = KeyDrop[entry, "SemanticTokens"];
              invalidated += 1
            ]
          ]
        ]
      ]
    ],
    Keys[$OpenFilesMap]
  ];

  $InternalRequestId -= 1;
  id = $InternalRequestId;
    log[0, "DBG-ST: sending workspace/semanticTokens/refresh id=", id,
      " invalidated=", invalidated, " precomputed=", precomputed,
      " recovered=", recovered];
  Join[
    {<| "jsonrpc" -> "2.0", "id" -> id, "method" -> "workspace/semanticTokens/refresh" |>},
    responses
  ]
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
Module[{params, doc, uri, text, entry, responses = {}},

  If[$Debug2,
    log["textDocument/didOpenFencepost: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  text = doc["text"];

  entry = <|
    "Text" -> text,
    "LastChange" -> Now
  |>;

  (* Pre-process .ipwl files so the parse handlers use annotation-free source *)
  If[StringEndsQ[uri, ".ipwl"],
    entry["PreprocessedText"] = LSPServer`TypeWL`PreprocessIPWL[text][[1]]
  ];

  $OpenFilesMap[uri] = entry;

  (*
  Update the paclet index for this file, then cache the parsed artifacts into
  the entry so textDocument/concreteParse, textDocument/aggregateParse, and
  textDocument/abstractParse can skip their redundant re-parse steps.
  *)
  With[{parseResult = UpdateFileIndex[uri, text]},
    If[ListQ[parseResult] && Length[parseResult] == 3,
      Module[{e},
        e = $OpenFilesMap[uri];
        If[AssociationQ[e],
          e["CST"] = parseResult[[1]];
          If[!StringContainsQ[text, "\t"], e["CSTTabs"] = parseResult[[1]]];
          e["Agg"] = parseResult[[2]];
          e["AST"] = parseResult[[3]];
          e["PreviousAST"] = parseResult[[3]];
          With[{syms = findAllUserSymbols[parseResult[[3]]]},
            e["UserSymbols"]         = syms;
            e["PreviousUserSymbols"] = syms
          ];
          $OpenFilesMap[uri] = e
        ]
      ]
    ]
  ];

  (* didOpen can race with the editor's initial semanticTokens/full request.
     Once indexing has populated CST/AST, answer any pending request ids for
     this URI directly instead of relying on a later queued fencepost. *)
  If[$SemanticTokens,
    responses = recoverPendingSemanticTokenFenceposts[
      uri,
      "DBG-ST: didOpen indexed; recovering pending semantic-token requests"
    ]
  ];

  log[1, "textDocument/didOpenFencepost: Exit"];

  responses
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

  res = <| "method" -> #, "params" -> params |>& /@ ({
      "textDocument/didChangeFencepost"
    } ~Join~ $didChangeMethods);

  log[1, "textDocument/didChange: Exit"];

  res

]]


handleContent[content:KeyValuePattern["method" -> "textDocument/didChangeFencepost"]] :=
Catch[
Module[{params, doc, uri, text, lastChange, entry, changes, oldEntry},

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

  changes = params["contentChanges"];

  (*
  Currently only supporting full text, so always only apply the last change
  *)
  lastChange = changes[[-1]];

  text = lastChange["text"];

  oldEntry = Lookup[$OpenFilesMap, uri, <||>];

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
    "PreviousAST" -> Lookup[oldEntry, "PreviousAST", Lookup[oldEntry, "AST", Missing["NotAvailable"]]],
    "PreviousUserSymbols" -> Lookup[oldEntry, "PreviousUserSymbols", Lookup[oldEntry, "UserSymbols", Missing["NotAvailable"]]]
  |>;

  (* Pre-process .ipwl files so the parse handlers use annotation-free source *)
  If[StringEndsQ[uri, ".ipwl"],
    entry["PreprocessedText"] = LSPServer`TypeWL`PreprocessIPWL[text][[1]]
  ];

  $OpenFilesMap[uri] = entry;

  (*
  Schedule paclet index update (debounced with other scheduled jobs)
  *)
  AppendTo[entry["ScheduledJobs"],
    Function[{e}, If[Now - e["LastChange"] > Quantity[$DiagnosticsDelayAfterLastChange, "Seconds"],
      (* Run index update, then cache parsed artifacts into $OpenFilesMap so the
         diagnostics pipeline can skip its redundant concreteParse / aggregateParse /
         abstractParse steps. *)
      With[{parseResult = UpdateFileIndex[uri, e["Text"]]},
        If[ListQ[parseResult] && Length[parseResult] == 3,
          Module[{curEntry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]]},
            If[AssociationQ[curEntry] && Lookup[curEntry, "LastChange", Missing["NotAvailable"]] === e["LastChange"],
              curEntry["CST"] = parseResult[[1]];
              If[!StringContainsQ[e["Text"], "\t"], curEntry["CSTTabs"] = parseResult[[1]]];
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
            ]
          ];
          If[AssociationQ[Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]]] &&
             Lookup[$OpenFilesMap[uri], "LastChange", Missing["NotAvailable"]] === e["LastChange"],
            Scan[
              LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics,
              Keys[$OpenFilesMap]
            ];
            queueWorkspaceDiagnosticsSweep[];
            If[$SemanticTokens,
              (* Recover any semanticTokens/full requests that became stale
                 while the didChange index update was still pending.
                 Do not proactively push workspace/semanticTokens/refresh here:
                 VS Code can turn that into a self-sustaining refresh loop. *)
              queuePendingSemanticTokenFenceposts[
                uri,
                "DBG-ST: didChange indexed; queuing pending semantic-token fenceposts"
              ]
            ]
          ]
        ]
      ];
      {{}, True},
      {{}, False}]
    ]
  ];
  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/didChangeFencepost: Exit"];

  {}
]]


exitGracefully[] := (
  log[0, "\n\n"];
  log[0, "KERNEL IS EXITING GRACEFULLY"];
  log[0, "\n\n"];
  If[$DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
    Quiet[AbortKernels[$DiagnosticsKernel]];
    Quiet[CloseKernels[$DiagnosticsKernel]];
    $DiagnosticsKernel = None
  ];
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
  If[$DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
    Quiet[AbortKernels[$DiagnosticsKernel]];
    Quiet[CloseKernels[$DiagnosticsKernel]];
    $DiagnosticsKernel = None
  ];
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
  If[$DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
    Quiet[AbortKernels[$DiagnosticsKernel]];
    Quiet[CloseKernels[$DiagnosticsKernel]];
    $DiagnosticsKernel = None
  ];
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
Module[{name},

  If[!FailureQ[$startupMessagesFile],

    name = Close[$startupMessagesFile];

    $startupMessagesText = Import[name, "Text"];

    DeleteFile[name];

    $Messages = $oldMessages
  ]
]


End[]

EndPackage[]
