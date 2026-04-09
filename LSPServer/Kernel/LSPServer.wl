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

ProcessScheduledJobs


exitHard
exitGracefully
exitSemiGracefully
shutdownLSPComm


handleContent

handleContentAfterShutdown

buildWorkerSnapshot




$Debug3

(*
level 0: Server start and exit log
level 1: Content handler entry and exit log to understand the flow of the handlers
level 2: Log inside a content handler for content handler debugging
level 3: Further detailed log
*)
$LogLevel = 0

$DebugBracketMatcher


$PreExpandContentQueue

$ContentQueue

$OpenFilesMap

$CancelMap

$hrefIdCounter

$ServerState


$AllowedImplicitTokens


$BracketMatcher

(*
$BracketMatcherDisplayInsertionText
*)

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

$DiagnosticsTask

$DiagnosticsTaskURI

$DiagnosticsTaskResult

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



Needs["CodeFormatter`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`ImplicitTokens`"]
Needs["CodeInspector`BracketMismatches`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

Needs["PacletManager`"] (* for PacletInformation *)


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
Module[{contents},

  contents = contentsIn;

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

  $ContentQueue = $ContentQueue ~Join~ contents;

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

  (* Background kernel for async workspace diagnostics *)
  $DiagnosticsTask       = None;
  $DiagnosticsTaskURI    = None;
  $DiagnosticsTaskResult = None;
  $DiagnosticsKernel     = Quiet[Check[First[LaunchKernels[1]], $Failed]];
  If[$DiagnosticsKernel =!= $Failed,
    (* Load required packages on the worker kernel, then distribute LSPServer definitions *)
    ParallelEvaluate[
      Needs["CodeParser`"];
      Needs["CodeInspector`"];
      Needs["CodeFormatter`"]
      ,
      $DiagnosticsKernel
    ];
    DistributeDefinitions["LSPServer`", "LSPServer`Private`", "LSPServer`Utils`",
      "LSPServer`PacletIndex`", "LSPServer`Diagnostics`",
      $DiagnosticsKernel];
    ,
    log[0, "WARNING: LaunchKernels failed — workspace diagnostics will run synchronously"]
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
  *)
  ProcessPendingIndexFiles[];

  openFilesMapCopy = $OpenFilesMap;

  contents = {};
  KeyValueMap[
    Function[{uri, entry},
      jobs = Lookup[entry, "ScheduledJobs", {}];
      toRemoveIndices = {};
      Do[
        job = jobs[[j]];
        res = job[entry];
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

    $ContentQueue = $ContentQueue ~Join~ contents;
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

    Throw[contents]
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


  capabilities = params["capabilities"];
  textDocument = capabilities["textDocument"];
  codeAction = textDocument["codeAction"];

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
    hierarchicalDocumentSymbolSupport = documentSymbol["hierarchicalDocumentSymbolSupport"];
    $HierarchicalDocumentSymbolSupport = hierarchicalDocumentSymbolSupport
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
    LoadProjectIgnoreConfig[$WorkspaceRootPath]
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

  log[1, "initialized: Exit"];

  res
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
  If[StringQ[$WorkspaceRootPath],
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
    ]
  ];

  log[1, "textDocument/didOpenFencepost: Exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/concreteParse"]] :=
Catch[
Module[{params, doc, uri, cst, text, entry, fileName, fileFormat},

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

  fileName = normalizeURI[uri];

  fileFormat = "Package";
  If[FileExtension[fileName] == "wls",
    fileFormat = "Script"
  ];

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
Module[{params, doc, uri, text, entry, cstTabs, fileName, fileFormat},


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

  fileName = normalizeURI[uri];

  fileFormat = "Package";
  If[FileExtension[fileName] == "wls",
    fileFormat = "Script"
  ];

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

  cst = entry["CST"];

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

  agg = entry["Agg"];

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
Module[{params, doc, uri},


  log[1, "textDocument/didCloseFencepost: Enter"];


  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  $OpenFilesMap[uri] =.;

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
Module[{params, doc, uri, text, lastChange, entry, changes},

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
  If[$DiagnosticsTask =!= None,
    $DiagnosticsTask    = None;
    $DiagnosticsTaskURI = None;
    Quiet[If[$DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
      AbortKernels[$DiagnosticsKernel]
    ]]
  ];

  changes = params["contentChanges"];

  (*
  Currently only supporting full text, so always only apply the last change
  *)
  lastChange = changes[[-1]];

  text = lastChange["text"];

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
    "PreviousAST" -> entry["PreviousAST"],
    "PreviousUserSymbols" -> entry["PreviousUserSymbols"]
  |>;

  (* Pre-process .ipwl files so the parse handlers use annotation-free source *)
  If[StringEndsQ[uri, ".ipwl"],
    entry["PreprocessedText"] = LSPServer`TypeWL`PreprocessIPWL[text][[1]]
  ];

  $OpenFilesMap[uri] = entry;

  (*
  Schedule paclet index update (debounced with other scheduled jobs)
  *)
  If[StringQ[$WorkspaceRootPath],
    AppendTo[entry["ScheduledJobs"],
      Function[{e}, If[Now - e["LastChange"] > Quantity[$DiagnosticsDelayAfterLastChange, "Seconds"],
        (* Run index update, then cache parsed artifacts into $OpenFilesMap so the
           diagnostics pipeline can skip its redundant concreteParse / aggregateParse /
           abstractParse steps. *)
        With[{parseResult = UpdateFileIndex[uri, e["Text"]]},
          If[ListQ[parseResult] && Length[parseResult] == 3,
            Module[{curEntry = $OpenFilesMap[uri]},
              If[AssociationQ[curEntry],
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
              ]
            ]
          ]
        ];
        {{}, True},
        {{}, False}]
      ]
    ];
    $OpenFilesMap[uri] = entry
  ];

  log[1, "textDocument/didChangeFencepost: Exit"];

  {}
]]


exitGracefully[] := (
  log[0, "\n\n"];
  log[0, "KERNEL IS EXITING GRACEFULLY"];
  log[0, "\n\n"];
  If[$DiagnosticsKernel =!= $Failed && $DiagnosticsKernel =!= None,
    Quiet[CloseKernels[$DiagnosticsKernel]];
    $DiagnosticsKernel = None
  ];
  shutdownLSPComm[$commProcess, $initializedComm];
  Pause[1];
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
    Quiet[CloseKernels[$DiagnosticsKernel]];
    $DiagnosticsKernel = None
  ];
  shutdownLSPComm[$commProcess, $initializedComm];
  Pause[1];
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
    Quiet[CloseKernels[$DiagnosticsKernel]];
    $DiagnosticsKernel = None
  ];
  shutdownLSPComm[$commProcess, $initializedComm];
  Pause[1];
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
