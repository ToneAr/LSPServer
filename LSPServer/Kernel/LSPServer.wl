(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)
BeginPackage["LSPServer`"]

StartServer::usage =
	StringJoin[
		"StartServer[] puts the kernel into a state ready for traffic from ",
		"the client.\
 StartServer[logDir] logs traffic to logDir."
	]

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
$WorkerLaunchAttempts
$WorkerLastFailureReason
$WorkerStatusNotified
$HighlightKernel
$HighlightKernelBin
$HighlightKernelLaunchAfter
$HighlightWorkerLaunchAttempts
$HighlightWorkerLastFailureReason
$HighlightWorkerStatusNotified
$HighlightTask
$HighlightTaskKind
$HighlightTaskURI
$HighlightTaskID
$HighlightTaskContent
$HighlightTaskResult
$HighlightTaskStartTime
$HighlightPendingContents
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
$WorkspaceIndexingInterval
$WorkspaceIndexingLastRun
$WorkspaceIndexingBatchSize
$WorkspaceReferenceBatchSize
$ExternalDependencyIndexingBatchSize
$DependencyDiscoveryBatchSize
$ExternalDependencyFileLimit
$ClosedFileDiagnosticsIdleDelay
$ClosedFileDiagnosticsInterval
$ClosedFileDiagnosticsLastRun
$ClosedFileDiagnosticsMaxTextLength
$IdleLoopPause

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
	$Messages = {$startupMessagesFile},
	$startupMessagesText =
		"OpenWrite[] failed while setting up Startup Messages handling"
]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

Needs["PacletManager`"]  (* for PacletInformation *)

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
		Get[FileNameJoin[{location, "Resources", "Data", "Options.wl"}]]

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
workspaceSourceFilePatterns[] :=
	{"*.wl", "*.m", "*.wls", "*.wlt", "*.mt", "*.ipwl"}

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

$FeatureModuleFiles =
	<|
		"Diagnostics"       -> FileNameJoin[
			{location, "Kernel", "Diagnostics.wl"}
		],
		"DocumentSymbol"    -> FileNameJoin[
			{location, "Kernel", "DocumentSymbol.wl"}
		],
		"BracketMismatches" -> FileNameJoin[
			{location, "Kernel", "BracketMismatches.wl"}
		],
		"CodeAction"        -> FileNameJoin[
			{location, "Kernel", "CodeAction.wl"}
		],
		"Color"             -> FileNameJoin[{location, "Kernel", "Color.wl"}],
		"Completion"        -> FileNameJoin[
			{location, "Kernel", "Completion.wl"}
		],
		"Definitions"       -> FileNameJoin[
			{location, "Kernel", "Definitions.wl"}
		],
		"FoldingRange"      -> FileNameJoin[
			{location, "Kernel", "FoldingRange.wl"}
		],
		"Formatting"        -> FileNameJoin[
			{location, "Kernel", "Formatting.wl"}
		],
		"Hover"             -> FileNameJoin[{location, "Kernel", "Hover.wl"}],
		"ImplicitTokens"    -> FileNameJoin[
			{location, "Kernel", "ImplicitTokens.wl"}
		],
		"InlayHints"        -> FileNameJoin[
			{location, "Kernel", "InlayHints.wl"}
		],
		"References"        -> FileNameJoin[
			{location, "Kernel", "References.wl"}
		],
		"SelectionRange"    -> FileNameJoin[
			{location, "Kernel", "SelectionRange.wl"}
		],
		"SemanticTokens"    -> FileNameJoin[
			{location, "Kernel", "SemanticTokens.wl"}
		],
		"Workspace"         -> FileNameJoin[
			{location, "Kernel", "Workspace.wl"}
		]
	|>;

$FeatureModulesLoaded = <||>;

$FeatureExpandMethods =
	<|
		"textDocument/runDiagnostics"       -> {"Diagnostics"},
		"textDocument/documentSymbol"       -> {"DocumentSymbol"},
		"textDocument/runBracketMismatches" -> {"BracketMismatches"},
		"textDocument/codeAction"           -> {"CodeAction"},
		"textDocument/documentColor"        -> {"Color"},
		"textDocument/completion"           -> {"Completion"},
		"textDocument/definition"           -> {"Definitions"},
		"textDocument/foldingRange"         -> {"FoldingRange"},
		"textDocument/hover"                -> {"Hover"},
		"textDocument/runImplicitTokens"    -> {"ImplicitTokens"},
		"textDocument/inlayHint"            -> {"InlayHints"},
		"textDocument/references"           -> {"References"},
		"textDocument/selectionRange"       -> {"SelectionRange"},
		"textDocument/semanticTokens/full"  -> {"SemanticTokens"}
	|>;

$FeatureHandleMethods =
	<|
		"textDocument/runFastDiagnostics"            -> {"Diagnostics"},
		"textDocument/runClosedFileDiagnostics"      -> {"Diagnostics"},
		"textDocument/suppressedRegions"             -> {"Diagnostics"},
		"textDocument/parseIgnoreComments"           -> {"Diagnostics"},
		"textDocument/runConcreteDiagnostics"        -> {"Diagnostics"},
		"textDocument/runAggregateDiagnostics"       -> {"Diagnostics"},
		"textDocument/runAbstractDiagnostics"        -> {"Diagnostics"},
		"textDocument/runScopingDiagnostics"         -> {"Diagnostics"},
		"textDocument/runWorkspaceDiagnostics"       -> {"Diagnostics"},
		"textDocument/mergeWorkspaceLints"           -> {"Diagnostics"},
		"textDocument/clearDiagnostics"              -> {"Diagnostics"},
		"textDocument/publishDiagnostics"            -> {"Diagnostics"},
		"textDocument/publishClosedFileDiagnostics"  -> {"Diagnostics"},
		"textDocument/documentNodeList"              -> {"DocumentSymbol"},
		"textDocument/documentSymbolFencepost"       -> {"DocumentSymbol"},
		"textDocument/runBracketMismatchesFencepost" -> {"BracketMismatches"},
		"textDocument/suggestBracketEdits"           -> {"BracketMismatches"},
		"textDocument/clearBracketMismatches"        -> {"BracketMismatches"},
		"textDocument/publishBracketMismatches"      -> {"BracketMismatches"},
		"textDocument/codeActionFencepost"           -> {"CodeAction"},
		"textDocument/documentColorFencepost"        -> {"Color"},
		"textDocument/colorPresentation"             -> {"Color"},
		"textDocument/completionFencepost"           -> {"Completion"},
		"completionItem/resolve"                     -> {"Completion"},
		"textDocument/definitionFencepost"           -> {"Definitions"},
		"textDocument/foldingRangeFencepost"         -> {"FoldingRange"},
		"textDocument/formatting"                    -> {"Formatting"},
		"textDocument/rangeFormatting"               -> {"Formatting"},
		"textDocument/hoverFencepost"                -> {"Hover"},
		"textDocument/runImplicitTokensFencepost"    -> {"ImplicitTokens"},
		"textDocument/clearImplicitTokens"           -> {"ImplicitTokens"},
		"textDocument/publishImplicitTokens"         -> {"ImplicitTokens"},
		"textDocument/inlayHintFencepost"            -> {"InlayHints"},
		"textDocument/referencesFencepost"           -> {"References"},
		"textDocument/selectionRangeFencepost"       -> {"SelectionRange"},
		"textDocument/semanticTokens/fullFencepost"  -> {"SemanticTokens"},
		"textDocument/runScopingData"                -> {"SemanticTokens"},
		"workspace/executeCommand"                   -> {"Workspace"},
		"workspace/didChangeWatchedFiles"            -> {"Workspace"},
		"workspace/didChangeConfiguration"           -> {"Workspace"},
		"workspace/symbol"                           -> {"Workspace"},
		"workspace/didChangeWorkspaceFolders"        -> {"Workspace"}
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
	!TrueQ[
		Lookup[
			$FeatureModulesLoaded,
			First[$FeatureExpandMethods[method]],
			False
		]
	] :=
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

$ExecuteCommandProvider =
	<|
		"commands" -> {
			(*
			Toggle inlay hints on/off at runtime.
			When toggled, the server sends a workspace/inlayHint/refresh request
			to notify the client to re-request hints.
			*)
			"toggle_inlay_hints",
			(*
			worker_kernel_status reports background-worker launch state (debug/diagnosis)
			*)
			"worker_kernel_status",
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

$ErrorCodes =
	<|
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

Clear[jsonRPCErrorResponse]
jsonRPCErrorResponse[content_, codeName_String, message_String] :=
	Module[{id},
		If[!AssociationQ[content] || !KeyExistsQ[content, "id"], Return[{}]];
		id = content["id"];
		{
			<|
				"jsonrpc" -> "2.0",
				"id"      -> id,
				"error"   -> <|
					"code"    -> Lookup[
						$ErrorCodes,
						codeName,
						$ErrorCodes["InternalError"]
					],
					"message" -> message
				|>
			|>
		}
	]

$TextDocumentSyncKind = <|"None" -> 0, "Full" -> 1, "Incremental" -> 2|>

$MessageType = <|"Error" -> 1, "Warning" -> 2, "Info" -> 3, "Log" -> 4|>

$ContentQueue = {}

$WorkspaceBootstrapAfter = None

(*
Thunks set by readEvalWriteLoop so that yield points inside long-running
handlers can drain the transport queue and write responses without knowing
the transport type.
*)
$TryQueueThunk = Function[Null]

$WriteLSPResultThunk = Function[{contentsArg}, Null]

$PriorityContentQueueMethods =
	{
		"textDocument/didOpenFencepost",
		"textDocument/didChangeFencepost",
		"textDocument/didCloseFencepost",
		"textDocument/semanticTokens/fullFencepost",
		"textDocument/documentColorFencepost"
	}

(*
Interactive methods that a user is waiting on — these should be served before
background diagnostics items whenever possible. The takeFirstContentQueueItem
function promotes any interactive item to the front of the queue.
*)
$InteractiveContentQueueMethods =
	{
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
$DeferrableDiagnosticMethods =
	{
		"textDocument/runFastDiagnostics",
		"textDocument/runWorkspaceDiagnostics",
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
		If[deferrable === Automatic,
			MemberQ[
				$DeferrableDiagnosticMethods,
				Lookup[content, "method", None]
			],
			TrueQ[deferrable]
		]
	]

	contentURI[content_] :=
		Lookup[
			Lookup[Lookup[content, "params", <||>], "textDocument", <||>],
			"uri",
			None
		]

	serverInitiatedResponseQ[content_] :=
		AssociationQ[content] &&
		!KeyExistsQ[content, "method"] &&
		IntegerQ[Lookup[content, "id", None]] &&
		Lookup[content, "id", None] < 0

	contentMethodName[content_] :=
		Lookup[
			content,
			"method",
			If[
				serverInitiatedResponseQ[content],
				"$/serverResponse",
				Missing["NotFound"]
			]
		]

contiguousRequestPipelineRange[contents_List, idx_Integer] :=
	Module[{id, uri, start = idx, end = idx},
		id = Lookup[contents[[idx]], "id", Missing["NotFound"]];
		If[MissingQ[id], Return[{idx}]];
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
		idx =
			SelectFirst[
				Range[2, Length[contents]],
				!deferrableDiagnosticQ[contents[[#]]]&,
				0
			];
		If[idx == 0, {}, contiguousRequestPipelineRange[contents, idx]]
	]

promoteFirstNonDeferrableQueueRange[] :=
	Module[{range, block},
		range = firstNonDeferrableQueueRange[$ContentQueue];
		If[range === {} || First[range] <= 1, Return[False]];
		block = $ContentQueue[[range]];
		$ContentQueue = Join[block, Delete[$ContentQueue, List /@ range]];
		True
	]

contentQueuePriorityMethodQ[content_] :=
	Module[{priority},
		priority = Lookup[content, "priority", Automatic];
		AssociationQ[content] &&
		If[priority === Automatic,
			MemberQ[
				$PriorityContentQueueMethods,
				Lookup[content, "method", None]
			],
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
	Module[
		{
			priorityGroups,
			nonGrouped,
			result,
			uri,
			i,
			fencepostIndices,
			claimed,
			group,
			groupEnd
		},
		(* Find indices of all priority (fencepost) items *)
		fencepostIndices =
			Select[
				Range[Length[contents]],
				contentQueuePriorityMethodQ[contents[[#]]]&
			];
		(* For each fencepost, greedily claim the immediately following
		   non-priority items that share the same URI (these are the
		   runDiagnostics / publishDiagnostics that were expanded together
		   with the fencepost). *)
		claimed = <||>; (* index -> True for items claimed by a group *)
		priorityGroups =
			Flatten[
				Last[
					Reap[
						Do[
							uri = contentURI[contents[[idx]]];
							groupEnd = idx;
							i = idx + 1;
							While[
								i <= Length[contents] &&
								!KeyExistsQ[claimed, i] &&
								!contentQueuePriorityMethodQ[contents[[i]]] &&
								contentURI[contents[[i]]] === uri,
								groupEnd = i;
								i++
							];
							group = Range[idx, groupEnd];
							Do[claimed[g] = True, {g, group}];
							Sow[group],
							{idx, fencepostIndices}
						]
					]
				],
				1
			];
		(* Collect remaining unclaimed non-priority items *)
		nonGrouped = Select[Range[Length[contents]], !KeyExistsQ[claimed, #]&];
		(* Result: priority groups first (each fencepost + its diagnostics),
		   then any remaining items *)
		result =
			Join[
				Flatten[Map[contents[[#]]&, priorityGroups, {2}], 1],
				contents[[nonGrouped]]
			];
		result
	]

appendContentsToContentQueue[contents_List] :=
	If[
		contents =!= {},
		(* Keep fencepost items grouped with their per-URI diagnostic
		   pipeline items so diagnostics are not starved behind a wall
		   of fenceposts from other URIs. *)
		$ContentQueue =
			If[
				AnyTrue[$ContentQueue, contentQueuePriorityMethodQ] ||
				AnyTrue[contents, contentQueuePriorityMethodQ],
				prioritizeContentQueueContents[Join[$ContentQueue, contents]],
				Join[$ContentQueue, contents]
			]
	]

contentQueueEmptyQ[] := $ContentQueue === {}

takeFirstContentQueueItem[] :=
	If[contentQueueEmptyQ[],
		None,
		Module[
			{content},
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
yieldToInteractiveRequests[currentURI_ : None] :=
	Module[
		{content, contents},
		(* Drain transport → $ContentQueue *)
		$TryQueueThunk[];
		While[
			!contentQueueEmptyQ[] &&
			If[deferrableDiagnosticQ[First[$ContentQueue]],
				promoteFirstNonDeferrableQueueRange[],
				True
			],
			content = First[$ContentQueue];
			$ContentQueue = Rest[$ContentQueue];
			log[
				1,
				"yield: serving queued work ",
				Lookup[content, "method", Missing["NotFound"]]
			];
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
			ignoredResponses =
				Select[
					contents,
					AssociationQ[#] &&
					!KeyExistsQ[#, "method"] &&
					!serverInitiatedResponseQ[#]&
				];
			If[ignoredResponses =!= {},
				log[
					1,
					"Ignoring client responses without method: ids=",
					InputForm[Lookup[ignoredResponses, "id", Missing["NotFound"]]]
				]
			];
			contents =
				Select[
					contents,
					AssociationQ[#] &&
					(
						KeyExistsQ[#, "method"] ||
						serverInitiatedResponseQ[#]
					)&
				];
			If[contents === {}, Return[Null]];
		log[
			1,
			StringJoin[
				"**************************************** Message Cycle ",
				"****************************************** \n"
			]
		];
		log[
				1,
				"$ContentQueue Methods(before expansion):> ",
				InputForm[contentMethodName /@ $ContentQueue]
			];
			log[
				1,
				"New message (before expansion):> ",
				InputForm[contentMethodName /@ contents]
			];
		If[!MatchQ[contents, {_?AssociationQ...}],
			log[0, "\n\n"];
			log[
				0,
				"Internal assert 1 failed: list of Associations: ",
				contents
			];
			log[0, "\n\n"];
			exitHard[]
		];
		preScanForCancels[contents];
		(*
		Now expand new contents
		*)
			contents = expandContents[contents];
			contents =
				Select[
					contents,
					AssociationQ[#] &&
					(
						KeyExistsQ[#, "method"] ||
						serverInitiatedResponseQ[#]
					)&
				];
			appendContentsToContentQueue[contents];
			log[
				1,
				"$ContentQueue methods (after expansion & joining new content) :> ",
				InputForm[contentMethodName /@ $ContentQueue]
			];
		log[
			3,
			"$ContentQueue (after expansion & joining new content):> ",
			InputForm[$ContentQueue],
			"\n"
		];
	]

(*

Use 0.4 seconds, same as default value of spelling squiggly in FE

In[7]:= CurrentValue[$FrontEnd, {SpellingOptions, "AutoSpellCheckDelay"}]

Out[7]= 0.4
*)
$DiagnosticsDelayAfterLastChange = 0.4

$ImplicitTokensDelayAfterLastChange = 3.0

$BracketMatcherDelayAfterLastChange = 4.0

(* Background work must not monopolize the LSP kernel while the editor is idle. *)
$IdleLoopPause = 0.03

(*
Server-initiated request ids count down from -1. The initialization path
resets this; the top-level default keeps internal requests issued before
initialization (tests, profiling harnesses) from hitting SubtractFrom::rvalue.
*)
$InternalRequestId = -1

$WorkspaceIndexingInterval = 0.25

$WorkspaceIndexingLastRun = 0

$WorkspaceIndexingBatchSize = 5

$WorkspaceReferenceBatchSize = 10

$ExternalDependencyIndexingBatchSize = 3

$DependencyDiscoveryBatchSize = 2

$ExternalDependencyFileLimit = 80

$ClosedFileDiagnosticsIdleDelay = 1.0

$ClosedFileDiagnosticsInterval = 2.0

$ClosedFileDiagnosticsLastRun = 0

$ClosedFileDiagnosticsMaxTextLength = 200000

$DiagnosticsKernel = None

$DiagnosticsKernelBin = None

$DiagnosticsTask = None

$DiagnosticsTaskURI = None

$DiagnosticsTaskKind = None

$DiagnosticsTaskResult = None

$DiagnosticsTaskStartTime = None

$DiagnosticsKernelLaunchAfter = None

$WorkerLaunchAttempts = 0

$WorkerLastFailureReason = None

$WorkerStatusNotified = False

$WorkerMaxLaunchAttempts = 4

$WorkerBackoffSchedule = {5, 15, 45, 120}

$WorkerHealthCheckInterval = 10

$WorkerLastHealthCheck = 0

$HighlightKernel = None

$HighlightKernelBin = None

$HighlightKernelLaunchAfter = None

$HighlightWorkerLaunchAttempts = 0

$HighlightWorkerLastFailureReason = None

$HighlightWorkerStatusNotified = False

$HighlightWorkerLastHealthCheck = 0

$HighlightTaskTimeout = 8

$HighlightTask = None

$HighlightTaskKind = None

$HighlightTaskURI = None

$HighlightTaskID = None

$HighlightTaskContent = None

$HighlightTaskResult = None

$HighlightTaskStartTime = None

$HighlightPendingContents = {}

$HoverTask = None

$HoverTaskURI = None

$HoverTaskID = None

$HoverTaskResult = None

$HoverTaskStartTime = None

StartServer::notebooks =
	"LSPServer cannot be started inside of a notebook session."
Options[StartServer] =
	{ConfidenceLevel -> Automatic, CommunicationMethod -> "StdIO"}
(*
setup the REPL to handle traffic from client
*)
StartServer[logDir_String : "", OptionsPattern[]] :=
	Catch[
		Catch[
			Module[{
					logFile,
					logFileStream,
					logFileName,
					logFileCounter,
					oldLogFiles,
					now,
					quantity30days,
					dateStr,
					readEvalWriteCycle
				},
				$kernelStartTime = Now;
				If[
					$Notebooks,
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
				$WorkspaceBootstrapAfter = None;
				$DiagnosticsKernel = None;
				$DiagnosticsKernelBin = None;
				$DiagnosticsTask = None;
				$DiagnosticsTaskURI = None;
				$DiagnosticsTaskKind = None;
				$DiagnosticsTaskResult = None;
				$DiagnosticsTaskStartTime = None;
				$DiagnosticsKernelLaunchAfter = None;
				$WorkerLaunchAttempts = 0;
				$WorkerLastFailureReason = None;
				$WorkerStatusNotified = False;
				$WorkerLastHealthCheck = 0;
				$HighlightKernel = None;
				$HighlightKernelBin = None;
				$HighlightKernelLaunchAfter = None;
				$HighlightWorkerLaunchAttempts = 0;
				$HighlightWorkerLastFailureReason = None;
				$HighlightWorkerStatusNotified = False;
				$HighlightWorkerLastHealthCheck = 0;
				$HighlightTask = None;
				$HighlightTaskKind = None;
				$HighlightTaskURI = None;
				$HighlightTaskID = None;
				$HighlightTaskContent = None;
				$HighlightTaskResult = None;
				$HighlightTaskStartTime = None;
				$HighlightPendingContents = {};
				$HoverTask = None;
				$HoverTaskURI = None;
				$HoverTaskID = None;
				$HoverTaskResult = None;
				$HoverTaskStartTime = None;
				$IndexingWasActive = False;
				$InternalRequestId = -1;
				$PendingSemanticTokenRequests = <||>;
				$WorkspaceDiagnosticsSweepURIs = {};
				$ClosedFileDiagnosticsNotifications = <||>;
				$QueueLastNonEmptyTime = 0;
				$PendingTokenRefresh = False;
				$PendingTokenRefreshTime = None;
				$WorkspaceIndexingQueued = False;
				$WorkspaceIndexingLastRun = 0;
				$ClosedFileDiagnosticsLastRun = 0;
				If[(logDir != ""),
					(
						(* :!CodeAnalysis::BeginBlock:: *)
						(* :!CodeAnalysis::Disable::BackwardsCompatibility:: *)
						Quiet[
							CreateDirectory[logDir],
							{CreateDirectory::eexist, CreateDirectory::filex}
						];
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
					dateStr =
						DateString[
							now,
							{
								"Year",
								"-",
								"Month",
								"-",
								"Day",
								"_",
								"Hour24",
								"-",
								"Minute",
								"-",
								"Second"
							}
						];
					quantity30days = Quantity[30, "Days"];
					Do[
						(*
						Delete oldLogFile if not modified for 30 days
						*)
						If[
							(
								now -
								Information[File[oldLogFile]][
									"LastModificationDate"
								]
							) >
							quantity30days,
							DeleteFile[oldLogFile]
						],
						{oldLogFile, oldLogFiles}
					];
					logFileName = StringJoin[ "kernelLog-", dateStr];
					logFile =
						FileNameJoin[
							{logDir, StringJoin[ logFileName, ".txt"]}
						];
					logFileCounter = 1;
					While[
						True,
						If[FileExistsQ[logFile],
							logFile =
								FileNameJoin[
									{
										logDir,
										StringJoin[
											logFileName,
											"-",
											ToString[logFileCounter],
											".txt"
										]
									}
								];
							logFileCounter++;,
							Break[]
						]
					];
					logFileStream =
						OpenWrite[logFile, CharacterEncoding -> "UTF-8"];
					If[FailureQ[logFileStream],
						log["\n\n"];
						log["opening log file failed: ", logFileStream];
						log["\n\n"];
						exitHard[]
					];
					$Messages = Join[$Messages, {logFileStream}];
					$Output = Join[$Output, {logFileStream}]
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
				log[
					0,
					StringJoin[
						"Starting server... (If this is the last line you ",
						"see, then StartServer[] may have been called in an ",
						"unexpected way and the server is hanging.)"
					]
				];
				log[0, "\n\n"];
				If[$startupMessagesText =!= "",
					log["\n\n"];
					log[
						"There were messages when loading LSPServer` package: ",
						$startupMessagesText
					];
					log["\n\n"];
					exitHard[]
				];
				(*
				This is the first use of LSPServer library, so this is where the library is initialized.
				Handle any initialization failures or other errors.
				*)
				$initializedComm = initializeLSPComm[$commProcess];
				If[FailureQ[$initializedComm],
					log[0, "\n\n"];
					(*
					//InputForm to work-around bug 411375
					*)
					log[
						"Initialization failed: ",
						$initializedComm // InputForm
					];
					log["\n\n"];
					exitHard[]
				];
				readEvalWriteCycle =
					readEvalWriteLoop[$commProcess, $initializedComm];
				If[FailureQ[readEvalWriteCycle],
					log["\n\n"];
					log["Read-Eval-Write-Loop failed: ", readEvalWriteCycle];
					log["\n\n"];
					exitHard[]
				];
			]
		], (*Module, 1-arg Catch*)
		_,
		(
			log["\n\n"];
			log["uncaught Throw: ", #1];
			log["\n\n"];
			exitHard[]
		)&
	]

preScanForCancels[contents : {_?AssociationQ...}] :=
	Module[{cancels, params, id},
		cancels =
			Cases[contents, KeyValuePattern["method" -> "$/cancelRequest"]];
		Scan[
			Function[
				{content},
				params = content["params"];
				id = params["id"];
				$CancelMap[id] = True
			],
			cancels
		];
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
				log[
					"New message (before expansion):> ",
					InputForm[contentMethodName /@ contents],
					"\n"
				]
			];
		log[2, "before expandContent"];
		Block[{$PreExpandContentQueue},
			$PreExpandContentQueue = contents;
			lastContents = $PreExpandContentQueue;
			$PreExpandContentQueue =
				Flatten[
					MapIndexed[
						expandContent,
						$PreExpandContentQueue
					] /. expandContent[c_, _] :> {c}
				];
				log[
					2,
					"$PreExpandContentQueue (up to 20): ",
					contentMethodName /@ Take[$PreExpandContentQueue, UpTo[20]]
				];
				log[2, "..."];
				While[
					$PreExpandContentQueue =!= lastContents,
					log[
						2,
						"expanded (up to 20): ",
						contentMethodName /@ Take[
							$PreExpandContentQueue,
							UpTo[20]
						]
					];
					log[2, "..."];
					lastContents = $PreExpandContentQueue;
				$PreExpandContentQueue =
					Flatten[
						MapIndexed[
							expandContent,
							$PreExpandContentQueue
						] /. expandContent[c_, _] :> {c}
					];
					log[
						2,
						"$PreExpandContentQueue (up to 20): ",
						contentMethodName /@ Take[
							$PreExpandContentQueue,
							UpTo[20]
						]
					];
				log[2, "..."];
			];
			log[2, "after expandContent"];
			contents = $PreExpandContentQueue;
		];
		If[!MatchQ[contents, {_?AssociationQ...}],
			log[0, "\n\n"];
			log[
				0,
				"Internal assert 2 failed: list of Associations: ",
				contents
			];
			log[0, "\n\n"];
			exitHard[]
		];
		contents
	]

(*
workerKernelHealthyQ[kernel] — True only if kernel is a live subkernel that
answers a trivial round-trip within a timeout. Never throws.
The KernelObject head guard is load-bearing: ParallelEvaluate on anything else
launches the default parallel kernels (~5 s main-thread stall), and aborting
that launch via TimeConstrained leaves Parallel` half-initialized.
*)
workerKernelHealthyQ[kernel_] :=
	MatchQ[kernel, _KernelObject] &&
	Quiet[TimeConstrained[ParallelEvaluate[1 + 1, kernel], 5, $TimedOut]] === 2

parallelTaskReadyQ[task_] :=
	Module[{ready},
		If[!MatchQ[task, _EvaluationObject], Return[True]];
		Quiet[Check[Parallel`Developer`QueueRun[], Null]];
		ready = Quiet[Check[Parallel`Developer`DoneQ[task], False]];
		TrueQ[ready]
	]

parallelTaskResult[task_] :=
	If[MatchQ[task, _EvaluationObject],
		If[parallelTaskReadyQ[task],
			Quiet[CheckAbort[WaitAll[task], $Failed]],
			Missing["StillRunning"]
		],
		Quiet[
			TimeConstrained[
				WaitAll[task],
				0.001,
				Missing["StillRunning"]
			]
		]
	]

(*
notifyWorkerStatus[ok, reason] — enqueue a window/showMessage so the user knows
whether the background worker started. Failures always notify (Warning, with
reason). Success notifies once (Info), guarded by $WorkerStatusNotified.
*)
(*
scheduleWorkerRelaunch[] — schedule the next launch attempt with exponential
backoff, or give up (leaving $DiagnosticsKernelLaunchAfter = None) once the
attempt cap is reached. $WorkerLaunchAttempts is the count of attempts already made.
*)
scheduleWorkerRelaunch[] :=
	Module[{idx, delay},
		If[$WorkerLaunchAttempts >= $WorkerMaxLaunchAttempts,
			$DiagnosticsKernelLaunchAfter = None;
			Return[Null]
		];
		idx = Min[$WorkerLaunchAttempts, Length[$WorkerBackoffSchedule]];
		idx = Max[idx, 1];
		delay = $WorkerBackoffSchedule[[idx]];
		$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + delay;
		Null
	]

notifyWorkerStatus[ok_, reason_] :=
	Module[{type, message},
		If[TrueQ[ok],
			If[TrueQ[$WorkerStatusNotified], Return[Null]];
			$WorkerStatusNotified = True;
			type = $MessageType["Info"];
			message = "LSPServer: background worker kernel started.",
			type = $MessageType["Warning"];
			message =
				StringJoin[
					"LSPServer: background worker kernel failed to start; ",
					"workspace diagnostics will run on the main thread ",
					"(slower). Reason: ",
					ToString[reason]
				]
		];
		appendContentsToContentQueue[
			{
				<|
					"method" -> "window/showMessage",
					"params" -> <|"type" -> type, "message" -> message|>
				|>
			}
		];
		Null
	]

(*
workerStatusReport[] — a plain association describing worker state, for the
worker_kernel_status execute-command and logging.
*)
workerStatusReport[] :=
	<|
		"running"                    -> (
			workerKernelHealthyQ[$DiagnosticsKernel]
		),
		"kernelBin"                  -> Replace[
			$DiagnosticsKernelBin,
			Except[_String] -> Null
		],
		"attempts"                   -> $WorkerLaunchAttempts,
		"lastFailureReason"          -> Replace[
			$WorkerLastFailureReason,
			Except[_String] -> Null
		],
		"highlightRunning"           -> (
			workerKernelHealthyQ[$HighlightKernel]
		),
		"highlightKernelBin"         -> Replace[
			$HighlightKernelBin,
			Except[_String] -> Null
		],
		"highlightAttempts"          -> $HighlightWorkerLaunchAttempts,
		"highlightLastFailureReason" -> Replace[
			$HighlightWorkerLastFailureReason,
			Except[_String] -> Null
		]
	|>

(*
workerLaunchKernels[] — indirection seam over LaunchKernels[1] so tests can
simulate launch failure. Tests must not Block System`LaunchKernels itself:
it carries an autoload stub (OwnValue), and Block-ing it across the load
discards the real definitions for the rest of the session.
*)
workerLaunchKernels[] := LaunchKernels[1]

(*
launchWorkerKernel[] — robustly launch the background worker subkernel.
Increments $WorkerLaunchAttempts. On success: provisions the kernel, sets
$DiagnosticsKernel and $DiagnosticsKernelBin, notifies (once). On failure:
records $WorkerLastFailureReason, sets $DiagnosticsKernel = $Failed, notifies,
and schedules a backoff retry. Always returns Null.
*)
launchWorkerKernel[] :=
	Module[{
			kernel = $Failed,
			setupResult = $Failed,
			reason = "unknown"
		},
		If[$DiagnosticsKernel =!= None && $DiagnosticsKernel =!= $Failed,
			Return[Null]
		];
		If[
			$DiagnosticsTask =!= None,
			(* A stale in-flight task from a dead worker: requeue + clear before relaunching. *)
			cleanupDiagnosticsWorker[True]
		];
		$WorkerLaunchAttempts = $WorkerLaunchAttempts + 1;
		Quiet[Needs["Parallel`"]];
		(* Built-in auto-relaunch of dead subkernels, belt-and-suspenders with our own. *)
		Quiet[Parallel`Settings`$RelaunchFailedKernels = True];
		(* Judge the launch by its result shape + health check, not by messages:
		   LaunchKernels can emit benign messages on success, and treating those as
		   failure would leak a live kernel while reporting $Failed. *)
		kernel =
			Quiet[
				CheckAbort[
					Module[{ks = workerLaunchKernels[]},
						If[
							ListQ[ks] &&
							Length[ks] > 0 &&
							MatchQ[First[ks], _KernelObject],
							First[ks],
							$Failed
						]
					],
					$Failed
				]
			];
		If[!workerKernelHealthyQ[kernel],
			If[MatchQ[kernel, _KernelObject],
				Quiet[AbortKernels[kernel]]; Quiet[CloseKernels[kernel]]
			];
			reason =
				StringJoin[
					"LaunchKernels failed or returned an unhealthy kernel ",
					"(attempt ",
					ToString[$WorkerLaunchAttempts],
					")"
				];
			$WorkerLastFailureReason = reason;
			$DiagnosticsKernel = $Failed;
			$DiagnosticsKernelBin = $Failed;
			log[
				0,
				"WARNING: worker kernel launch failed (attempt ",
				$WorkerLaunchAttempts,
				"): ",
				reason
			];
			notifyWorkerStatus[False, reason];
			scheduleWorkerRelaunch[];
			Return[Null]
		];
		setupResult =
			Quiet[
				Check[
					ParallelEvaluate[
						Needs["CodeParser`"];
						Needs["CodeInspector`"];
						Needs["CodeFormatter`"],
						kernel
					];
					DistributeDefinitions[
						"LSPServer`",
						"LSPServer`Private`",
						"LSPServer`Utils`",
						"LSPServer`PacletIndex`",
						"LSPServer`Diagnostics`",
						"LSPServer`Diagnostics`Private`",
						kernel
					],
					$Failed
				]
			];
		If[setupResult === $Failed,
			Quiet[AbortKernels[kernel]]; Quiet[CloseKernels[kernel]];
			$WorkerLastFailureReason =
				"worker provisioning (Needs/DistributeDefinitions) failed";
			$DiagnosticsKernel = $Failed;
			$DiagnosticsKernelBin = $Failed;
			log[
				0,
				"WARNING: worker kernel provisioning failed (attempt ",
				$WorkerLaunchAttempts,
				")"
			];
			notifyWorkerStatus[False, $WorkerLastFailureReason];
			scheduleWorkerRelaunch[];
			Return[Null]
		];
		$DiagnosticsKernel = kernel;
		$DiagnosticsKernelBin = $CommandLine[[1]];
		$WorkerLastFailureReason = None;
		log[0, "worker kernel launched (attempt ", $WorkerLaunchAttempts, ")"];
		notifyWorkerStatus[True, ""];
		Null
	]

(*
workerRelaunchDueQ[] — True when a (re)launch attempt is due: no live worker
(None or $Failed) and the scheduled launch time has arrived. Must accept
$Failed, not just None, or the backoff retry after a failed launch never runs.
*)
workerRelaunchDueQ[] :=
	($DiagnosticsKernel === None || $DiagnosticsKernel === $Failed) &&
	NumberQ[$DiagnosticsKernelLaunchAfter] &&
	AbsoluteTime[] >= $DiagnosticsKernelLaunchAfter

(*
maybeRelaunchDeadWorker[] — throttled liveness check. If we believe we have a
worker ($DiagnosticsKernel is not None/$Failed) but it fails a health check,
mark it dead (None) and schedule a relaunch. Throttled by $WorkerHealthCheckInterval.
*)
maybeRelaunchDeadWorker[] :=
	Module[{},
		If[$DiagnosticsKernel === None || $DiagnosticsKernel === $Failed,
			Return[Null]
		];
		If[AbsoluteTime[] - $WorkerLastHealthCheck < $WorkerHealthCheckInterval,
			Return[Null]
		];
		$WorkerLastHealthCheck = AbsoluteTime[];
		If[!workerKernelHealthyQ[$DiagnosticsKernel],
			log[0, "WARNING: worker kernel became unresponsive; relaunching"];
			If[MatchQ[$DiagnosticsKernel, _KernelObject],
				Quiet[AbortKernels[$DiagnosticsKernel]];
				Quiet[CloseKernels[$DiagnosticsKernel]]
			];
			$DiagnosticsKernel = None;
			scheduleWorkerRelaunch[]
		];
		Null
	]

(*
Back-compat wrapper: existing call sites use launchDiagnosticsKernel[].
*)
launchDiagnosticsKernel[] := launchWorkerKernel[]

$HighlightEntryMergeKeys =
	{
		"SemanticTokens",
		"SemanticTokensIncomplete",
		"SemanticTokensStale",
		"ScopingData"
	}

$HighlightSnapshotEntryKeys =
	{
		"Text",
		"PreprocessedText",
		"LastChange",
		"ScheduledJobs",
		"IndexUpdatePending",
		"SemanticTokens",
		"SemanticTokensIncomplete",
		"SemanticTokensStale",
		"ScopingData",
		"UserSymbols",
		"PreviousUserSymbols"
	}

$HighlightResultEntryKeys =
	{
		"SemanticTokens",
		"SemanticTokensIncomplete",
		"SemanticTokensStale",
		"ScopingData"
	}

highlightWorkerLaunchKernels[] := LaunchKernels[1]

scheduleHighlightWorkerRelaunch[] :=
	Module[{idx, delay},
		If[$HighlightWorkerLaunchAttempts >= $WorkerMaxLaunchAttempts,
			$HighlightKernelLaunchAfter = None;
			Return[Null]
		];
		idx =
			Min[$HighlightWorkerLaunchAttempts, Length[$WorkerBackoffSchedule]];
		idx = Max[idx, 1];
		delay = $WorkerBackoffSchedule[[idx]];
		$HighlightKernelLaunchAfter = AbsoluteTime[] + delay;
		Null
	]

highlightWorkerRelaunchDueQ[] :=
	($HighlightKernel === None || $HighlightKernel === $Failed) &&
	NumberQ[$HighlightKernelLaunchAfter] &&
	AbsoluteTime[] >= $HighlightKernelLaunchAfter

notifyHighlightWorkerStatus[ok_, reason_] :=
	Module[{type, message},
		If[TrueQ[ok],
			If[TrueQ[$HighlightWorkerStatusNotified], Return[Null]];
			$HighlightWorkerStatusNotified = True;
			Return[Null],
			type = $MessageType["Warning"];
			message =
				StringJoin[
					"LSPServer: highlight worker kernel failed to start; ",
					"semantic tokens and document colors will run on the main ",
					"thread (slower). Reason: ",
					ToString[reason]
				]
		];
		appendContentsToContentQueue[
			{
				<|
					"method" -> "window/showMessage",
					"params" -> <|"type" -> type, "message" -> message|>
				|>
			}
		];
		Null
	]

semanticTokenDataSnapshot[] :=
	<|
		"BuiltinFunctions"    -> WolframLanguageSyntax`Generate`$builtinFunctions,
		"Constants"           -> WolframLanguageSyntax`Generate`$constants,
		"Options"             -> WolframLanguageSyntax`Generate`$options,
		"ExperimentalSymbols" -> WolframLanguageSyntax`Generate`$experimentalSymbols,
		"ObsoleteSymbols"     -> WolframLanguageSyntax`Generate`$obsoleteSymbols,
		"UndocumentedSymbols" -> WolframLanguageSyntax`Generate`$undocumentedSymbols,
		"SessionSymbols"      -> WolframLanguageSyntax`Generate`$sessionSymbols,
		"BadSymbols"          -> WolframLanguageSyntax`Generate`$badSymbols,
		"SystemLongNames"     -> WolframLanguageSyntax`Generate`$systemLongNames
	|>

buildHighlightWorkerSnapshot[uri_String] :=
	Module[{entry},
		entry = Lookup[$OpenFilesMap, uri, Null];
		<|
		"OpenFileEntry"     -> If[
			AssociationQ[entry],
			KeyTake[entry, $HighlightSnapshotEntryKeys],
			entry
		],
		"PacletIndex"       -> LSPServer`PacletIndex`$PacletIndex,
		"WorkspaceRootPath" -> $WorkspaceRootPath,
		"SemanticTokens"    -> TrueQ[$SemanticTokens],
		"SyntaxData"        -> semanticTokenDataSnapshot[]
	|>
	]

highlightKernelAvailableQ[] :=
	$HighlightKernel =!= None && $HighlightKernel =!= $Failed

highlightEntryReadyQ[entry_?AssociationQ] :=
	Module[{cst, ast},
		cst = Lookup[entry, "CST", Null];
		ast = Lookup[entry, "AST", Null];
		cst =!= Null && !FailureQ[cst] && ast =!= Null && !FailureQ[ast]
	]
highlightEntryReadyQ[_] := False

prepareHighlightWorkerEntry[uri_String, entry_?AssociationQ] :=
	Module[{
			prepared = entry,
			text,
			sourceText,
			filePath,
			fileFormat,
			cst,
			agg,
			ast
		},
		If[highlightEntryReadyQ[prepared], Return[prepared]];
		text = Lookup[prepared, "Text", Missing["NotAvailable"]];
		If[!StringQ[text], Return[prepared]];
		sourceText = Lookup[prepared, "PreprocessedText", text];
		filePath = StringReplace[uri, StartOfString ~~ "file://" -> ""];
		fileFormat = LSPServer`SourceFileFormat[filePath];
		cst = Quiet[CodeConcreteParse[sourceText, "FileFormat" -> fileFormat]];
		If[FailureQ[cst], Return[prepared]];
		cst[[1]] = File;
		agg = Quiet[CodeParser`Abstract`Aggregate[cst]];
		If[FailureQ[agg], Return[prepared]];
		ast = Quiet[CodeParser`Abstract`Abstract[agg]];
		If[FailureQ[ast], Return[prepared]];
		prepared["CST"] = cst;
		If[!StringContainsQ[sourceText, "\t"], prepared["CSTTabs"] = cst];
		prepared["Agg"] = agg;
		prepared["AST"] = ast;
		prepared
	]

clearHighlightTaskState[] :=
	(
		$HighlightTask = None;
		$HighlightTaskKind = None;
		$HighlightTaskURI = None;
		$HighlightTaskID = None;
		$HighlightTaskContent = None;
		$HighlightTaskResult = None;
		$HighlightTaskStartTime = None;
		Null
	)

requeueHighlightPendingContents[] :=
	Module[{pending},
		pending =
			DeleteCases[
				Join[
					Replace[
						$HighlightTaskContent,
						{a_Association :> {a}, Except[_Association] -> {}}
					],
					Replace[$HighlightPendingContents, Except[_List] -> {}]
				],
				Nothing
			];
		clearHighlightTaskState[];
		$HighlightPendingContents = {};
		If[pending =!= {}, appendContentsToContentQueue[pending]];
		Null
	]

closeHighlightKernel[] :=
	Module[{kernel = $HighlightKernel},
		If[kernel =!= $Failed && kernel =!= None,
			Quiet[AbortKernels[kernel]];
			Quiet[CloseKernels[kernel]]
		];
		$HighlightKernel = None;
		$HighlightKernelBin = None;
		Null
	]

cleanupHighlightWorker[requeuePending_ : False] :=
	(
		If[TrueQ[requeuePending], requeueHighlightPendingContents[]];
		closeHighlightKernel[]
	)

highlightTaskTimedOutQ[] :=
	$HighlightTask =!= None &&
	NumberQ[$HighlightTaskStartTime] &&
	NumberQ[$HighlightTaskTimeout] &&
	AbsoluteTime[] - $HighlightTaskStartTime > $HighlightTaskTimeout

recoverTimedOutHighlightTask[] :=
	Module[{taskKind = $HighlightTaskKind, taskURI = $HighlightTaskURI},
		If[!highlightTaskTimedOutQ[], Return[False]];
		$HighlightWorkerLastFailureReason = "highlight worker task timed out";
		log[
			0,
			"WARNING: highlight worker task timed out; falling back to foreground for ",
			taskKind,
			" uri=",
			taskURI
		];
		requeueHighlightPendingContents[];
		closeHighlightKernel[];
		scheduleHighlightWorkerRelaunch[];
		True
	]

launchHighlightKernel[] :=
	Module[{
			kernel = $Failed,
			setupResult = $Failed,
			reason = "unknown"
		},
		If[highlightKernelAvailableQ[], Return[Null]];
		$HighlightWorkerLaunchAttempts = $HighlightWorkerLaunchAttempts + 1;
		Quiet[Needs["Parallel`"]];
		Quiet[Parallel`Settings`$RelaunchFailedKernels = True];
		kernel =
			Quiet[
				CheckAbort[
					Module[{ks = highlightWorkerLaunchKernels[]},
						If[
							ListQ[ks] &&
							Length[ks] > 0 &&
							MatchQ[First[ks], _KernelObject],
							First[ks],
							$Failed
						]
					],
					$Failed
				]
			];
		If[!workerKernelHealthyQ[kernel],
			If[MatchQ[kernel, _KernelObject],
				Quiet[AbortKernels[kernel]];
				Quiet[CloseKernels[kernel]]
			];
			reason =
				StringJoin[
					"LaunchKernels failed or returned an unhealthy kernel ",
					"(attempt ",
					ToString[$HighlightWorkerLaunchAttempts],
					")"
				];
			$HighlightWorkerLastFailureReason = reason;
			$HighlightKernel = $Failed;
			$HighlightKernelBin = $Failed;
			log[0, "WARNING: highlight worker launch failed: ", reason];
			notifyHighlightWorkerStatus[False, reason];
			scheduleHighlightWorkerRelaunch[];
			Return[Null]
		];
		loadFeatureModule["SemanticTokens"];
		loadFeatureModule["Color"];
		setupResult =
			Quiet[
				Check[
					ParallelEvaluate[
						Needs["CodeParser`"];
						Needs["CodeParser`Scoping`"];
						Needs["CodeParser`Utils`"],
						kernel
					];
					DistributeDefinitions[
						"LSPServer`",
						"LSPServer`Private`",
						"LSPServer`Utils`",
						"LSPServer`PacletIndex`",
						"LSPServer`PacletIndex`Private`",
						"LSPServer`SemanticTokens`",
						"LSPServer`SemanticTokens`Private`",
						"LSPServer`Color`",
						"LSPServer`Color`Private`",
						kernel
					],
					$Failed
				]
			];
		If[setupResult === $Failed,
			Quiet[AbortKernels[kernel]];
			Quiet[CloseKernels[kernel]];
			$HighlightWorkerLastFailureReason =
				"highlight worker provisioning failed";
			$HighlightKernel = $Failed;
			$HighlightKernelBin = $Failed;
			log[0, "WARNING: highlight worker provisioning failed"];
			notifyHighlightWorkerStatus[
				False,
				$HighlightWorkerLastFailureReason
			];
			scheduleHighlightWorkerRelaunch[];
			Return[Null]
		];
		$HighlightKernel = kernel;
		$HighlightKernelBin = $CommandLine[[1]];
		$HighlightWorkerLastFailureReason = None;
		log[
			0,
			"highlight worker kernel launched (attempt ",
			$HighlightWorkerLaunchAttempts,
			")"
		];
		notifyHighlightWorkerStatus[True, ""];
		Null
	]

maybeRelaunchDeadHighlightWorker[] :=
	Module[{},
		If[$HighlightKernel === None || $HighlightKernel === $Failed,
			Return[Null]
		];
		If[$HighlightTask =!= None,
			Return[Null]
		];
		If[
			AbsoluteTime[] - $HighlightWorkerLastHealthCheck <
			$WorkerHealthCheckInterval,
			Return[Null]
		];
		$HighlightWorkerLastHealthCheck = AbsoluteTime[];
		If[!workerKernelHealthyQ[$HighlightKernel],
			log[
				0,
				"WARNING: highlight worker became unresponsive; relaunching"
			];
			requeueHighlightPendingContents[];
			closeHighlightKernel[];
			$HighlightKernel = None;
			scheduleHighlightWorkerRelaunch[]
		];
		Null
	]

runHighlightWorker[
	kind_String,
	content_?AssociationQ,
	snapshot_?AssociationQ
] :=
	Module[{
			uri,
			id,
			entry,
			data,
			response,
			updatedEntry,
			wasStale
		},
		uri = contentURI[content];
		id = Lookup[content, "id", None];
		entry = Lookup[snapshot, "OpenFileEntry", Null];
		If[!AssociationQ[entry], Return[$Failed]];
		entry = prepareHighlightWorkerEntry[uri, entry];
		data = Lookup[snapshot, "SyntaxData", <||>];
		wasStale =
			TrueQ[Lookup[entry, "SemanticTokensStale", False]] ||
			TrueQ[Lookup[entry, "SemanticTokensIncomplete", False]];
		Quiet[
			CheckAbort[
				Block[{
						LSPServer`$OpenFilesMap = <|uri -> entry|>,
						LSPServer`$ContentQueue = {},
						LSPServer`$PreExpandContentQueue = {},
						LSPServer`$CancelMap = <||>,
						LSPServer`$SemanticTokens =
							TrueQ[Lookup[snapshot, "SemanticTokens", True]],
						LSPServer`$PendingSemanticTokenRequests = <||>,
						LSPServer`$PendingTokenRefresh = False,
						LSPServer`$PendingTokenRefreshTime = None,
						LSPServer`$HighlightKernel = None,
						LSPServer`$HighlightTask = None,
						LSPServer`PacletIndex`$PacletIndex =
							Lookup[snapshot, "PacletIndex", <||>],
						LSPServer`$WorkspaceRootPath =
							Lookup[snapshot, "WorkspaceRootPath", None],
						WolframLanguageSyntax`Generate`$builtinFunctions =
							Lookup[data, "BuiltinFunctions", {}],
						WolframLanguageSyntax`Generate`$constants =
							Lookup[data, "Constants", {}],
						WolframLanguageSyntax`Generate`$options =
							Lookup[data, "Options", {}],
						WolframLanguageSyntax`Generate`$experimentalSymbols =
							Lookup[data, "ExperimentalSymbols", {}],
						WolframLanguageSyntax`Generate`$obsoleteSymbols =
							Lookup[data, "ObsoleteSymbols", {}],
						WolframLanguageSyntax`Generate`$undocumentedSymbols =
							Lookup[data, "UndocumentedSymbols", {}],
						WolframLanguageSyntax`Generate`$sessionSymbols =
							Lookup[data, "SessionSymbols", {}],
						WolframLanguageSyntax`Generate`$badSymbols =
							Lookup[data, "BadSymbols", {}],
						WolframLanguageSyntax`Generate`$systemLongNames =
							Lookup[data, "SystemLongNames", {}]
					},
					response =
						LSPServer`handleContent[
							Append[content, "fromHighlightWorker" -> True]
						];
					updatedEntry = Lookup[LSPServer`$OpenFilesMap, uri, entry];
					<|
						"Kind"       -> kind,
						"URI"        -> uri,
						"ID"         -> id,
						"LastChange" -> Lookup[entry, "LastChange", None],
						"WasStale"   -> wasStale,
					"Response"   -> response,
					"Entry"      -> KeyTake[
						updatedEntry,
						$HighlightResultEntryKeys
					]
				|>
				],
				$Failed
			]
		]
	]

startHighlightTask[content_?AssociationQ, kind_String] :=
	Module[{uri, id, snapshot, task},
		If[!highlightKernelAvailableQ[], Return[False]];
		If[$HighlightTask =!= None, Return[False]];
		uri = contentURI[content];
		id = Lookup[content, "id", None];
		snapshot = buildHighlightWorkerSnapshot[uri];
		If[!AssociationQ[Lookup[snapshot, "OpenFileEntry", Null]],
			Return[False]
		];
		task =
			Quiet[
				Check[
					ParallelSubmit[
						{$HighlightKernel},
						runHighlightWorker[kind, content, snapshot]
					],
					$Failed
				]
			];
		If[task === $Failed,
			$HighlightWorkerLastFailureReason = "ParallelSubmit failed";
			$HighlightKernel = $Failed;
			scheduleHighlightWorkerRelaunch[];
			Return[False]
		];
		$HighlightTask = task;
		$HighlightTaskKind = kind;
		$HighlightTaskURI = uri;
		$HighlightTaskID = id;
		$HighlightTaskContent = content;
		$HighlightTaskResult = None;
		$HighlightTaskStartTime = AbsoluteTime[];
		True
	]

queueHighlightContent[content_?AssociationQ, kind_String] :=
	(
		$HighlightPendingContents =
			Append[
				Replace[$HighlightPendingContents, Except[_List] -> {}],
				Append[content, "highlightKind" -> kind]
			];
		True
	)

startNextHighlightTask[] :=
	Module[{next, rest, kind},
		If[$HighlightTask =!= None, Return[Null]];
		If[!highlightKernelAvailableQ[], Return[Null]];
		If[
			!ListQ[$HighlightPendingContents] ||
			$HighlightPendingContents === {},
			Return[Null]
		];
		next = First[$HighlightPendingContents];
		rest = Rest[$HighlightPendingContents];
		kind = Lookup[next, "highlightKind", "semantic-tokens"];
		next = KeyDrop[next, "highlightKind"];
		$HighlightPendingContents = rest;
		If[!startHighlightTask[next, kind],
			$HighlightPendingContents =
				Prepend[rest, Append[next, "highlightKind" -> kind]]
		];
		Null
	]

dispatchHighlightContent[content_?AssociationQ, kind_String] :=
	Module[{},
		If[TrueQ[Lookup[content, "fromHighlightWorker", False]], Return[False]];
		If[!highlightKernelAvailableQ[], Return[False]];
		If[$HighlightTask =!= None,
			Return[queueHighlightContent[content, kind]]
		];
		If[startHighlightTask[content, kind], True, False]
	]

dispatchHighlightSemanticTokens[content_?AssociationQ] :=
	dispatchHighlightContent[content, "semantic-tokens"]

dispatchHighlightDocumentColor[content_?AssociationQ] :=
	dispatchHighlightContent[content, "document-color"]

dispatchHighlightScopingData[content_?AssociationQ] :=
	dispatchHighlightContent[content, "scoping-data"]

dropPendingHighlightSemanticTokens[uri_String, ids_List] :=
	If[ids =!= {} && ListQ[$HighlightPendingContents],
		$HighlightPendingContents =
			Select[
				$HighlightPendingContents,
				!(
					AssociationQ[#] &&
					Lookup[#, "method", None] ===
					"textDocument/semanticTokens/fullFencepost" &&
					MemberQ[ids, Lookup[#, "id", None]] &&
					contentURI[#] === uri
				)&
			]
	]

mergeHighlightWorkerEntry[uri_String, workerEntry_?AssociationQ] :=
	Module[{entry},
		entry = Lookup[$OpenFilesMap, uri, Null];
		If[!AssociationQ[entry], Return[False]];
		Scan[
			Function[
				{key},
				If[KeyExistsQ[workerEntry, key],
					entry[key] = workerEntry[key],
					entry = KeyDrop[entry, key]
				]
			],
			$HighlightEntryMergeKeys
		];
		$OpenFilesMap[uri] = entry;
		True
	]

highlightWorkerResultCurrentQ[result_?AssociationQ] :=
	Module[{uri, entry},
		uri = Lookup[result, "URI", None];
		entry = Lookup[$OpenFilesMap, uri, Null];
		AssociationQ[entry] &&
		Lookup[entry, "LastChange", None] ===
		Lookup[result, "LastChange", Missing["NotAvailable"]]
	]

publishHighlightWorkerResult[result_?AssociationQ] :=
	Switch[Lookup[result, "Kind", None],
		"semantic-tokens",
			handleContent[
				<|
					"method" -> "textDocument/publishSemanticTokensWorkerResult",
					"result" -> result
				|>
			],
		"document-color",
			handleContent[
				<|
					"method" -> "textDocument/publishDocumentColorWorkerResult",
					"result" -> result
				|>
			],
		"scoping-data",
			handleContent[
				<|
					"method" -> "textDocument/publishScopingDataWorkerResult",
					"result" -> result
				|>
			],
		_,
			{}
	]

clearDiagnosticsTaskState[requeueClosedFileSweep_ : False] :=
	Module[{taskKind, taskURI},
		taskKind = $DiagnosticsTaskKind;
		taskURI = $DiagnosticsTaskURI;
		If[
			TrueQ[requeueClosedFileSweep] &&
			taskKind === "closed-file-sweep" &&
			StringQ[taskURI],
			requeueWorkspaceDiagnosticsSweepURI[taskURI]
		];
		$DiagnosticsTask = None;
		$DiagnosticsTaskURI = None;
		$DiagnosticsTaskKind = None;
		$DiagnosticsTaskResult = None;
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

abortDiagnosticsKernel[] :=
	Module[{kernel = $DiagnosticsKernel},
		If[kernel =!= $Failed && kernel =!= None, Quiet[AbortKernels[kernel]]];
		Null
	]

closeDiagnosticsKernel[] :=
	Module[{kernel = $DiagnosticsKernel},
		abortDiagnosticsKernel[];
		If[kernel =!= $Failed && kernel =!= None, Quiet[CloseKernels[kernel]]];
		$DiagnosticsKernel = None;
		$DiagnosticsKernelBin = None;
		Null
	]

cleanupDiagnosticsWorker[requeueClosedFileSweep_ : False] :=
	(
		clearDiagnosticsTaskState[requeueClosedFileSweep];
		closeDiagnosticsKernel[]
	)

cancelCurrentDiagnosticsTask[] :=
	Module[{},
		If[$DiagnosticsTask =!= None,
			clearDiagnosticsTaskState[True];
			abortDiagnosticsKernel[]
		];
		Null
	]

handleContent[
	content : KeyValuePattern["method" -> "workspace/processIndexing"]
] :=
	Module[{moreWork},
		log[1, "workspace/processIndexing: enter"];
		$WorkspaceIndexingQueued = False;
		If[!workspaceIndexingPendingQ[],
			finishWorkspaceIndexing[];
			log[1, "workspace/processIndexing: exit"];
			Return[{}]
		];
		moreWork = LSPServer`PacletIndex`ProcessPendingIndexFiles[];
		$WorkspaceIndexingLastRun = AbsoluteTime[];
		If[moreWork,
			$IndexingWasActive = True;
			queueWorkspaceIndexing[
				"workspace/processIndexing: re-queued remaining index work"
			],
			finishWorkspaceIndexing[]
		];
		log[1, "workspace/processIndexing: exit"];
		{}
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
		filtered =
			Select[DeleteDuplicates[uris], workspaceDiagnosticsSweepURIQ];
		If[filtered =!= {},
			$WorkspaceDiagnosticsSweepURIs =
				DeleteDuplicates[Join[$WorkspaceDiagnosticsSweepURIs, filtered]]
		];
		Null
	]
queueWorkspaceDiagnosticsSweep[] :=
	(* Full workspace closed-file sweeps are intentionally disabled; only explicit
	   changed URI lists may enqueue closed-file diagnostics. *)
	Null

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
		If[symbols === {}, Return[{}]];
		referencingURIs =
			DeleteDuplicates @
			Flatten[
				Function[
					{sym},
					Lookup[
						Replace[
							LSPServer`PacletIndex`GetSymbolReferences[sym],
							Except[_List] -> {}
						],
						"uri",
						{}
					]
				] /@ symbols
			];
		Select[referencingURIs, KeyExistsQ[$OpenFilesMap, #]&]
	]

invalidateWorkspaceDiagnostics[uris_List] :=
	Scan[
		Function[
			{uri},
			Module[{entry},
				entry = Lookup[$OpenFilesMap, uri, Null];
				If[AssociationQ[entry],
					entry["WorkspaceLints"] = Null;
					entry = KeyDrop[entry, "WorkspaceLintsLastChange"];
					If[KeyExistsQ[entry, "SemanticTokens"],
						entry["SemanticTokensStale"] = True
					];
					$OpenFilesMap[uri] = entry
				]
			]
		],
		DeleteDuplicates[uris]
	]

workspaceIndexingPendingQ[] :=
	Length[LSPServer`PacletIndex`Private`$PendingExternalDepFiles] > 0 ||
	Length[LSPServer`PacletIndex`$PendingIndexFiles] > 0 ||
	Length[LSPServer`PacletIndex`$PendingReferenceFiles] > 0 ||
	Length[LSPServer`PacletIndex`Private`$PendingDepDiscovery] > 0

backgroundIntervalReadyQ[last_, interval_] :=
	Module[{now, nLast, nInterval},
		now = AbsoluteTime[];
		nLast = Replace[last, Except[_?NumberQ] -> 0];
		nInterval = Replace[interval, Except[_?NumberQ] -> 0];
		now - nLast >= nInterval
	]

queueWorkspaceIndexing[reason_String : ""] :=
	If[workspaceIndexingPendingQ[] &&
	!TrueQ[$WorkspaceIndexingQueued] &&
	backgroundIntervalReadyQ[
		$WorkspaceIndexingLastRun,
		$WorkspaceIndexingInterval
	],
		If[reason =!= "", log[1, reason]];
		$WorkspaceIndexingQueued = True;
		appendContentsToContentQueue[
			{<|"method" -> "workspace/processIndexing", "deferrable" -> True|>}
		]
	]

finishWorkspaceIndexing[] :=
	If[$IndexingWasActive,
		Module[{urisToRefresh},
			$IndexingWasActive = False;
			urisToRefresh =
				Select[
					Keys[$OpenFilesMap],
					Module[{entry = Lookup[$OpenFilesMap, #, Null]},
						AssociationQ[entry] &&
						!ListQ[Lookup[entry, "WorkspaceLints", Null]]
					]&
				];
			log[
				1,
				"Indexing complete. Refreshing workspace diagnostics for ",
				Length[urisToRefresh],
				" open files with pending workspace lints."
			];
			If[urisToRefresh =!= {},
				invalidateWorkspaceDiagnostics[urisToRefresh];
				loadFeatureModule["Diagnostics"];
				Scan[
					LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics,
					urisToRefresh
				];
				queueSemanticTokensRefresh[
					"DBG-ST: indexing done; refreshing affected semantic tokens"
				]
			]
		]
	]

ProcessScheduledJobs[] :=
	Catch[
		Module[
			{
				openFilesMapCopy,
				entryCopy,
				jobs,
				res,
				methods,
				contents,
				toRemove,
				job,
				toRemoveIndices,
				contentsToAdd
			},
			(*
			Do not process any scheduled jobs after shutdown
			*)
			If[$ServerState == "shutdown", Throw[Null]];
			If[workerRelaunchDueQ[],
				$DiagnosticsKernelLaunchAfter = None;
				launchWorkerKernel[]
			];
			(* Detect a worker that died/hung and schedule a relaunch (throttled). *)
			maybeRelaunchDeadWorker[];
			If[highlightWorkerRelaunchDueQ[] &&
			Length[$ContentQueue] == 0 &&
			AbsoluteTime[] - $QueueLastNonEmptyTime >= 0.2,
				$HighlightKernelLaunchAfter = None;
				launchHighlightKernel[]
			];
			recoverTimedOutHighlightTask[];
			maybeRelaunchDeadHighlightWorker[];
			If[$HighlightTask =!= None && !highlightKernelAvailableQ[],
				requeueHighlightPendingContents[]
			];
			If[$HighlightTask =!= None && highlightKernelAvailableQ[],
				Module[{taskResult, taskKind, taskURI, taskID},
					taskResult = parallelTaskResult[$HighlightTask];
					If[taskResult =!= Missing["StillRunning"],
						taskKind = $HighlightTaskKind;
						taskURI = $HighlightTaskURI;
						taskID = $HighlightTaskID;
						clearHighlightTaskState[];
						appendContentsToContentQueue[
							{
								<|
									"method"   -> Switch[taskKind,
										"document-color",
											"textDocument/publishDocumentColorWorkerResult",
										"scoping-data",
											"textDocument/publishScopingDataWorkerResult",
										_,
											"textDocument/publishSemanticTokensWorkerResult"
									],
									"result"   -> taskResult,
									"id"       -> taskID,
									"params"   -> <|
										"textDocument" -> <|"uri" -> taskURI|>
									|>,
									"priority" -> True
								|>
							}
						];
						startNextHighlightTask[]
					]
				]
			];
			If[$HoverTask =!= None &&
			$DiagnosticsKernel =!= $Failed &&
			$DiagnosticsKernel =!= None,
				Module[{taskResult, taskURI, taskID},
					taskResult = parallelTaskResult[$HoverTask];
					If[taskResult =!= Missing["StillRunning"],
						taskURI = $HoverTaskURI;
						taskID = $HoverTaskID;
						clearHoverTaskState[];
						$HoverTaskResult = taskResult;
						appendContentsToContentQueue[
							{
								<|
									"method" -> "textDocument/publishHoverResult",
									"id"     -> taskID,
									"params" -> <|
										"textDocument" -> <|"uri" -> taskURI|>
									|>
								|>
							}
						]
					]
				]
			];
			If[$DiagnosticsTask =!= None &&
			$DiagnosticsKernel =!= $Failed &&
			$DiagnosticsKernel =!= None,
				Module[{taskResult, taskKind, taskURI},
					taskResult = parallelTaskResult[$DiagnosticsTask];
					If[taskResult =!= Missing["StillRunning"],
						taskKind = $DiagnosticsTaskKind;
						taskURI = $DiagnosticsTaskURI;
						$DiagnosticsTask = None;
						$DiagnosticsTaskURI = None;
						$DiagnosticsTaskKind = None;
						$DiagnosticsTaskStartTime = None;
						If[AssociationQ[taskResult] &&
						Lookup[taskResult, "URI", None] === taskURI,
							$DiagnosticsTaskResult = taskResult;
							appendContentsToContentQueue[
								{
									If[taskKind === "closed-file-sweep",
										<|
											"method" -> "textDocument/publishClosedFileDiagnostics",
											"params" -> <|
												"textDocument" -> <|
													"uri" -> taskURI
												|>
											|>
										|>,
										<|
											"method" -> "textDocument/mergeWorkspaceLints",
											"params" -> <|
												"textDocument" -> <|
													"uri" -> taskURI
												|>
											|>
										|>
									]
								}
							],
							$DiagnosticsTaskResult = None
						]
					]
				]
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
						StringJoin[
							"ProcessScheduledJobs: queued cooperative ",
							"workspace indexing"
						]
					],
					finishWorkspaceIndexing[]
				]
			];
			(*
			Track the last time the queue was non-empty, for the kernel launch idle guard.
			*)
			If[Length[$ContentQueue] > 0,
				$QueueLastNonEmptyTime = AbsoluteTime[]
			];
			If[$WorkspaceBootstrapAfter =!= None &&
			AbsoluteTime[] >= $WorkspaceBootstrapAfter &&
			Length[$ContentQueue] == 0 &&
			AbsoluteTime[] - $QueueLastNonEmptyTime >= 1,
				$WorkspaceBootstrapAfter = None;
				appendContentsToContentQueue[
					{
						<|
							"method"     -> "workspace/bootstrapWorkspaceIndex",
							"deferrable" -> True
						|>
					}
				]
			];
			(*
			When the queue is idle, process one explicitly changed closed workspace file
			at a time. The no-argument workspace sweep is disabled so diagnostics do not
			rescan the whole workspace after initialization.
			*)
			Module[{canStartSweep},
				canStartSweep =
					ListQ[$WorkspaceDiagnosticsSweepURIs] &&
					Length[$WorkspaceDiagnosticsSweepURIs] > 0 &&
					Length[$ContentQueue] == 0 &&
					backgroundIntervalReadyQ[
						$QueueLastNonEmptyTime,
						$ClosedFileDiagnosticsIdleDelay
					] &&
					backgroundIntervalReadyQ[
						$ClosedFileDiagnosticsLastRun,
						$ClosedFileDiagnosticsInterval
					];
				If[canStartSweep,
					Module[{nextPos, nextURI},
						nextPos =
							SelectFirst[
								Range[Length[$WorkspaceDiagnosticsSweepURIs]],
								workspaceDiagnosticsSweepURIQ[
									$WorkspaceDiagnosticsSweepURIs[[#]]
								]&,
								Missing["NotFound"]
							];
						If[IntegerQ[nextPos],
							nextURI = $WorkspaceDiagnosticsSweepURIs[[nextPos]];
							$WorkspaceDiagnosticsSweepURIs =
								Delete[$WorkspaceDiagnosticsSweepURIs, nextPos];
							$ClosedFileDiagnosticsLastRun = AbsoluteTime[];
							loadFeatureModule["Diagnostics"];
							LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics[
								nextURI
							]
						]
					]
				]
			];
			(*
			If the client never acknowledged the workspace/semanticTokens/refresh request
			(no response received, or the response arrived with an unexpected id), the
			$PendingTokenRefresh flag stays True forever and blocks all future refreshes.
			Reset it after a 3-second timeout so the server can recover automatically.
			*)
			If[TrueQ[$PendingTokenRefresh] &&
			NumberQ[$PendingTokenRefreshTime] &&
			AbsoluteTime[] - $PendingTokenRefreshTime > 3,
				log[
					0,
					StringJoin[
						"DBG-ST: workspace/semanticTokens/refresh ack not ",
						"received within 3s; resetting $PendingTokenRefresh"
					]
				];
				$PendingTokenRefresh = False;
				$PendingTokenRefreshTime = None
			];
			openFilesMapCopy = $OpenFilesMap;
			contents = {};
			KeyValueMap[
				Function[
					{uri, entry},
					jobs = Lookup[entry, "ScheduledJobs", {}];
					toRemoveIndices = {};
					Do[
						job = jobs[[j]];
						res = Catch[job[entry]];
						If[!MatchQ[res, {{___String}, True | False}],
							log[
								0,
								StringJoin[
									"WARNING: dropping scheduled job with ",
									"invalid result for "
								],
								uri,
								": ",
								res
							];
							AppendTo[toRemoveIndices, {j}];
							Continue[]
						];
						{methods, toRemove} = res;
						contentsToAdd =
							<|
								"method" -> #,
								"params" -> <|
									"textDocument" -> <|"uri" -> uri|>
								|>
							|>& /@ methods;
						contents = Join[contents, contentsToAdd];
						If[toRemove, AppendTo[toRemoveIndices, {j}]],
						{j, 1, Length[jobs]}
					];
					If[toRemoveIndices =!= {},
						jobs = Delete[jobs, toRemoveIndices];
						entryCopy = entry;
						entryCopy["ScheduledJobs"] = jobs;
						$OpenFilesMap[uri] = entryCopy
					]
				],
				openFilesMapCopy
			];
			If[contents =!= {},
				contents = expandContents[contents];
				appendContentsToContentQueue[contents];
			]
		]
	]

(*
input: JSON RPC assoc

returns: a list of JSON RPC assocs
*)
LSPEvaluate[content_ (*no Association here, allow everything*)] :=
	Catch[
		Module[
			{contents, methodName},
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
					contents = handleContentAfterShutdown[content],
				True,
					contents = handleContent[content]
			];
			If[
				MatchQ[contents, Failure["URINotFound", _]],
				(*
				This can happen under some circumstances

				A file is closed, and something like publishDiagnostics is after the close in the queue

				Do not kill the kernel for this
				*)
				log[1, "\n\n"];
				log[
					1,
					"Internal assert 3 failed: list of Associations: ",
					contents
				];
				log[1, "\n\n"];
				Throw[{}]
			];
			If[!MatchQ[contents, {_?AssociationQ...}],
				log[0, "\n\n"];
				log[
					0,
					"Internal assert 4 failed: list of Associations: ",
					contents
				];
				log[0, "\n\n"];
				methodName =
					If[AssociationQ[content],
						Lookup[content, "method", "message"],
						"message"
					];
				contents =
					jsonRPCErrorResponse[
						content,
						"InternalError",
						StringJoin[
							"Internal server error while handling ",
							ToString[methodName]
						]
					]
			];
			contents
		]
	]

(*
  runDiagnostics expands to runFastDiagnostics which already publishes
  partial diagnostics.  A separate publishDiagnostics was re-reading the
  entry before workspace lints arrived, overwriting real results with an
  empty array.  Removed the redundant publishDiagnostics here;
  the workspace-diagnostics slow tier publishes a final update itself
  once it completes.
*)
$didOpenMethods = {}

$didCloseMethods = {"textDocument/publishDiagnostics"}

$didSaveMethods = {}

$didChangeMethods = {"textDocument/runDiagnostics"}

$didChangeScheduledJobs = {}

RegisterDidOpenMethods[meths_] :=
	($didOpenMethods = Join[$didOpenMethods, meths])

RegisterDidCloseMethods[meths_] :=
	($didCloseMethods = Join[$didCloseMethods, meths])

RegisterDidSaveMethods[meths_] :=
	($didSaveMethods = Join[$didSaveMethods, meths])

RegisterDidChangeMethods[meths_] :=
	($didChangeMethods = Join[$didChangeMethods, meths])

RegisterDidOpenScheduledJobs[jobs_] :=
	($didOpenScheduledJobs = Join[$didOpenScheduledJobs, jobs])

RegisterDidCloseScheduledJobs[jobs_] :=
	($didCloseScheduledJobs = Join[$didCloseScheduledJobs, jobs])

RegisterDidSaveScheduledJobs[jobs_] :=
	($didSaveScheduledJobs = Join[$didSaveScheduledJobs, jobs])

RegisterDidChangeScheduledJobs[jobs_] :=
	($didChangeScheduledJobs = Join[$didChangeScheduledJobs, jobs])

(*
content: JSON-RPC Association

returns: a list of associations (possibly empty), each association represents JSON-RPC
*)
handleContent[content : KeyValuePattern["method" -> "initialize"]] :=
	Module[{
			id,
			params,
			capabilities,
			textDocument,
			codeAction,
			codeActionLiteralSupport,
			codeActionKind,
			valueSet,
			codeActionProviderValue,
			initializationOptions,
			implicitTokens,
			bracketMatcher,
			debugBracketMatcher,
			clientName,
			semanticTokensProviderValue,
			inlayHintProviderValue,
			semanticTokens,
			contents,
			documentSymbol,
			hierarchicalDocumentSymbolSupport,
			performanceOptions,
			setPositiveNumberOption,
			setPositiveIntegerOption
		},
		log[1, "initialize: Enter"];
		id = content["id"];
		params = content["params"];
		If[KeyExistsQ[params, "initializationOptions"],
			initializationOptions = params["initializationOptions"];
			log[2, "initializationOptions: ", initializationOptions];
			(*
			initializationOptions may be Null, such as from Jupyter Lab LSP
			*)
			If[
				AssociationQ[initializationOptions],
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
					debugBracketMatcher =
						initializationOptions["debugBracketMatcher"];
					$DebugBracketMatcher = debugBracketMatcher
				];
				If[KeyExistsQ[initializationOptions, "semanticTokens"],
					semanticTokens = initializationOptions["semanticTokens"];
					$SemanticTokens = semanticTokens
				];
				If[KeyExistsQ[initializationOptions, "inlayHints"],
					$InlayHints = TrueQ[initializationOptions["inlayHints"]]
				];
				If[KeyExistsQ[initializationOptions, "performance"] &&
				AssociationQ[initializationOptions["performance"]],
					performanceOptions = initializationOptions["performance"];
					setPositiveNumberOption =
						Function[
							{key, sym},
							If[
								KeyExistsQ[performanceOptions, key] &&
								NumberQ[performanceOptions[key]] &&
								performanceOptions[key] >= 0,
								sym = performanceOptions[key]
							],
							HoldRest
						];
					setPositiveIntegerOption =
						Function[
							{key, sym},
							If[
								KeyExistsQ[performanceOptions, key] &&
								IntegerQ[performanceOptions[key]] &&
								performanceOptions[key] > 0,
								sym = performanceOptions[key]
							],
							HoldRest
						];
					setPositiveNumberOption["idleLoopPause", $IdleLoopPause];
					setPositiveNumberOption[
						"workspaceIndexingInterval",
						$WorkspaceIndexingInterval
					];
					setPositiveIntegerOption[
						"workspaceIndexingBatchSize",
						$WorkspaceIndexingBatchSize
					];
					setPositiveIntegerOption[
						"workspaceReferenceBatchSize",
						$WorkspaceReferenceBatchSize
					];
					setPositiveIntegerOption[
						"externalDependencyIndexingBatchSize",
						$ExternalDependencyIndexingBatchSize
					];
					setPositiveIntegerOption[
						"dependencyDiscoveryBatchSize",
						$DependencyDiscoveryBatchSize
					];
					setPositiveIntegerOption[
						"externalDependencyFileLimit",
						$ExternalDependencyFileLimit
					];
					setPositiveNumberOption[
						"closedFileDiagnosticsIdleDelay",
						$ClosedFileDiagnosticsIdleDelay
					];
					setPositiveNumberOption[
						"closedFileDiagnosticsInterval",
						$ClosedFileDiagnosticsInterval
					];
					setPositiveIntegerOption[
						"closedFileDiagnosticsMaxTextLength",
						$ClosedFileDiagnosticsMaxTextLength
					]
				];
			];
			log[1, "initialize: Exit"];
		];
		(*
		Only use confidenceLevel from initializationOptions if no ConfidenceLevel option was passed to StartServer[]
		*)
		Which[
			NumberQ[$ConfidenceLevelOption],
			$ConfidenceLevel = $ConfidenceLevelOption,
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
		If[KeyExistsQ[params, "workspaceFolders"] &&
		ListQ[params["workspaceFolders"]] &&
		Length[params["workspaceFolders"]] > 0,
			$WorkspaceRootPath =
				normalizeURI[params["workspaceFolders"][[1]]["uri"]];
			If[$Debug2,
				log[
					"workspace root from workspaceFolders: ",
					$WorkspaceRootPath
				]
			],
			If[KeyExistsQ[params, "rootUri"] && StringQ[params["rootUri"]],
				$WorkspaceRootPath = normalizeURI[params["rootUri"]];
				If[$Debug2,
					log["workspace root from rootUri: ", $WorkspaceRootPath]
				],
				If[KeyExistsQ[params, "rootPath"] &&
				StringQ[params["rootPath"]],
					$WorkspaceRootPath = params["rootPath"];
					If[$Debug2,
						log[
							"workspace root from rootPath: ",
							$WorkspaceRootPath
						]
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
			If[clientName == "Eclipse IDE", $ColorProvider = False]
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
			codeActionProviderValue = <|"codeActionKinds" -> {"quickfix"}|>,
			codeActionProviderValue = True
		];
		If[$AllowedImplicitTokens != {},
			RegisterDidOpenMethods[
				{
					"textDocument/runImplicitTokens",
					"textDocument/publishImplicitTokens"
				}
			];
			RegisterDidCloseMethods[{"textDocument/publishImplicitTokens"}];
			RegisterDidSaveMethods[{}];
			RegisterDidChangeMethods[
				{
					"textDocument/clearImplicitTokens",
					"textDocument/publishImplicitTokens"
				}
			];
			RegisterDidChangeScheduledJobs[
				{
					Function[
						{entry},
						If[
							Now - entry["LastChange"] >
							Quantity[
								$ImplicitTokensDelayAfterLastChange,
								"Seconds"
							],
							{
								{
									"textDocument/runImplicitTokens",
									"textDocument/publishImplicitTokens"
								},
								True
							},
							{{}, False}
						]
					]
				}
			]
		];
		If[$BracketMatcher,
			RegisterDidOpenMethods[
				{
					"textDocument/runBracketMismatches",
					"textDocument/suggestBracketEdits",
					"textDocument/publishBracketMismatches"
				}
			];
			RegisterDidCloseMethods[{"textDocument/publishBracketMismatches"}];
			RegisterDidSaveMethods[{}];
			RegisterDidChangeMethods[
				{
					"textDocument/clearBracketMismatches",
					"textDocument/publishBracketMismatches"
				}
			];
			RegisterDidChangeScheduledJobs[
				{
					Function[
						{entry},
						If[
							Now - entry["LastChange"] >
							Quantity[
								$BracketMatcherDelayAfterLastChange,
								"Seconds"
							],
							{
								{
									"textDocument/runBracketMismatches",
									"textDocument/suggestBracketEdits",
									"textDocument/publishBracketMismatches"
								},
								True
							},
							{{}, False}
						]
					]
				}
			];
			$ExecuteCommandProvider =
				Merge[
					{
						$ExecuteCommandProvider,
						<|
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
						|>
					},
					Flatten
				]
		];
		If[$SemanticTokens,
			If[
				KeyExistsQ[textDocument, "semanticTokens"],
				loadFeatureModule["SemanticTokens"];
				semanticTokensProviderValue =
					<|
						"legend" -> <|
							"tokenTypes"     -> Keys[
								LSPServer`SemanticTokens`$SemanticTokenTypes
							],
							"tokenModifiers" -> Keys[
								LSPServer`SemanticTokens`$SemanticTokenModifiers
							]
						|>,
						"range"  -> False,
						"full"   -> <|"delta" -> False|>
					|>,
				(*
				if client does not advertise semantic token support, then do not respond with any support
				*)
				semanticTokensProviderValue = Null
			];,
			semanticTokensProviderValue = Null
		];
		inlayHintProviderValue =
			If[TrueQ[$InlayHints], <|"resolveProvider" -> False|>, Null];
		If[KeyExistsQ[textDocument, "documentSymbol"],
			documentSymbol = textDocument["documentSymbol"];
			hierarchicalDocumentSymbolSupport =
				Lookup[
					documentSymbol,
					"hierarchicalDocumentSymbolSupport",
					False
				];
			$HierarchicalDocumentSymbolSupport =
				TrueQ[hierarchicalDocumentSymbolSupport]
		];
		$kernelInitializeTime = Now;
		log[2, "time to intialize: ", $kernelInitializeTime - $kernelStartTime];
		contents =
			{
				<|
					"jsonrpc" -> "2.0",
					"id"      -> id,
					"result"  -> <|
						"capabilities" -> <|
							"referencesProvider"              -> True,
							"textDocumentSync"                -> <|
								"openClose" -> True,
								"save"      -> <|"includeText" -> False|>,
								"change"    -> $TextDocumentSyncKind["Full"]
							|>,
							(* "completionProvider" -> <|
							  "resolveProvider" -> False,
							  "triggerCharacters" -> {}
							|>, *)
							"codeActionProvider"              -> codeActionProviderValue,
							"colorProvider"                   -> $ColorProvider,
							"hoverProvider"                   -> True,
							"definitionProvider"              -> True,
							"documentFormattingProvider"      -> True,
							"documentRangeFormattingProvider" -> True,
							"executeCommandProvider"          -> $ExecuteCommandProvider,
							"documentSymbolProvider"          -> True,
							"selectionRangeProvider"          -> True,
							"semanticTokensProvider"          -> semanticTokensProviderValue,
							"foldingRangeProvider"            -> True,
							(*
							Completion support
							triggerCharacters:
							  $ - for system variables like $Version
							  ` - for context paths like Developer`
							  [ - for function arguments
							  " - for association string keys like data["
							*)
							"completionProvider"              -> <|
								"triggerCharacters" -> {"$", "`", "[", "\""},
								"resolveProvider"   -> True
							|>,
							"inlayHintProvider"               -> inlayHintProviderValue,
							(*
							Workspace symbol search support
							*)
							"workspaceSymbolProvider"         -> True,
							(*
							Workspace folders support
							*)
							"workspace"                       -> <|
								"workspaceFolders" -> <|
									"supported"           -> True,
									"changeNotifications" -> True
								|>
							|>
						|>
					|>
				|>
			};
		contents
	]
handleContent[content : KeyValuePattern["method" -> "initialized"]] :=
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
			LSPServer`SemanticTokens`Private`isObsoleteSymbol[
				"DirectedInfinity"
			];
			LSPServer`SemanticTokens`Private`isUndocumentedSymbol[
				"SequenceHold"
			];
			LSPServer`SemanticTokens`Private`isSessionSymbol["Print"];
			LSPServer`SemanticTokens`Private`isBadSymbol["SetDelayedDelayed"]
		];
		(*
		Initialize paclet index if workspace root is set
		*)
		If[
			StringQ[$WorkspaceRootPath],
			(* Keep workspace bootstrap off the foreground queue until the loop has been
			   idle briefly so startup didOpen and semantic-token requests land first. *)
			$WorkspaceBootstrapAfter = AbsoluteTime[] + 1
		];
		If[$HighlightKernel === None && $HighlightKernelLaunchAfter === None,
			$HighlightKernelLaunchAfter = AbsoluteTime[] + 0.2
		];
		If[
			$DiagnosticsKernel === None &&
			$DiagnosticsKernelLaunchAfter === None,
			$DiagnosticsKernelLaunchAfter = AbsoluteTime[] + 5
		];
		warningMessages = ServerDiagnosticWarningMessages[];
		log[2, "warningMessages: ", warningMessages];
		res =
			<|
				"jsonrpc" -> "2.0",
				"method"  -> "window/showMessage",
				"params"  -> <|
					"type"    -> $MessageType["Warning"],
					"message" -> #
				|>
			|>& /@ warningMessages;
		log[1, "initialized: Exit"];
		res
	]
handleContent[
	content :
		KeyValuePattern["method" -> "workspace/bootstrapClosedFileDiagnostics"]
] :=
	Module[{nextPos, nextURI},
		log[1, "workspace/bootstrapClosedFileDiagnostics: Enter"];
		If[!ListQ[$WorkspaceDiagnosticsSweepURIs] ||
		$WorkspaceDiagnosticsSweepURIs === {},
			log[1, "workspace/bootstrapClosedFileDiagnostics: Exit"];
			Return[{}]
		];
		nextPos =
			SelectFirst[
				Range[Length[$WorkspaceDiagnosticsSweepURIs]],
				workspaceDiagnosticsSweepURIQ[
					$WorkspaceDiagnosticsSweepURIs[[#]]
				]&,
				Missing["NotFound"]
			];
		If[IntegerQ[nextPos],
			nextURI = $WorkspaceDiagnosticsSweepURIs[[nextPos]];
			$WorkspaceDiagnosticsSweepURIs =
				Delete[$WorkspaceDiagnosticsSweepURIs, nextPos];
			loadFeatureModule["Diagnostics"];
			LSPServer`Diagnostics`Private`dispatchClosedFileDiagnostics[nextURI]
		];
		log[1, "workspace/bootstrapClosedFileDiagnostics: Exit"];
		{}
	]
handleContent[
	content : KeyValuePattern["method" -> "workspace/bootstrapWorkspaceIndex"]
] :=
	Module[{},
		log[1, "workspace/bootstrapWorkspaceIndex: Enter"];
		If[!StringQ[$WorkspaceRootPath],
			log[1, "workspace/bootstrapWorkspaceIndex: Exit"];
			Return[{}]
		];
		If[$Debug2, log["initializing paclet index for: ", $WorkspaceRootPath]];
		InitializePacletIndex[$WorkspaceRootPath];
		(*
		Load project-level ignore configuration (.wllintrc)
		*)
		If[$Debug2, log["loading project ignore config"]];
		LoadProjectIgnoreConfig[$WorkspaceRootPath];
		log[1, "workspace/bootstrapWorkspaceIndex: Exit"];
		{}
	]
handleContent[content : KeyValuePattern["method" -> "shutdown"]] :=
	Catch[
		Module[{id},
			log[1, "shutdown: Enter"];
			id = content["id"];
			If[Lookup[$CancelMap, id, False],
				$CancelMap[id] =.;
				If[$Debug2, log["$CancelMap: ", $CancelMap]];
				Throw[{<|"jsonrpc" -> "2.0", "id" -> id, "result" -> Null|>}]
			];
			$WorkspaceBootstrapAfter = None;
			$HighlightKernelLaunchAfter = None;
			$DiagnosticsKernelLaunchAfter = None;
			clearHoverTaskState[];
			cleanupHighlightWorker[False];
			cleanupDiagnosticsWorker[False];
			$OpenFilesMap =.;
			$ServerState = "shutdown";
			log[1, "shutdown: Exit"];
			{<|"jsonrpc" -> "2.0", "id" -> id, "result" -> Null|>}
		]
	]
(*
Unexpected call to exit
*)
handleContent[content : KeyValuePattern["method" -> "exit"]] :=
	Module[{},
		log[1, "exit: Enter"];
		log[1, "exit: Exit"];
		exitSemiGracefully[]
	]
handleContent[content : KeyValuePattern["method" -> "$/cancelRequest"]] :=
	Catch[
		Module[{params, id},
			If[$Debug2, log["$/cancelRequest: enter"]];
			params = content["params"];
			id = params["id"];
			If[!KeyExistsQ[$CancelMap, id], Throw[{}]];
			log[2, "cancel was not handled: ", id];
			$CancelMap[id] =.;
			log[2, "$CancelMap: ", $CancelMap];
			log[1, "$/cancelRequest: exit"];
			{}
		]
	]
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
handleContent[
	content :
		KeyValuePattern["method" -> meth_ /; StringMatchQ[meth, "$/" ~~ __]]
] :=
	Module[{id},
		log[1, StringJoin[ meth, ": enter"]];
		If[
			KeyExistsQ[content, "id"],
			(*
			has id, so this is a request
			*)
			id = content["id"];
			{
				<|
					"jsonrpc" -> "2.0",
					"id"      -> id,
					"error"   -> <|
						"code"    -> $ErrorCodes["MethodNotFound"],
						"message" -> "Method Not Found"
					|>
				|>
			},
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
	!TrueQ[
		Lookup[
			$FeatureModulesLoaded,
			First[$FeatureHandleMethods[method]],
			False
		]
	] :=
	Module[{},
		loadFeatureModulesForMethod[method];
		handleContent[content]
	]
handleContent[content : KeyValuePattern["method" -> method_String]] :=
	Module[{},
		log[0, "Unknown LSP method: ", method];
		jsonRPCErrorResponse[content, "MethodNotFound", "Method Not Found"]
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
		$ContentQueue =
			Select[
				Replace[$ContentQueue, Except[_List] -> {}],
				!(
					AssociationQ[#] &&
					Lookup[#, "method", None] ===
					"textDocument/semanticTokens/fullFencepost" &&
					MemberQ[ids, Lookup[#, "id", None]] &&
					Lookup[
						Lookup[Lookup[#, "params", <||>], "textDocument", <||>],
						"uri",
						None
					] ===
					uri
				)&
			];
		dropPendingHighlightSemanticTokens[uri, ids]
	]

supersededSemanticTokenFencepostContents[uri_String, ids_List] :=
	<|
		"method"     -> "textDocument/semanticTokens/fullFencepost",
		"id"         -> #,
		"params"     -> <|"textDocument" -> <|"uri" -> uri|>|>,
		"superseded" -> True,
		"priority"   -> True
	|>& /@ ids

forgetPendingSemanticTokenRequest[uri_String, id_Integer] :=
	Module[{ids},
		If[!AssociationQ[$PendingSemanticTokenRequests],
			$PendingSemanticTokenRequests = <||>
		];
		ids = DeleteCases[Lookup[$PendingSemanticTokenRequests, uri, {}], id];
		If[ids === {},
			$PendingSemanticTokenRequests =
				KeyDrop[$PendingSemanticTokenRequests, uri],
			$PendingSemanticTokenRequests[uri] = ids
		]
	]

pendingSemanticTokenFencepostIDsToRecover[uri_String] :=
	Module[{pendingIDs, queuedFencepostIDs},
		If[!AssociationQ[$PendingSemanticTokenRequests],
			$PendingSemanticTokenRequests = <||>
		];
		pendingIDs = Lookup[$PendingSemanticTokenRequests, uri, {}];
		queuedFencepostIDs =
			Cases[
				$ContentQueue,
				KeyValuePattern[
					{
						"method" -> "textDocument/semanticTokens/fullFencepost",
						"id"     -> pendingID_,
						"params" -> KeyValuePattern[
							"textDocument" -> KeyValuePattern["uri" -> uri]
						]
					}
				] :> pendingID
			];
		Complement[pendingIDs, queuedFencepostIDs]
	]

pendingSemanticTokenFencepostResponses[uri_String, ids_List] :=
	Flatten[
		Function[
			{pendingID},
			handleContent[
				<|
					"method" -> "textDocument/semanticTokens/fullFencepost",
					"id"     -> pendingID,
					"params" -> <|"textDocument" -> <|"uri" -> uri|>|>
				|>
			]
		] /@ ids,
		1
	]

recoverPendingSemanticTokenFenceposts[uri_String, reason_String : ""] :=
	Module[{idsToRecover},
		If[!TrueQ[$SemanticTokens], Return[{}]];
		idsToRecover = pendingSemanticTokenFencepostIDsToRecover[uri];
		If[idsToRecover === {}, Return[{}]];
		If[reason =!= "",
			log[0, reason, " recovered=", Length[idsToRecover], " uri=", uri]
		];
		pendingSemanticTokenFencepostResponses[uri, idsToRecover]
	]

queuePendingSemanticTokenFenceposts[uri_String, reason_String : ""] :=
	Module[{idsToRecover},
		If[!TrueQ[$SemanticTokens], Return[0]];
		idsToRecover = pendingSemanticTokenFencepostIDsToRecover[uri];
		If[idsToRecover === {}, Return[0]];
		If[reason =!= "",
			log[0, reason, " recovered=", Length[idsToRecover], " uri=", uri]
		];
		appendContentsToContentQueue[
			<|
				"method"     -> "textDocument/semanticTokens/fullFencepost",
				"id"         -> #,
				"params"     -> <|"textDocument" -> <|"uri" -> uri|>|>,
				"priority"   -> False,
				"deferrable" -> True
			|>& /@ idsToRecover
		];
		Length[idsToRecover]
	]

semanticTokensRefreshQueuedQ[] :=
	AnyTrue[
		Replace[$ContentQueue, Except[_List] -> {}],
		AssociationQ[#] &&
		Lookup[#, "method", None] === "workspace/semanticTokens/refresh"&
	]

(*
deliverFreshSemanticTokens[uri, reason, wasStale]

Called when fresh tokens are available — or, as in runIndexUpdate, will be
recomputed by the re-fetch this triggers — so the client is brought up to date.
`wasStale` says whether the tokens the client is currently displaying were the
stale, carried-across-the-edit set (callers capture this BEFORE the recompute,
which clears the SemanticTokensStale flag). Delivery is gap-free:
  1. If pending (unanswered, not-yet-queued) fencepost requests exist for uri,
     recover them directly with the fresh tokens (no global churn).
  2. Otherwise, if the displayed tokens were stale, queue ONE coalesced refresh
     so the client re-fetches. Because the refresh handler no longer drops the
     cache, the re-fetch is an instant cache-hit.
*)
deliverFreshSemanticTokens[uri_String, reason_String : "", wasStale_ : False] :=
	Module[{recovered},
		If[!TrueQ[$SemanticTokens], Return[Null]];
		recovered = queuePendingSemanticTokenFenceposts[uri, reason];
		If[recovered == 0 && TrueQ[wasStale],
			queueSemanticTokensRefresh[reason]
		];
		Null
	]

queueSemanticTokensRefresh[reason_String : ""] :=
	If[$SemanticTokens &&
	!TrueQ[$PendingTokenRefresh] &&
	!semanticTokensRefreshQueuedQ[],
		If[reason =!= "", log[0, reason]];
		(* Set the flag immediately so subsequent calls within the same event-loop
		   tick don't enqueue a second refresh.  The flag stays True until the
		   client acknowledges the request (or the 3-second timeout fires). *)
		$PendingTokenRefresh = True;
		$PendingTokenRefreshTime = AbsoluteTime[];
		AppendTo[
			$ContentQueue,
			<|"method" -> "workspace/semanticTokens/refresh"|>
		]
	]

cachedSemanticTokensResponse[uri_String, id_Integer] :=
	Module[{entry, tokens},
		entry = Lookup[$OpenFilesMap, uri, Null];
		tokens = If[AssociationQ[entry], Lookup[entry, "SemanticTokens", Null], Null];
		If[ListQ[tokens],
			forgetPendingSemanticTokenRequest[uri, id];
			{<|"jsonrpc" -> "2.0", "id" -> id, "result" -> <|"data" -> tokens|>|>},
			{}
		]
	]

foregroundSemanticTokensFencepost[uri_String, id_Integer] :=
	Module[{entry, prepared},
		entry = Lookup[$OpenFilesMap, uri, Null];
		If[AssociationQ[entry] && !highlightEntryReadyQ[entry],
			prepared = prepareHighlightWorkerEntry[uri, entry];
			If[AssociationQ[prepared],
				$OpenFilesMap[uri] = prepared
			]
		];
		handleContent[
			<|
				"method"              -> "textDocument/semanticTokens/fullFencepost",
				"id"                  -> id,
				"params"              -> <|"textDocument" -> <|"uri" -> uri|>|>,
				"fromHighlightWorker" -> True
			|>
		]
	]

handleContent[
	content :
		KeyValuePattern[
			"method" -> "textDocument/publishSemanticTokensWorkerResult"
		]
] :=
	Module[{
			result,
			id,
			uri,
			response,
			entry,
			pendingQ,
			fallbackResponse
		},
		result = Lookup[content, "result", $Failed];
		id = Lookup[content, "id", None];
		If[!IntegerQ[id] && AssociationQ[result],
			id = Lookup[result, "ID", None]
		];
		uri = contentURI[content];
		If[AssociationQ[result], uri = Lookup[result, "URI", uri]];
		pendingQ =
			StringQ[uri] &&
			IntegerQ[id] &&
			MemberQ[Lookup[$PendingSemanticTokenRequests, uri, {}], id];
		If[IntegerQ[id] && !TrueQ[pendingQ], Return[{}]];
		If[!AssociationQ[result] || !highlightWorkerResultCurrentQ[result],
			If[StringQ[uri] && IntegerQ[id],
				fallbackResponse = cachedSemanticTokensResponse[uri, id];
				If[fallbackResponse =!= {}, Return[fallbackResponse]];
				Return[foregroundSemanticTokensFencepost[uri, id]]
			];
			Return[{}]
		];
		mergeHighlightWorkerEntry[uri, Lookup[result, "Entry", <||>]];
		entry = Lookup[$OpenFilesMap, uri, <||>];
		If[TrueQ[Lookup[entry, "SemanticTokensIncomplete", False]],
			loadFeatureModule["SemanticTokens"];
			LSPServer`SemanticTokens`Private`queueSemanticTokenScopingFollowup[
				uri
			]
		];
		If[StringQ[uri] && IntegerQ[id],
			forgetPendingSemanticTokenRequest[uri, id]
		];
		response = Lookup[result, "Response", {}];
		If[ListQ[response], response, {}]
	]
handleContent[
	content :
		KeyValuePattern[
			"method" -> "textDocument/publishDocumentColorWorkerResult"
		]
] :=
	Module[{result, id, response},
		result = Lookup[content, "result", $Failed];
		id = Lookup[content, "id", None];
		If[!IntegerQ[id] && AssociationQ[result],
			id = Lookup[result, "ID", None]
		];
		If[!AssociationQ[result] || !highlightWorkerResultCurrentQ[result],
			If[IntegerQ[id],
				Return[{<|"jsonrpc" -> "2.0", "id" -> id, "result" -> Null|>}]
			];
			Return[{}]
		];
		response = Lookup[result, "Response", {}];
		If[ListQ[response], response, {}]
	]
handleContent[
	content :
		KeyValuePattern[
			"method" -> "textDocument/publishScopingDataWorkerResult"
		]
] :=
	Module[{result, uri, wasStale},
		result = Lookup[content, "result", $Failed];
		If[!AssociationQ[result] || !highlightWorkerResultCurrentQ[result],
			Return[{}]
		];
		uri = Lookup[result, "URI", contentURI[content]];
		mergeHighlightWorkerEntry[uri, Lookup[result, "Entry", <||>]];
		wasStale = TrueQ[Lookup[result, "WasStale", False]];
		deliverFreshSemanticTokens[
			uri,
			StringJoin[
				"DBG-ST runScopingData worker: delivering fresh tokens for ",
				uri
			],
			wasStale
		];
		{}
	]
(*
Send workspace/semanticTokens/refresh to tell VS Code to re-fetch tokens for all
open files.

This handler must stay cheap. It should only invalidate cached tokens and queue
pending semantic-token fenceposts; expensive token computation happens later in
the normal semanticTokens/fullFencepost path or when the client re-requests.
Uses a negative server-generated id to avoid colliding with client request ids.
*)
handleContent[
	content : KeyValuePattern["method" -> "workspace/semanticTokens/refresh"]
] :=
	Module[{id, invalidated = 0, recovered = 0},
		Scan[
			Function[
				{uri},
				Module[{
						entry = Lookup[$OpenFilesMap, uri, Null],
						recoveredCount = 0
					},
					If[
						AssociationQ[entry],
						(* Do NOT drop cached tokens. They are already fresh by the time a
						   refresh is emitted, so keeping them makes the client's re-fetch an
						   instant cache-hit with no blank gap. *)
						If[KeyExistsQ[entry, "SemanticTokens"],
							invalidated += 1
						];
						recoveredCount =
							queuePendingSemanticTokenFenceposts[
								uri,
								StringJoin[
									"DBG-ST: refresh queued pending ",
									"semantic-token fenceposts"
								]
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
		log[
			0,
			"DBG-ST: sending workspace/semanticTokens/refresh id=",
			id,
			" invalidated=",
			invalidated,
			" recovered=",
			recovered
		];
		{
			<|
				"jsonrpc" -> "2.0",
				"id"      -> id,
				"method"  -> "workspace/semanticTokens/refresh"
			|>
		}
	]

handleContentAfterShutdown[content : KeyValuePattern["method" -> "exit"]] :=
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
		If[
			KeyExistsQ[content, "id"],
			(*
			has id, so this is a request
			*)
			id = content["id"];
			{
				<|
					"jsonrpc" -> "2.0",
					"id"      -> id,
					"error"   -> <|
						"code"    -> $ErrorCodes["InvalidRequest"],
						"message" -> "Invalid request"
					|>
				|>
			},
			(*
			does not have id, so this is a notification
			just ignore
			*)
			{}
		]
	]

expandContent[
	content : KeyValuePattern["method" -> "textDocument/didOpen"],
	pos_
] :=
	Catch[
		Module[{params, doc, uri, res},
			log[1, "textDocument/didOpen: enter expand"];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[isStale[$PreExpandContentQueue[[pos[[1]] + 1;;]], uri],
				If[$Debug2, log["stale"]];
				Throw[
					{
						<|
							"method" -> "textDocument/didOpenFencepost",
							"params" -> params,
							"stale"  -> True
						|>
					}
				]
			];
			res =
				<|"method" -> #, "params" -> params|>& /@ (
					Join[{"textDocument/didOpenFencepost"}, $didOpenMethods]
				);
			log[1, "textDocument/didOpen: Exit"];
			res
		]
	]

handleContent[
	content : KeyValuePattern["method" -> "textDocument/didOpenFencepost"]
] :=
	Catch[
		Module[{params, doc, uri, text, entry},
			If[$Debug2, log["textDocument/didOpenFencepost: enter"]];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			text = doc["text"];
			entry =
				<|
					"Text"               -> text,
					"LastChange"         -> Now,
					"ScheduledJobs"      -> {},
					"IndexUpdatePending" -> True
				|>;
			(* Pre-process .ipwl files so the parse handlers use annotation-free source *)
			If[StringEndsQ[uri, ".ipwl"],
				entry["PreprocessedText"] =
					LSPServer`TypeWL`PreprocessIPWL[text][[1]]
			];
			$OpenFilesMap[uri] = entry;
			appendContentsToContentQueue[
				{
					<|
						"method" -> "textDocument/runOpenIndexUpdate",
						"params" -> <|"textDocument" -> <|"uri" -> uri|>|>
					|>
				}
			];
			log[1, "textDocument/didOpenFencepost: Exit"];
			{}
		]
	]
handleContent[
	content : KeyValuePattern["method" -> "textDocument/runOpenIndexUpdate"]
] :=
	Catch[
		Module[{
				params,
				doc,
				uri,
				entry,
				text,
				parseResult,
				curEntry
			},
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			entry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
			If[!AssociationQ[entry], Throw[{}]];
			text = Lookup[entry, "Text", Missing["NotAvailable"]];
			If[!StringQ[text], Throw[{}]];
			If[NumberQ[$ClosedFileDiagnosticsMaxTextLength] &&
			StringLength[text] > $ClosedFileDiagnosticsMaxTextLength,
				$OpenFilesMap[uri] = KeyDrop[entry, "IndexUpdatePending"];
				Throw[{}]
			];
			If[isStale[$ContentQueue, uri], Throw[{}]];
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
				If[AssociationQ[curEntry] &&
				Lookup[curEntry, "LastChange", Missing["NotAvailable"]] ===
				entry["LastChange"],
					curEntry = KeyDrop[curEntry, "IndexUpdatePending"];
					curEntry["CST"] = parseResult[[1]];
					If[!StringContainsQ[text, "\t"],
						curEntry["CSTTabs"] = parseResult[[1]]
					];
					curEntry["Agg"] = parseResult[[2]];
					curEntry["AST"] = parseResult[[3]];
					With[{
							syms = findAllUserSymbols[parseResult[[3]]]
						},
						curEntry["UserSymbols"] = syms;
						curEntry["PreviousUserSymbols"] = syms
					];
					$OpenFilesMap[uri] = curEntry
				];
				If[AssociationQ[
					Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]]
				] &&
				Lookup[
					$OpenFilesMap[uri],
					"LastChange",
					Missing["NotAvailable"]
				] ===
				entry["LastChange"],
					If[$SemanticTokens,
						queuePendingSemanticTokenFenceposts[
							uri,
							StringJoin[
								"DBG-ST: didOpen indexed; queuing pending ",
								"semantic-token fenceposts"
							]
						]
					];
					appendContentsToContentQueue[
						{
							<|
								"method" -> "textDocument/runFastDiagnostics",
								"params" -> <|
									"textDocument" -> <|"uri" -> uri|>
								|>
							|>
						}
					]
				],
				curEntry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
				If[
					AssociationQ[curEntry] &&
					Lookup[curEntry, "LastChange", Missing["NotAvailable"]] ===
					entry["LastChange"],
					$OpenFilesMap[uri] = KeyDrop[curEntry, "IndexUpdatePending"]
				]
			];
			{}
		]
	]
handleContent[
	content : KeyValuePattern["method" -> "textDocument/concreteParse"]
] :=
	Catch[
		Module[{
				params,
				doc,
				uri,
				cst,
				text,
				entry,
				fileFormat
			},
			log[1, "textDocument/concreteParse: Enter"];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[isStale[$ContentQueue, uri],
				If[$Debug2, log["stale"]];
				Throw[{}]
			];
			entry = Lookup[$OpenFilesMap, uri, Null];
			If[entry === Null,
				Throw[
					Failure[
						"URINotFound",
						<|
							"URI"              -> uri,
							"OpenFilesMapKeys" -> Keys[$OpenFilesMap]
						|>
					]
				]
			];
			cst = Lookup[entry, "CST", Null];
			If[cst =!= Null, Throw[{}]];
			text = Lookup[entry, "PreprocessedText", entry["Text"]];
			If[$Debug2,
				log[
					"text: ",
					stringLineTake[
						StringTake[ToString[text, InputForm], UpTo[1000]],
						UpTo[20]
					]
				];
				log["...\n"]
			];
			If[$Debug2, log["before CodeConcreteParse"]];
			fileFormat = LSPServer`SourceFileFormat[uri];
			cst = CodeConcreteParse[text, "FileFormat" -> fileFormat];
			log[2, "after CodeConcreteParse"];
			If[
				FailureQ[cst],
				(*
				It is possible that a file is open in an editor, the actual file system contents get deleted,
				but the editor still has a stale window open.
				Focusing on that window could trigger a textDocument/didOpen notification, but the file does not exist!
				TODO: is this a bug in Sublime / LSP package?
				*)
				If[MatchQ[cst, Failure["FindFileFailed", _]], Throw[{}]];
				Throw[cst]
			];
			cst[[1]] = File;
			entry["CST"] = cst;
			(*
			save time if the file has no tabs
			*)
			If[!StringContainsQ[text, "\t"], entry["CSTTabs"] = cst];
			$OpenFilesMap[uri] = entry;
			log[1, "textDocument/concreteParse: Exit"];
			{}
		]
	]
handleContent[
	content : KeyValuePattern["method" -> "textDocument/concreteTabsParse"]
] :=
	Catch[
		Module[{
				params,
				doc,
				uri,
				text,
				entry,
				cstTabs,
				fileFormat
			},
			log[1, "textDocument/concreteTabsParse: enter"];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[isStale[$ContentQueue, uri],
				If[$Debug2, log["stale"]];
				Throw[{}]
			];
			entry = Lookup[$OpenFilesMap, uri, Null];
			If[entry === Null,
				Throw[
					Failure[
						"URINotFound",
						<|
							"URI"              -> uri,
							"OpenFilesMapKeys" -> Keys[$OpenFilesMap]
						|>
					]
				]
			];
			cstTabs = Lookup[entry, "CSTTabs", Null];
			If[cstTabs =!= Null, Throw[{}]];
			text = Lookup[entry, "PreprocessedText", entry["Text"]];
			(*
			Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
			FIXME: Must use the tab width from the editor
			*)
			log[2, "before CodeConcreteParse (TabWidth 4)"];
			fileFormat = LSPServer`SourceFileFormat[uri];
			cstTabs =
				CodeConcreteParse[
					text,
					"TabWidth"   -> 4,
					"FileFormat" -> fileFormat
				];
			log[2, "after CodeConcreteParse (TabWidth 4)"];
			If[
				FailureQ[cstTabs],
				(*
				It is possible that a file is open in an editor, the actual file system contents get deleted,
				but the editor still has a stale window open.
				Focusing on that window could trigger a textDocument/didOpen notification, but the file does not exist!
				TODO: is this a bug in Sublime / LSP package?
				*)
				If[MatchQ[cstTabs, Failure["FindFileFailed", _]], Throw[{}]];
				Throw[cstTabs]
			];
			cstTabs[[1]] = File;
			entry["CSTTabs"] = cstTabs;
			$OpenFilesMap[uri] = entry;
			log[1, "textDocument/concreteTabsParse: exit"];
			{}
		]
	]
handleContent[
	content : KeyValuePattern["method" -> "textDocument/aggregateParse"]
] :=
	Catch[
		Module[{
				params,
				doc,
				uri,
				cst,
				text,
				entry,
				agg
			},
			log[1, "textDocument/aggregateParse: Enter"];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[isStale[$ContentQueue, uri],
				If[$Debug2, log["stale"]];
				Throw[{}]
			];
			entry = Lookup[$OpenFilesMap, uri, Null];
			If[entry === Null,
				Throw[
					Failure[
						"URINotFound",
						<|
							"URI"              -> uri,
							"OpenFilesMapKeys" -> Keys[$OpenFilesMap]
						|>
					]
				]
			];
			text = entry["Text"];
			agg = Lookup[entry, "Agg", Null];
			If[agg =!= Null, Throw[{}]];
			cst = Lookup[entry, "CST", Null];
			If[cst === Null || MissingQ[cst], Throw[{}]];
			If[$Debug2, log["before Aggregate"]];
			agg = CodeParser`Abstract`Aggregate[cst];
			log[2, "after Aggregate"];
			entry["Agg"] = agg;
			(*
			save time if the file has no tabs
			*)
			If[!StringContainsQ[text, "\t"], entry["AggTabs"] = agg];
			$OpenFilesMap[uri] = entry;
			log[1, "textDocument/aggregateParse: Exit"];
			{}
		]
	]
handleContent[
	content : KeyValuePattern["method" -> "textDocument/aggregateTabsParse"]
] :=
	Catch[
		Module[{
				params,
				doc,
				uri,
				entry,
				cstTabs,
				aggTabs
			},
			log[1, "textDocument/aggregateTabsParse: enter"];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[isStale[$ContentQueue, uri],
				If[$Debug2, log["stale"]];
				Throw[{}]
			];
			entry = Lookup[$OpenFilesMap, uri, Null];
			If[entry === Null,
				Throw[
					Failure[
						"URINotFound",
						<|
							"URI"              -> uri,
							"OpenFilesMapKeys" -> Keys[$OpenFilesMap]
						|>
					]
				]
			];
			aggTabs = Lookup[entry, "AggTabs", Null];
			If[aggTabs =!= Null, Throw[{}]];
			cstTabs = entry["CSTTabs"];
			(*
			Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
			FIXME: Must use the tab width from the editor
			*)
			log[2, "before Aggregate"];
			aggTabs = CodeParser`Abstract`Aggregate[cstTabs];
			log[2, "after Aggregate"];
			If[FailureQ[aggTabs], Throw[aggTabs]];
			entry["AggTabs"] = aggTabs;
			$OpenFilesMap[uri] = entry;
			log[1, "textDocument/aggregateTabsParse: exit"];
			{}
		]
	]
handleContent[
	content : KeyValuePattern["method" -> "textDocument/abstractParse"]
] :=
	Catch[
		Module[{
				params,
				doc,
				uri,
				entry,
				agg,
				ast,
				userSymbols
			},
			log[1, "textDocument/abstractParse: enter"];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[isStale[$ContentQueue, uri],
				If[$Debug2, log["stale"]];
				Throw[{}]
			];
			entry = Lookup[$OpenFilesMap, uri, Null];
			If[entry === Null,
				Throw[
					Failure[
						"URINotFound",
						<|
							"URI"              -> uri,
							"OpenFilesMapKeys" -> Keys[$OpenFilesMap]
						|>
					]
				]
			];
			ast = Lookup[entry, "AST", Null];
			If[ast =!= Null, Throw[{}]];
			agg = Lookup[entry, "Agg", Null];
			If[agg === Null || MissingQ[agg] || FailureQ[agg], Throw[{}]];
			If[$Debug2, log["before Abstract"]];
			ast = CodeParser`Abstract`Abstract[agg];
			userSymbols = findAllUserSymbols[ast];
			log[2, "after Abstract"];
			entry["AST"] = ast;
			entry["UserSymbols"] = userSymbols;
			entry["PreviousUserSymbols"] = userSymbols;
			$OpenFilesMap[uri] = entry;
			log[1, "textDocument/abstractParse: exit"];
			{}
		]
	]

findAllUserSymbols[ast_] :=
	DeleteDuplicates[
		Cases[
			ast,
			{
				CallNode[
					LeafNode[Symbol, "SetDelayed" | "Set", <||>],
					{CallNode[LeafNode[Symbol, sym_, _], _, _], rhs : _} |
					{LeafNode[Symbol, sym_, _], rhs : _},
					_
				],
				_
			} :> sym,
			8
		] (* Same depth used in finding function call pattern in Hover feature *)
	]

expandContent[
	content : KeyValuePattern["method" -> "textDocument/didClose"],
	pos_
] :=
	Catch[
		Module[{params, doc, uri, res},
			log[1, "textDocument/didClose: enter expand"];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[isStale[$PreExpandContentQueue[[pos[[1]] + 1;;]], uri],
				If[$Debug2, log["stale"]];
				Throw[
					{
						<|
							"method" -> "textDocument/didCloseFencepost",
							"params" -> params,
							"stale"  -> True
						|>
					}
				]
			];
			res =
				<|"method" -> #, "params" -> params|>& /@ (
					Join[{"textDocument/didCloseFencepost"}, $didCloseMethods]
				);
			log[1, "textDocument/didClose: exit"];
			res
		]
	]

handleContent[
	content : KeyValuePattern["method" -> "textDocument/didCloseFencepost"]
] :=
	Module[{
			params,
			doc,
			uri,
			beforeQueueLen,
			dropped,
			entry,
			notification
		},
		log[1, "textDocument/didCloseFencepost: Enter"];
		params = content["params"];
		doc = params["textDocument"];
		uri = doc["uri"];
		entry = Lookup[$OpenFilesMap, uri, Null];
		If[AssociationQ[entry],
			loadFeatureModule["Diagnostics"];
			notification =
				LSPServer`Diagnostics`Private`buildPublishNotification[
					uri,
					entry,
					LSPServer`Diagnostics`Private`allEntryDiagnosticsLints[
						entry
					]
				];
			If[!AssociationQ[$ClosedFileDiagnosticsNotifications],
				$ClosedFileDiagnosticsNotifications = <||>
			];
			$ClosedFileDiagnosticsNotifications[uri] = notification
		];
		$OpenFilesMap[uri] =.;
		beforeQueueLen = Length[$ContentQueue];
		$ContentQueue =
			Select[
				$ContentQueue,
				!(
					Lookup[
						Lookup[Lookup[#, "params", <||>], "textDocument", <||>],
						"uri",
						None
					] ===
					uri &&
					!MemberQ[$didCloseMethods, Lookup[#, "method", None]]
				)&
			];
		dropped = beforeQueueLen - Length[$ContentQueue];
		If[AssociationQ[$PendingSemanticTokenRequests],
			$PendingSemanticTokenRequests =
				KeyDrop[$PendingSemanticTokenRequests, uri]
		];
		If[dropped > 0,
			log[
				0,
				"DBG-ST: didClose purged queued uri work dropped=",
				dropped,
				" uri=",
				uri
			]
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

expandContent[
	content : KeyValuePattern["method" -> "textDocument/didSave"],
	pos_
] :=
	Catch[
		Module[{params, doc, uri},
			log[1, "textDocument/didSave: Enter"];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[isStale[$PreExpandContentQueue[[pos[[1]] + 1;;]], uri],
				If[$Debug2, log["stale"]];
				Throw[
					{
						<|
							"method" -> "textDocument/didSaveFencepost",
							"params" -> params,
							"stale"  -> True
						|>
					}
				]
			];
			res =
				<|"method" -> #, "params" -> params|>& /@ (
					Join[{"textDocument/didSaveFencepost"}, $didSaveMethods]
				);
			log[1, "textDocument/didSave: Exit"];
			res
		]
	]

handleContent[
	content : KeyValuePattern["method" -> "textDocument/didSaveFencepost"]
] :=
	Module[{},
		log[1, "textDocument/didSaveFencepost: Enter"];
		log[1, "textDocument/didSaveFencepost: Exit"];
		{}
	]

expandContent[
	content : KeyValuePattern["method" -> "textDocument/didChange"],
	pos_
] :=
	Catch[
		Module[{params, doc, uri, res},
			log[1, "textDocument/didChange: enter expand"];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[isStale[$PreExpandContentQueue[[pos[[1]] + 1;;]], uri],
				If[$Debug2, log["stale"]];
				Throw[
					{
						<|
							"method" -> "textDocument/didChangeFencepost",
							"params" -> params,
							"stale"  -> True
						|>
					}
				]
			];
			res =
				Join[
					{
						<|
							"method" -> "textDocument/didChangeFencepost",
							"params" -> params
						|>
					},
					Map[
						Function[
							{method},
							If[
								MemberQ[
									{
										"textDocument/runDiagnostics",
										"textDocument/publishDiagnostics"
									},
									method
								],
								<|
									"method"   -> method,
									"params"   -> params,
									"priority" -> True
								|>,
								<|"method" -> method, "params" -> params|>
							]
						],
						$didChangeMethods
					]
				];
			log[1, "textDocument/didChange: Exit"];
			res
		]
	]

handleContent[
	content : KeyValuePattern["method" -> "textDocument/didChangeFencepost"]
] :=
	Catch[
		Module[{
				params,
				doc,
				uri,
				text,
				lastChange,
				entry,
				changes,
				oldEntry,
				previousUserSymbols,
				changedSymbols,
				affectedOpenURIs
			},
			If[$Debug2, log["textDocument/didChangeFencepost: enter"]];
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],
				If[$Debug2, log["stale"]];
				Throw[{}]
			];
			cancelCurrentDiagnosticsTask[];
			changes = params["contentChanges"];
			(*
			Currently only supporting full text, so always only apply the last change
			*)
			lastChange = changes[[-1]];
			text = lastChange["text"];
			oldEntry = Lookup[$OpenFilesMap, uri, <||>];
			previousUserSymbols =
				Replace[
					Lookup[
						oldEntry,
						"PreviousUserSymbols",
						Lookup[oldEntry, "UserSymbols", {}]
					],
					Except[_List] -> {}
				];
			(*
			    We do not keep entry["AST"] here: the text changed, so the AST
			    must be re-evaluated by the parse pipeline.
			*)
			entry =
				<|
					"Text"                -> text,
					"LastChange"          -> Now,
					"ScheduledJobs"       -> $didChangeScheduledJobs,
					"IndexUpdatePending"  -> True,
					"PreviousUserSymbols" -> Lookup[
						oldEntry,
						"PreviousUserSymbols",
						Lookup[oldEntry, "UserSymbols", Missing["NotAvailable"]]
					]
				|>;
			(* Never-blank: carry the last-good semantic tokens across the edit and mark
			   them stale. The serve path will display these (rather than nothing) until
			   fresh tokens are computed, so coloring never goes monochrome on a keystroke. *)
			With[{
					oldTokens = Lookup[oldEntry, "SemanticTokens", Null]
				},
				If[oldTokens =!= Null,
					entry["SemanticTokens"] = oldTokens;
					entry["SemanticTokensStale"] = True
				]
			];
			(* Pre-process .ipwl files so the parse handlers use annotation-free source *)
			If[StringEndsQ[uri, ".ipwl"],
				entry["PreprocessedText"] =
					LSPServer`TypeWL`PreprocessIPWL[text][[1]]
			];
			$OpenFilesMap[uri] = entry;
			(*
			Schedule paclet index update (debounced with other scheduled jobs).
			The job queues a "textDocument/runIndexUpdate" content message instead of
			running UpdateFileIndex inline, so the main loop can interleave interactive
			requests (hover, completion) between the fencepost and the index update.
			*)
			AppendTo[
				entry["ScheduledJobs"],
				Function[
					{e},
					If[
						Now - e["LastChange"] >
						Quantity[$DiagnosticsDelayAfterLastChange, "Seconds"],
						{{"textDocument/runIndexUpdate"}, True},
						{{}, False}
					]
				]
			];
			$OpenFilesMap[uri] = entry;
			log[1, "textDocument/didChangeFencepost: Exit"];
			{}
		]
	]
(*
handleContent for the deferred index update. This runs as a normal queue item
so interactive requests (hover, completion) can be promoted ahead of it by
takeFirstContentQueueItem.
*)
handleContent[
	content : KeyValuePattern["method" -> "textDocument/runIndexUpdate"]
] :=
	Catch[
		Module[{
				params,
				doc,
				uri,
				entry,
				text,
				parseResult,
				curEntry,
				previousUserSymbols,
				changedSymbols,
				affectedOpenURIs
			},
			params = content["params"];
			doc = params["textDocument"];
			uri = doc["uri"];
			entry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
			If[!AssociationQ[entry], Throw[{}]];
			text = Lookup[entry, "Text", Missing["NotAvailable"]];
			If[!StringQ[text], Throw[{}]];
			If[NumberQ[$ClosedFileDiagnosticsMaxTextLength] &&
			StringLength[text] > $ClosedFileDiagnosticsMaxTextLength,
				$OpenFilesMap[uri] = KeyDrop[entry, "IndexUpdatePending"];
				Throw[{}]
			];
			previousUserSymbols =
				Replace[
					Lookup[
						entry,
						"PreviousUserSymbols",
						Lookup[entry, "UserSymbols", {}]
					],
					Except[_List] -> {}
				];
			(* If there is a newer didChange for this URI queued, skip — it will trigger
			   its own index update. *)
			If[isStale[$ContentQueue, uri], Throw[{}]];
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
				If[AssociationQ[curEntry] &&
				Lookup[curEntry, "LastChange", Missing["NotAvailable"]] ===
				entry["LastChange"],
					curEntry = KeyDrop[curEntry, "IndexUpdatePending"];
					curEntry["CST"] = parseResult[[1]];
					If[!StringContainsQ[text, "\t"],
						curEntry["CSTTabs"] = parseResult[[1]]
					];
					curEntry["Agg"] = parseResult[[2]];
					curEntry["AST"] = parseResult[[3]];
					With[{
							syms = findAllUserSymbols[parseResult[[3]]]
						},
						curEntry["UserSymbols"] = syms;
						curEntry["PreviousUserSymbols"] = syms
					];
					$OpenFilesMap[uri] = curEntry,
					log[
						0,
						StringJoin[
							"DBG-ST: didChange index result stale; skipping ",
							"refresh for "
						],
						uri
					]
				];
				If[AssociationQ[
					Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]]
				] &&
				Lookup[
					$OpenFilesMap[uri],
					"LastChange",
					Missing["NotAvailable"]
				] ===
				entry["LastChange"],
					changedSymbols =
						DeleteDuplicates[
							Join[
								previousUserSymbols,
								Replace[
									Lookup[
										$OpenFilesMap[uri],
										"UserSymbols",
										{}
									],
									Except[_List] -> {}
								]
							]
						];
					affectedOpenURIs =
						DeleteDuplicates[
							Join[
								{uri},
								openFilesAffectedByDefinitions[changedSymbols]
							]
						];
					invalidateWorkspaceDiagnostics[affectedOpenURIs];
					(*
					Dispatch the slow tier only for OTHER affected open files.
					For the edited file itself the fast tier (queued below)
					dispatches workspace diagnostics when it completes;
					dispatching here as well ran the slow tier twice per edit.
					*)
					With[{otherAffected = DeleteCases[affectedOpenURIs, uri]},
						If[otherAffected =!= {},
							loadFeatureModule["Diagnostics"];
							Scan[
								LSPServer`Diagnostics`Private`dispatchWorkspaceDiagnostics,
								otherAffected
							]
						]
					];
					If[$SemanticTokens,
						Module[{wasStaleBefore},
							wasStaleBefore =
								TrueQ[
									Lookup[
										Lookup[$OpenFilesMap, uri, <||>],
										"SemanticTokensStale",
										False
									]
								];
							deliverFreshSemanticTokens[
								uri,
								StringJoin[
									"DBG-ST: didChange indexed; delivering ",
									"fresh semantic tokens"
								],
								wasStaleBefore
							]
						]
					];
					appendContentsToContentQueue[
						{
							<|
								"method" -> "textDocument/runFastDiagnostics",
								"params" -> <|
									"textDocument" -> <|"uri" -> uri|>
								|>
							|>
						}
					]
				],
				curEntry = Lookup[$OpenFilesMap, uri, Missing["NotAvailable"]];
				If[
					AssociationQ[curEntry] &&
					Lookup[curEntry, "LastChange", Missing["NotAvailable"]] ===
					entry["LastChange"],
					$OpenFilesMap[uri] = KeyDrop[curEntry, "IndexUpdatePending"]
				]
			];
			{}
		]
	]

exitGracefully[] :=
	(
		log[0, "\n\n"];
		log[0, "KERNEL IS EXITING GRACEFULLY"];
		log[0, "\n\n"];
		shutdownLSPComm[$commProcess, $initializedComm];
		(
			(* :!CodeAnalysis::BeginBlock:: *)
			(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
			Exit[0]
			(* :!CodeAnalysis::EndBlock:: *)
		)
	)

exitSemiGracefully[] :=
	(
		log[0, "Language Server kernel did not shutdown properly."];
		log[0, ""];
		log[0, "This is the command that was used:"];
		log[0, $CommandLine];
		log[0, ""];
		log[
			0,
			StringJoin[
				"To help diagnose the problem, run this in a ",
				"notebook:\nNeeds[\"LSPServer`\"]\nLSPServer`RunServerDiagnostic[{",
				Riffle[
					(StringJoin[ "\"", #, "\""])& /@ StringReplace[
						$CommandLine,
						"\"" -> "\\\""
					],
					", "
				],
				"}]"
			]
		];
		log[0, ""];
		log[0, "Fix any problems then restart and try again."];
		log[0, "\n\n"];
		log[0, "KERNEL IS EXITING SEMI-GRACEFULLY"];
		log[0, "\n\n"];
		shutdownLSPComm[$commProcess, $initializedComm];
		(
			(* :!CodeAnalysis::BeginBlock:: *)
			(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
			Exit[1]
			(* :!CodeAnalysis::EndBlock:: *)
		)
	)

exitHard[] :=
	(
		log[0, "Language Server kernel did not shutdown properly."];
		log[0, ""];
		log[0, "This is the command that was used:"];
		log[0, $CommandLine];
		log[0, ""];
		log[
			0,
			StringJoin[
				"To help diagnose the problem, run this in a ",
				"notebook:\nNeeds[\"LSPServer`\"]\nLSPServer`RunServerDiagnostic[{",
				Riffle[
					(StringJoin[ "\"", #, "\""])& /@ StringReplace[
						$CommandLine,
						"\"" -> "\\\""
					],
					", "
				],
				"}]"
			]
		];
		log[0, ""];
		log[0, "Fix any problems then restart and try again."];
		log[0, "\n\n"];
		log[0, "KERNEL IS EXITING HARD"];
		log[0, "\n\n"];
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
		startupMessagesText =
			If[StringQ[name] && FileExistsQ[name],
				Replace[
					Quiet[Check[Import[name, "Text"], ""]],
					Except[_String] -> ""
				],
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
