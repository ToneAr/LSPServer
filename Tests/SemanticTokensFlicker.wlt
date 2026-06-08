(* Load LSPServer from the repository's build/paclet so in-tree changes are picked up. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "build", "paclet"}]]];
<<LSPServer`
LSPServer`LoadAllFeatureModules[];
Needs["CodeParser`"];

(* Semantic-token flicker regression tests. Each test sets up global server
   state (LSPServer`$OpenFilesMap, LSPServer`$ContentQueue, etc.), calls a
   handler directly, and inspects the result or resulting state. *)

VerificationTest[
  True,
  True,
  TestID -> "SemanticTokensFlicker-loader-smoke"
]

(* A fresh compute via computeAndCacheSemanticTokens must clear any stale flag. *)
VerificationTest[
  Module[{uri = "file:///clearflag.wl", cst, agg, ast, entry},
    cst = CodeParser`CodeConcreteParse["f[x_] := x\n", "FileFormat" -> "Package"];
    cst[[1]] = File;
    agg = CodeParser`Abstract`Aggregate[cst];
    ast = CodeParser`Abstract`Abstract[agg];
    LSPServer`$OpenFilesMap = <|
      uri -> <|
        "Text" -> "f[x_] := x\n",
        "CST" -> cst,
        "AST" -> ast,
        "SemanticTokens" -> {0, 0, 1, 2, 0},
        "SemanticTokensStale" -> True
      |>
    |>;
    LSPServer`SemanticTokens`computeAndCacheSemanticTokens[uri];
    entry = LSPServer`$OpenFilesMap[uri];
    TrueQ[Lookup[entry, "SemanticTokensStale", False]]
  ],
  False,
  TestID -> "computeAndCache-clears-stale-flag"
]

(* An edit must NOT discard the cached tokens; it preserves them and marks them stale. *)
VerificationTest[
  Module[{uri = "file:///edit.wl", entry},
    LSPServer`$ContentQueue = {};
    LSPServer`$OpenFilesMap = <|
      uri -> <|
        "Text" -> "f[x_] := x\n",
        "SemanticTokens" -> {0, 0, 3, 2, 0},
        "AST" -> Null
      |>
    |>;
    LSPServer`handleContent[<|
      "method" -> "textDocument/didChangeFencepost",
      "params" -> <|
        "textDocument" -> <|"uri" -> uri|>,
        "contentChanges" -> {<|"text" -> "g[y_] := y\n"|>}
      |>
    |>];
    entry = LSPServer`$OpenFilesMap[uri];
    {Lookup[entry, "SemanticTokens", Missing["x"]], TrueQ[Lookup[entry, "SemanticTokensStale", False]]}
  ],
  {{0, 0, 3, 2, 0}, True},
  TestID -> "didChangeFencepost-preserves-stale-tokens"
]

(* When the entry is awaiting reindex but stale tokens exist, the fencepost
   serves the stale tokens (non-empty) rather than blanking. *)
VerificationTest[
  Module[{uri = "file:///reindex.wl"},
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|
        "SemanticTokens" -> {0, 0, 1, 2, 0},
        "SemanticTokensStale" -> True,
        "IndexUpdatePending" -> True
      |>
    |>;
    LSPServer`handleContent[<|
      "method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> 42,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>]
  ],
  {<|"jsonrpc" -> "2.0", "id" -> 42, "result" -> <|"data" -> {0, 0, 1, 2, 0}|>|>},
  TestID -> "fullFencepost-serves-stale-when-reindex-pending"
]

(* A present-and-fresh cache is still served directly as a cache-hit. *)
VerificationTest[
  Module[{uri = "file:///freshhit.wl"},
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|"SemanticTokens" -> {0, 0, 2, 2, 0}|>
    |>;
    LSPServer`handleContent[<|
      "method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> 7,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>]
  ],
  {<|"jsonrpc" -> "2.0", "id" -> 7, "result" -> <|"data" -> {0, 0, 2, 2, 0}|>|>},
  TestID -> "fullFencepost-fresh-cache-hit-still-served"
]

(* GATE regression: a stale cache with CST/AST present (recompute possible, not
   awaiting reindex) must recompute fresh tokens, which clears the stale flag.
   With the old unconditional cache-hit (no !staleQ gate), the stale cache would
   be served and the flag would remain True. *)
VerificationTest[
  Module[{uri = "file:///gatefresh.wl", cst, agg, ast, entry},
    cst = CodeParser`CodeConcreteParse["x\n", "FileFormat" -> "Package"];
    cst[[1]] = File;
    agg = CodeParser`Abstract`Aggregate[cst];
    ast = CodeParser`Abstract`Abstract[agg];
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|
        "Text" -> "x\n",
        "CST" -> cst,
        "AST" -> ast,
        "ScopingData" -> {},
        "SemanticTokens" -> {9, 9, 9, 9, 9},
        "SemanticTokensStale" -> True
      |>
    |>;
    LSPServer`handleContent[<|
      "method" -> "textDocument/semanticTokens/fullFencepost",
      "id" -> 5,
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>];
    entry = LSPServer`$OpenFilesMap[uri];
    TrueQ[Lookup[entry, "SemanticTokensStale", False]]
  ],
  False,
  TestID -> "fullFencepost-stale-cache-recomputes-fresh"
]

(* The refresh handler must NOT drop cached tokens. Tokens are fresh in cache by
   the time a refresh is emitted; keeping them makes the re-fetch an instant
   cache-hit with no blank gap. *)
VerificationTest[
  Module[{uri = "file:///refresh.wl"},
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$InternalRequestId = -100;
    LSPServer`$OpenFilesMap = <|
      uri -> <|"SemanticTokens" -> {1, 2, 3, 4, 5}|>
    |>;
    LSPServer`handleContent[<|"method" -> "workspace/semanticTokens/refresh"|>];
    Lookup[LSPServer`$OpenFilesMap[uri], "SemanticTokens", Missing["x"]]
  ],
  {1, 2, 3, 4, 5},
  TestID -> "refresh-handler-keeps-cache"
]

(* After runScopingData processes an entry whose tokens were stale, the cache
   must remain populated (not dropped). *)
VerificationTest[
  Module[{uri = "file:///scoping.wl", cst, agg, ast, entry},
    cst = CodeParser`CodeConcreteParse["f[x_] := Module[{a}, a]\n", "FileFormat" -> "Package"];
    cst[[1]] = File;
    agg = CodeParser`Abstract`Aggregate[cst];
    ast = CodeParser`Abstract`Abstract[agg];
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PreExpandContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|
        "Text" -> "f[x_] := Module[{a}, a]\n",
        "CST" -> cst,
        "AST" -> ast,
        "SemanticTokens" -> {0, 0, 1, 2, 0},
        "SemanticTokensStale" -> True,
        "SemanticTokensIncomplete" -> True
      |>
    |>;
    LSPServer`handleContent[<|
      "method" -> "textDocument/runScopingData",
      "params" -> <|"textDocument" -> <|"uri" -> uri|>|>
    |>];
    entry = LSPServer`$OpenFilesMap[uri];
    KeyExistsQ[entry, "SemanticTokens"]
  ],
  True,
  TestID -> "runScopingData-keeps-cache-populated"
]

(* deliverFreshSemanticTokens: no pending request + wasStale=True =>
   exactly one coalesced workspace/semanticTokens/refresh is queued. *)
VerificationTest[
  Module[{uri = "file:///deliver1.wl", methods},
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$PendingTokenRefresh = False;
    LSPServer`$OpenFilesMap = <|
      uri -> <|"SemanticTokens" -> {0, 0, 1, 2, 0}|>
    |>;
    LSPServer`Private`deliverFreshSemanticTokens[uri, "", True];
    methods = Lookup[#, "method", None]& /@ LSPServer`$ContentQueue;
    Count[methods, "workspace/semanticTokens/refresh"]
  ],
  1,
  TestID -> "deliverFresh-queues-one-refresh-when-no-pending"
]

(* deliverFreshSemanticTokens: a pending (unanswered, not-yet-queued) request is
   recovered directly, so NO refresh is queued even when wasStale=True. *)
VerificationTest[
  Module[{uri = "file:///deliver2.wl", methods},
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingTokenRefresh = False;
    LSPServer`$PendingSemanticTokenRequests = <|uri -> {99}|>;
    LSPServer`$OpenFilesMap = <|
      uri -> <|"SemanticTokens" -> {0, 0, 1, 2, 0}|>
    |>;
    LSPServer`Private`deliverFreshSemanticTokens[uri, "", True];
    methods = Lookup[#, "method", None]& /@ LSPServer`$ContentQueue;
    {Count[methods, "workspace/semanticTokens/refresh"],
     Count[methods, "textDocument/semanticTokens/fullFencepost"]}
  ],
  {0, 1},
  TestID -> "deliverFresh-recovers-pending-without-refresh"
]

(* deliverFreshSemanticTokens: wasStale=False and no pending => nothing queued
   (tokens already current; no client churn). *)
VerificationTest[
  Module[{uri = "file:///deliver3.wl"},
    LSPServer`$SemanticTokens = True;
    LSPServer`$ContentQueue = {};
    LSPServer`$PendingSemanticTokenRequests = <||>;
    LSPServer`$PendingTokenRefresh = False;
    LSPServer`$OpenFilesMap = <|uri -> <|"SemanticTokens" -> {0, 0, 1, 2, 0}|>|>;
    LSPServer`Private`deliverFreshSemanticTokens[uri, "", False];
    Length[LSPServer`$ContentQueue]
  ],
  0,
  TestID -> "deliverFresh-no-op-when-not-stale-and-no-pending"
]
