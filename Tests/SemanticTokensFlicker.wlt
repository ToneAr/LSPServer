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
