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
