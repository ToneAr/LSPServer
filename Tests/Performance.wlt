(* Load LSPServer from the repository's build/paclet so in-tree changes are picked up. *)
PacletDirectoryLoad[AbsoluteFileName[
  FileNameJoin[{DirectoryName[$TestFileName], "..", "build", "paclet"}]]];
<<LSPServer`
LSPServer`LoadAllFeatureModules[];
Needs["CodeParser`"];

(* OPT-IN perf gate: set LSP_PERF=1 to run. Asserts UpdateFileIndex on a large
   file (with parse artifacts already cached, as in the real didChange flow)
   stays under 1200 ms. Pre-Stage-2 baseline: ~2000 ms; post: ~450 ms. *)
VerificationTest[
  If[Environment["LSP_PERF"] === "1",
    Module[{text, uri, cst, agg, ast, t},
      text = Import[FileNameJoin[{DirectoryName[$TestFileName], "..",
        "LSPServer", "Kernel", "Hover.wl"}], "Text"];
      uri = "file:///perf.wl";
      cst = CodeParser`CodeConcreteParse[text, "FileFormat" -> "Package"];
      cst[[1]] = File;
      agg = CodeParser`Abstract`Aggregate[cst];
      ast = CodeParser`Abstract`Abstract[agg];
      LSPServer`$OpenFilesMap = <|uri -> <|"Text" -> text, "CST" -> cst,
        "Agg" -> agg, "AST" -> ast, "LastChange" -> Now|>|>;
      (* warm caches once, then measure *)
      LSPServer`PacletIndex`UpdateFileIndex[uri, text];
      t = First[AbsoluteTiming[LSPServer`PacletIndex`UpdateFileIndex[uri, text]]];
      Print["UpdateFileIndex warm: ", Round[1000 t], " ms"];
      t < 1.2
    ],
    True
  ],
  True,
  TestID -> "perf-update-file-index-under-budget"
]
