(*
GenerateBuiltinPatterns.wl

Generates LSPServer/Resources/BuiltinInputPatterns.wl for every System` symbol.

Four complementary inference sources are used, in priority order:
  1. DownValues of each symbol - exact WL input patterns from actual definitions.
     ~3400 System` symbols have non-empty DownValues.
  2. RHS analysis of DownValue rules - $knownFunctionReturnTypes map is used to
     infer the return type when the RHS is a direct call to a well-known function
     (e.g. StringJoin, Table, Length).
  3. WolframLanguageData["sym", "PlaintextUsage"] - prose description parsed with
     verb/phrase regexes for return type inference; also used as fallback for
     symbols without useful DownValues.
  4. Post-processing passes:
       a. Cross-overload propagation: when all typed overloads of a symbol agree
          on one return type, that type is filled into the remaining None overloads.
       b. Name-based fallback: symbol-naming conventions ( *Q, String*, *List,
          *Count, *Plot, *Chart) applied as a last resort.

Run from the project root with:
  wolframscript -file CodeTools/Generate/GenerateBuiltinPatterns.wl
*)

(* ================================================================
   ── Part 1: DownValues-based input pattern extraction ──
   ================================================================ *)

(*
  Type names we recognise in our vocabulary.
  These are WL head names: Blank[Symbol[s]] gives _Type for matching.
  Anything else (Graphics, Triangle, Color, ...) maps to None.
*)
$knownTypes = {"String", "Integer", "Real", "Image", "Audio", "Video",
               "Association", "List", "Graph", "SparseArray",
               "Symbol", "Complex", "Rational", "Rule", "RuleDelayed",
               "Quantity"};

normalizeType[t_String] := If[MemberQ[$knownTypes, t], t, None]
normalizeType[None]      := None

(*
  Infer a type string from a PatternTest predicate function.
  e.g.  _?StringQ -> "String",  _?IntegerQ -> "Integer"
*)
inferTypeFromTest[f_] :=
  Switch[If[Developer`SymbolQ[f], SymbolName[f], ""],
    "StringQ" | "Internal`SymbolNameQ",                                  "String",
    "IntegerQ" | "MachineIntegerQ" | "Internal`NonNegativeIntegerQ" |
    "Internal`PositiveIntegerQ" | "EvenQ" | "OddQ" | "PrimeQ",          "Integer",
    "NumberQ" | "NumericQ" | "InexactNumberQ" | "ExactNumberQ" |
    "PositiveQ" | "NonNegativeQ" | "RealQ" | "MachineNumberQ",          "Real",
    "ComplexQ",                                                           "Complex",
    "RationalQ",                                                          "Rational",
    "ImageQ",                                                             "Image",
    "AudioQ",                                                             "Audio",
    "VideoQ",                                                             "Video",
    "ListQ" | "VectorQ" | "MatrixQ",                                     "List",
    "AssociationQ",                                                       "Association",
    "GraphQ",                                                             "Graph",
    "SparseArrayQ",                                                       "SparseArray",
    "SymbolQ",                                                            "Symbol",
    "RuleQ",                                                              "Rule",
    "QuantityQ",                                                          "Quantity",
    _,                                                                    None
  ]

(*
  Extract the type string from a single argument pattern expression as it
  appears in the LHS of a DownValue rule (already extracted via Hold).
*)
typeFromArg[arg_] := normalizeType @ Replace[arg, {
  (* Named blank with type: x_Type *)
  Verbatim[Pattern][_, Verbatim[Blank][t_Symbol]]       :> SymbolName[t],
  (* Unnamed blank with type: _Type *)
  Verbatim[Blank][t_Symbol]                              :> SymbolName[t],
  (* Optional with type: x_Type : default *)
  Verbatim[Optional][Verbatim[Pattern][_,
      Verbatim[Blank][t_Symbol]], _]                     :> SymbolName[t],
  Verbatim[Optional][Verbatim[Blank][t_Symbol], _]       :> SymbolName[t],
  (* PatternTest: x_?StringQ, _?IntegerQ, etc. *)
  Verbatim[PatternTest][_, f_]                           :> inferTypeFromTest[f],
  (* Condition on a typed blank: x_Type /; cond *)
  Verbatim[Condition][Verbatim[Pattern][_,
      Verbatim[Blank][t_Symbol]], _]                     :> SymbolName[t],
  Verbatim[Condition][Verbatim[Blank][t_Symbol], _]      :> SymbolName[t],
  (* Condition wrapping a PatternTest: (x_?f) /; cond *)
  Verbatim[Condition][Verbatim[PatternTest][_, f_], _]   :> inferTypeFromTest[f],
  (* Alternatives: (_Integer | _Real) -> pick first typed alternative *)
  Verbatim[Alternatives][alts___]                        :>
    FirstCase[typeFromArg /@ {alts}, _String, None],
  (* Named/unnamed pattern wrapping Alternatives: x : (_A | _B) *)
  Verbatim[Pattern][_, Verbatim[Alternatives][alts___]]  :>
    FirstCase[typeFromArg /@ {alts}, _String, None],
  (* List of typed elements: {_Integer, _Integer} -> "List" *)
  {___}                                                  :> "List",
  (* Untyped / catch-all -> None *)
  _                                                      :> None
}]

(*
  True if the argument is variadic/options and should not be counted as a
  discrete positional argument in the overload signature.
*)
isVariadicArg[arg_] := MatchQ[arg,
  Verbatim[BlankSequence][___]                                 |
  Verbatim[BlankNullSequence][___]                             |
  Verbatim[Pattern][_, Verbatim[BlankSequence][___]]           |
  Verbatim[Pattern][_, Verbatim[BlankNullSequence][___]]       |
  Verbatim[OptionsPattern][___]                                |
  Verbatim[Pattern][_, Verbatim[OptionsPattern][___]]
]

(*
  True if the arg is a typed or untyped BlankSequence/BlankNullSequence
  (excludes OptionsPattern, which is also variadic but contributes no type info).
*)
isSeqArg[arg_] := MatchQ[arg,
  Verbatim[BlankSequence][___]                             |
  Verbatim[BlankNullSequence][___]                         |
  Verbatim[Pattern][_, Verbatim[BlankSequence][___]]       |
  Verbatim[Pattern][_, Verbatim[BlankNullSequence][___]]
]

(* True if the arg is a BlankSequence (1+), not BlankNullSequence (0+) *)
isBSArg[arg_] := MatchQ[arg,
  Verbatim[BlankSequence][___]                       |
  Verbatim[Pattern][_, Verbatim[BlankSequence][___]]
]

(*
  True if a DownValue rule is an error-handler / argument-check rule.
  Uses Extract[_, {2}, Hold] to inspect the RHS WITHOUT evaluating it,
  so no messages are triggered by doing this check.
*)
isErrorRule[rule_RuleDelayed] :=
  Replace[Extract[rule, {2}, Hold], {
    Hold[$Failed]                                                     :> True,
    Hold[Verbatim[Condition][$Failed, _]]                             :> True,
    Hold[Verbatim[Condition][Null, False]]                            :> True,
    Hold[Verbatim[CompoundExpression][___, Verbatim[Condition][Null, False]]] :> True,
    _                                                                 :> False
  }]
isErrorRule[_] := False

(*
  Functions that return the same container type as their first argument.
  Kernel-verified: Sort[assoc]->assoc, Select[assoc,f]->assoc, Map[f,assoc]->assoc,
  Join[assoc,assoc]->assoc, Join[str,str]->str, Take[assoc,n]->assoc, etc.
  Used in inferReturnFromRHSSafe: when the RHS is f[firstArgVar, ...] with f in this
  set, emit "_[1]" instead of guessing a fixed type.
*)
$inputPreservingFunctions = {
  (* Head-preserving: list->list, assoc->assoc *)
  "Sort", "SortBy", "Select", "Map", "Reverse",
  "DeleteDuplicates", "DeleteDuplicatesBy",
  "Append", "Prepend", "AppendTo", "PrependTo",
  "Take", "Drop", "Most", "Rest", "First", "Last",
  (* Also string-returning for string input: Join, PadLeft, PadRight *)
  "Join", "PadLeft", "PadRight",
  (* Preserves outer head (not necessarily List) *)
  "Flatten", "DeleteCases",
  (* Structure-preserving expression operations *)
  "Transpose"
};

(*
  Return types for common WL functions, keyed by SymbolName.
  Used in inferReturnFromRHSSafe: when the outermost call on a DownValue RHS
  is a function in this map, its return type is inherited directly rather than
  left as None.  Only add entries with near-100% reliability.

  IMPORTANT: do NOT add input-type-preserving functions here (those are in
  $inputPreservingFunctions above and handled separately).  Only add functions
  whose return type is unconditionally fixed regardless of argument types.
*)
$knownFunctionReturnTypes = <|
  (* String-returning: always produce a String regardless of input *)
  "StringJoin"        -> "_String", "StringReplace"     -> "_String",
  "StringReplacePart" -> "_String", "StringTake"        -> "_String",
  "StringDrop"        -> "_String", "StringTrim"        -> "_String",
  "StringInsert"      -> "_String", "StringDelete"      -> "_String",
  "StringReverse"     -> "_String", "StringRepeat"      -> "_String",
  "StringPadLeft"     -> "_String", "StringPadRight"    -> "_String",
  "StringRiffle"      -> "_String", "StringRotateLeft"  -> "_String",
  "StringRotateRight" -> "_String",
  "ToString"          -> "_String", "IntegerString"     -> "_String",
  "TextString"        -> "_String", "ByteArrayToString" -> "_String",
  "CharacterName"     -> "_String", "FromCharacterCode" -> "_String",
  "ToLowerCase"       -> "_String", "ToUpperCase"       -> "_String",
  "Capitalize"        -> "_String", "FileBaseName"      -> "_String",
  "FileExtension"     -> "_String", "FileNameJoin"      -> "_String",
  "DirectoryName"     -> "_String", "ExportString"      -> "_String",
  "TemplateApply"     -> "_String", "StringForm"        -> "_String",
  (* List-returning: always produce a List regardless of input type.
     Excluded: Sort/Select/Map/Join/Take/Drop/Reverse/Flatten/DeleteCases/etc.
     which are input-type-preserving and handled by $inputPreservingFunctions. *)
  "Range"             -> "_List",   "Table"             -> "_List",
  "Array"             -> "_List",   "Union"             -> "_List",
  "Intersection"      -> "_List",   "Complement"        -> "_List",
  "Cases"             -> "_List",   "Position"          -> "_List",
  "Partition"         -> "_List",   "GatherBy"          -> "_List",
  "SplitBy"           -> "_List",   "Tally"             -> "_List",
  "Tuples"            -> "_List",   "Permutations"      -> "_List",
  "Subsets"           -> "_List",   "Riffle"            -> "_List",
  "Catenate"          -> "_List",   "MapThread"         -> "_List",
  "NestList"          -> "_List",   "FoldList"          -> "_List",
  "Thread"            -> "_List",   "Keys"              -> "_List",
  "Values"            -> "_List",   "ArrayReshape"      -> "_List",
  "Differences"       -> "_List",
  (* Integer-returning: always produce an Integer regardless of input *)
  "Length"            -> "_Integer", "StringLength"      -> "_Integer",
  "Depth"             -> "_Integer", "LeafCount"         -> "_Integer",
  "ByteCount"         -> "_Integer", "Hash"              -> "_Integer",
  "Floor"             -> "_Integer", "Ceiling"           -> "_Integer",
  "Round"             -> "_Integer", "IntegerPart"       -> "_Integer",
  "Quotient"          -> "_Integer", "Mod"               -> "_Integer",
  "BitLength"         -> "_Integer", "DigitCount"        -> "_Integer",
  "IntegerLength"     -> "_Integer", "Count"             -> "_Integer",
  (* Integer-returning: number theory / combinatorics *)
  "Factorial"         -> "_Integer", "Factorial2"        -> "_Integer",
  "Binomial"          -> "_Integer", "Fibonacci"         -> "_Integer",
  "LucasL"            -> "_Integer", "Prime"             -> "_Integer",
  "PartitionsP"       -> "_Integer", "StirlingS1"        -> "_Integer",
  "StirlingS2"        -> "_Integer", "BellB"             -> "_Integer",
  "CatalanNumber"     -> "_Integer", "EulerE"            -> "_Integer",
  "BernoulliB"        -> "_Integer",
  "GCD"               -> "_Integer", "LCM"               -> "_Integer",
  "EulerPhi"          -> "_Integer", "MoebiusMu"         -> "_Integer",
  "DivisorSigma"      -> "_Integer", "PrimeOmega"        -> "_Integer",
  "PrimeNu"           -> "_Integer", "JacobiSymbol"      -> "_Integer",
  "KroneckerSymbol"   -> "_Integer", "MangoldtLambda"    -> "_Integer",
  "BitAnd"            -> "_Integer", "BitOr"             -> "_Integer",
  "BitXor"            -> "_Integer", "BitNot"            -> "_Integer",
  "BitShiftLeft"      -> "_Integer", "BitShiftRight"     -> "_Integer",
  "MatrixRank"        -> "_Integer",
  (* Real-returning: always produce a Real when given numeric (real) input.
     NOTE: these return symbolic (non-Real) heads for exact Integer / Rational
     arguments, but when called with a Real argument the output is always Real.
     The typed overloads with "Real" input are added in $mathFunctionOverrides
     below; here we use None so the name-based pass doesn't override a typed DV. *)
  (* Association-returning - from TypeSystem`$Signatures *)
  "Merge"             -> "_Association"
|>;

(*
  Try to infer a return type string from the RHS of a DownValue rule.
  Uses Extract[_, {2}, Hold] to inspect WITHOUT evaluating.

  In addition to constructor-head detection (Image/Audio/Video), checks for
  identity/passthrough patterns where the function returns its first argument:
    - Direct:      RHS is just the first-arg variable symbol
    - Echo-like:   RHS is CompoundExpression[side-effects..., firstArgVar]
  These emit "_[1]" - a sentinel meaning "return type = type of arg 1".
  Also checks the outer call head against $knownFunctionReturnTypes.
*)
inferReturnFromRHSSafe[rule_RuleDelayed, firstArgVar_:Null] :=
  Replace[Extract[rule, {2}, Hold], {
    Hold[Image[___]]     :> "_Image",
    Hold[Audio[___]]     :> "_Audio",
    Hold[Video[___]]     :> "_Video",
    Hold[Graph[___]]     :> "_Graph",
    Hold[Quantity[___]]  :> "_Quantity",
    (* Predicate-style rules: RHS is literally True or False -> boolean return *)
    Hold[True]           :> "_?BooleanQ",
    Hold[False]          :> "_?BooleanQ",
    (* Direct identity: RHS IS the first arg variable *)
    Hold[s_Symbol] /; (firstArgVar =!= Null && s === firstArgVar) :> "_[1]",
    (* Echo-like: RHS is CompoundExpression ending with the first arg variable *)
    Hold[Verbatim[CompoundExpression][___, s_Symbol]] /;
      (firstArgVar =!= Null && s === firstArgVar) :> "_[1]",
    (* RHS is f[firstArgVar, ...] where f preserves the container type of its first
       argument (e.g. Sort[x] is List when x is a List, Association when assoc).
       Emit "_[1]" so downstream logic can resolve it from the caller's arg type. *)
    Hold[f_Symbol[s_Symbol, ___]] /;
      (firstArgVar =!= Null && s === firstArgVar &&
       MemberQ[$inputPreservingFunctions, SymbolName[f]]) :> "_[1]",
    (* RHS is a direct call to a function with an unconditionally fixed return type *)
    Hold[f_Symbol[___]] /;
      KeyExistsQ[$knownFunctionReturnTypes, SymbolName[f]] :>
        $knownFunctionReturnTypes[SymbolName[f]],
    _                    :> None
  }]

(*
  Extract all meaningful overloads from the DownValues of sym.

  Returns a list of associations:
    <| "ArgTypes" -> {type, ...}, "Variadic" -> bool, "RetFromRHS" -> str|None |>
  or {} if DownValues is empty or no parseable overloads exist.

  Key technique: Extract[rule, {1, 1}, Hold] retrieves the contents of
  HoldPattern[...] WITHOUT triggering evaluation of f[args], even for
  built-ins like Echo that would otherwise print their argument.
*)
SetAttributes[extractDVOverloads, HoldFirst]
extractDVOverloads[sym_Symbol] :=
  Module[{dv, overloads},
    dv = DownValues[sym];
    If[dv === {}, Return[{}]];

    overloads = DeleteDuplicatesBy[
      Select[
        Map[Function[{rule},
          If[isErrorRule[rule],
            Nothing,
            Module[{lhsHeld, callHeld, argsList, types, hasVariadic, retRHS},
              (* Safely extract from inside HoldPattern without evaluation *)
              lhsHeld = Extract[rule, {1, 1}, Hold];

              (* Strip top-level Condition if the whole call has /; cond *)
              callHeld = Replace[lhsHeld, {
                Hold[Verbatim[Condition][inner_, _]] :> Hold[inner],
                h_ :> h
              }];

              (* Decompose Hold[f[a1, a2, ...]] -> {a1, a2, ...} *)
              argsList = Replace[callHeld, Hold[_[a___]] :> {a}];
              If[!ListQ[argsList],
                Nothing,
                hasVariadic = AnyTrue[argsList, isVariadicArg];
                (* Drop variadic / options args; keep positional ones *)
                types = typeFromArg /@ Select[argsList, !isVariadicArg[#] &];
                (* Append variadic spec string if a BlankSequence/BlankNullSequence arg exists.
                   "Type..." = 1+ args of that type; "Type*" = 0+ args; "..." / "*" = untyped. *)
                Module[{seqArgs = Select[argsList, isSeqArg[#] &]},
                  If[Length[seqArgs] > 0,
                    With[{sa = First[seqArgs], t = typeFromArg[First[seqArgs]]},
                      AppendTo[types,
                        If[isBSArg[sa],
                          If[StringQ[t], t <> "...", "..."],   (* BlankSequence: 1+ *)
                          If[StringQ[t], t <> "*",   "*"]      (* BlankNullSequence: 0+ *)
                        ]
                      ]
                    ]
                  ]
                ];
                (* Extract the underlying symbol of the first positional arg
                   so inferReturnFromRHSSafe can detect identity patterns. *)
                With[{posArgs = Select[argsList, !isVariadicArg[#] &]},
                  retRHS = inferReturnFromRHSSafe[rule,
                    If[Length[posArgs] > 0,
                      Replace[posArgs[[1]], {
                        Verbatim[Pattern][s_, _] :> s,
                        _ :> Null
                      }],
                      Null
                    ]
                  ]
                ];
                <| "ArgTypes" -> types,
                   "Variadic" -> hasVariadic,
                   "RetFromRHS" -> retRHS |>
              ]
            ]
          ]
        ], dv],
        AssociationQ
      ],
      #["ArgTypes"] &   (* deduplicate overloads with identical arg-type signatures *)
    ];
    overloads
  ]

(* ================================================================
   ── Part 2: PlaintextUsage-based parsing (return types + fallback) ──
   ================================================================ *)

(* ── Argument parsing ── *)

(*
  Infer a WL type string from a single argument token as it appears in
  PlaintextUsage plaintext.  Quoted tokens like "string" map to String;
  unquoted identifiers are classified by common naming conventions.
*)
inferTypeFromToken[tok_String] :=
  Module[{s},
    s = StringTrim[StringReplace[tok, {
      "\"" -> "", "\[CloseCurlyDoubleQuote]" -> "",
      "\[OpenCurlyDoubleQuote]" -> ""
    }]];
    (* Quoted token -> String argument *)
    If[StringStartsQ[tok, "\""] || StringStartsQ[tok, "\[OpenCurlyDoubleQuote]"],
      Return["String"]
    ];
    s = ToLowerCase[StringReplace[s, RegularExpression["\\d+$"] -> ""]];
    Which[
      (* NOTE: pat/patt are WL pattern expressions, not plain strings - excluded *)
      MemberQ[{"s", "str", "string", "substring", "sep", "delimiter",
               "delimiters", "prefix", "suffix", "sval", "char", "word",
               "text", "fmt", "format", "encoding", "context", "lang",
               "language", "name", "filename", "filepath", "path",
               "uri", "url", "key", "label", "tag", "header", "field",
               "ext", "channel", "type", "method", "algorithm",
               "dir", "directory", "domain", "charset"}, s], "String",
      StringMatchQ[s, "s" ~~ DigitCharacter..], "String",
      MemberQ[{"n", "k", "m", "i", "j", "len", "r", "p", "q",
               "width", "height", "depth", "size", "nbins",
               "npts", "nmax", "imax", "imin", "nmin",
               "ndigits", "prec", "digits", "rmax", "level", "pos",
               "base", "bits", "degree", "order", "rank",
               "col", "row", "id", "offset", "step", "num", "count",
               "lev", "beg", "start", "end", "stop", "dim", "d",
               "index", "idx", "iter", "maxiter", "miniter",
               "nterms", "ncoeffs"}, s], "Integer",
      StringMatchQ[s, ("n" | "k" | "m" | "i" | "j" | "r") ~~ DigitCharacter..], "Integer",
      MemberQ[{"list", "lst", "vec", "vector", "matrix", "mat",
               "pair", "tuple", "seq", "sequence", "array",
               "table", "data", "set", "collection"}, s], "List",
      MemberQ[{"image", "img", "mask", "img3d"}, s], "Image",
      MemberQ[{"audio", "snd", "sound"}, s], "Audio",
      MemberQ[{"video"}, s], "Video",
      MemberQ[{"assoc", "association"}, s], "Association",
      MemberQ[{"graph"}, s], "Graph",
      StringMatchQ[s, ("condition*" | "cond*" | "test*" | "pred*" | "predicate*" | "bool*")], "_?BooleanQ",
      True, None
    ]
  ]

(*
  Extract the top-level arguments from the call form at the start of line.
  Auto-detects the function name (everything before the first "[").
  Validates that the prefix looks like a WL symbol name (starts uppercase or $).
  Returns {} if the line does not start with a valid call form.
  Tracks all bracket types ([]{}) to avoid splitting list args like {a,b}.
*)
extractCallArgs[line_String] :=
Module[{rest, sqDepth = 0, curlyDepth = 0, parenDepth = 0,
        cur = "", args = {}, i, c, bp},
  (* Find the opening bracket *)
  bp = First[StringPosition[line, "["], {}];
  If[bp === {}, Return[Missing["NoCall"]]];
  bp = bp[[1]];  (* start index of "[" *)
  (* The prefix must look like a WL identifer: starts uppercase or $, alphanumeric *)
  If[!StringMatchQ[StringTake[line, bp - 1],
       RegularExpression["[A-Z\\$][A-Za-z0-9\\$]*"]], Return[Missing["NoCall"]]];
  rest = StringDrop[line, bp - 1];  (* rest starts with "[..." *)
  (* Walk characters; collect one level inside the outermost [] *)
  Catch[
    Do[
      c = StringTake[rest, {i, i}];
      Which[
        c === "[",
          sqDepth++;
          If[sqDepth > 1, cur = cur <> c],
        c === "]",
          sqDepth--;
          If[sqDepth === 0,
            If[StringLength[StringTrim[cur]] > 0,
              args = Append[args, StringTrim[cur]]
            ];
            Throw[args]  (* {} for zero-arg f[], non-empty list otherwise *)
          ];
          cur = cur <> c,
        c === "{", curlyDepth++; cur = cur <> c,
        c === "}", curlyDepth--; cur = cur <> c,
        c === "(", parenDepth++; cur = cur <> c,
        c === ")", parenDepth--; cur = cur <> c,
        (* Only split on comma when at outermost [] and not inside {} or () *)
        c === "," && sqDepth === 1 && curlyDepth === 0 && parenDepth === 0,
          args = Append[args, StringTrim[cur]]; cur = "",
        True,
          cur = cur <> c
      ],
      {i, StringLength[rest]}
    ];
    Missing["NoClose"]  (* no closing bracket found *)
  ]
]

(*
  Parse ALL call forms found anywhere in a PlaintextUsage line.
  Scans the whole line for every UppercaseIdent[...] occurrence instead of
  requiring the call form to start at position 0.  This handles lines like
    "s1"<>"s2"<>..., StringJoin["s1", "s2", ...], or StringJoin[{...}] yields...
  where the canonical call form follows an infix or operator notation.

  Returns a list (possibly empty) of InputPatterns/ReturnPattern associations.
*)
parseUsageLine[line_String] :=
Module[{positions, forms},
  positions = StringPosition[line, RegularExpression["[A-Z\\$][A-Za-z0-9\\$]*\\["]];
  If[positions === {}, Return[{}]];

  forms = Flatten[Table[
    With[{subline = StringDrop[line, pos[[1]] - 1]},
      With[{args = extractCallArgs[subline]},
        If[MissingQ[args], Nothing,
          With[{
            types = inferTypeFromToken /@ args,
            (* description: strip the leading call form using non-greedy match *)
            desc = StringTrim[StringReplace[subline,
              RegularExpression["^[A-Z\\$][A-Za-z0-9\\$]*\\[.*?\\]\\s*"] -> "", 1]]
          },
            <| "InputPatterns" -> types, "ReturnPattern" -> inferReturnType[desc] |>
          ]
        ]
      ]
    ],
    {pos, positions}
  ], 1];

  (* Deduplicate: same InputPatterns can appear multiple times when a line
     contains multiple representations of the same overload *)
  DeleteDuplicatesBy[forms, #["InputPatterns"] &]
]

(*
  Infer a WL return pattern string from a PlaintextUsage description sentence.
*)
inferReturnType[desc_String] :=
  Which[
    (* Boolean returns: True/False, True if..., True or False, etc. *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*\\bTrue\\b.*\\bFalse\\b.*|.*\\bFalse\\b.*\\bTrue\\b.*"]],
      "_?BooleanQ",
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?|produces?)\\s+(True|False)\\b.*"]],
      "_?BooleanQ",
    StringMatchQ[desc, RegularExpression[
      "(?i).*\\bTrue\\b\\s+(if|when|whenever|iff)\\b.*"]],
      "_?BooleanQ",
    (* String returns *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?|produces?) (a |an |the )?(string|text|name)\\b.*"]],
      "_String",
    (* List/array returns *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?|generates?) (a |an |the )?(list|array|vector|matrix)\\b.*"]],
      "_List",
    (* Integer returns: count or explicit integer.
       Looser form (.{0,25}) catches "gives the number q(n) of ..." (PartitionsQ),
       "gives the number n of ..." and similar counted-quantity phrases. *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?) (a |an |the )?number .{0,25}\\bof\\b.*"]],
      "_Integer",
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?) (a |an |the )?count of\\b.*"]],
      "_Integer",
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?).*(an? integer|integer value|\\binteger\\b).*"]],
      "_Integer",
    (* Image returns *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?).*(a |an |the )?image\\b.*"]],
      "_Image",
    (* Audio returns *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?).*(a |an )(audio|sound)\\b.*"]],
      "_Audio",
    (* Real/numeric returns *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?) (a |an )(number|numeric value|real number)\\b.*"]],
      "_Real",
    (* Association returns *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?).*(a |an |the )?association\\b.*"]],
      "_Association",
	(* Graphics returns *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs|generates?).*(a |an |the )?three-dimensional (plot|chart)\\b.*"]],
      "_Graphics3D",
	(* Graphics returns *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs|generates?).*(a |an |the )?(plot|chart)\\b.*"]],
      "_Graphics",
    (* Graph returns *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(gives?|returns?|yields?|outputs?).*(a |an |the )?graph\\b.*"]],
      "_Graph",
    (* ── Additional patterns: broader verb/phrase coverage ── *)
    (* Image processing: transform/filter/adjust/crop/etc. acting on an image *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(adjusts?|filters?|crops?|clips?|pads?|resizes?|rotates?|reflects?" <>
      "|denoises?|binarizes?|dilates?|erodes?|enhances?|sharpens?|blurs?|normalizes?)" <>
      "\\s.*\\bimage\\b.*"]],
      "_Image",
    (* String: creates/converts/forms/builds a string object *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(creates?|builds?|constructs?|converts?|generates?).*(a |an |the )?(string|text)\\b.*|" <>
      "(?i).*\\bstring\\s+(representation|form|version)\\b.*"]],
      "_String",
    (* List: enumerates/collects/gathers/assembles a list *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(enumerates?|collects?|gathers?|accumulates?|assembles?|constructs?).*(a |an |the )?(list|array|sequence)\\b.*"]],
      "_List",
    (* Integer: computes/calculates a count, rank, or length *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(computes?|calculates?|determines?).*(\\bcount\\b|\\bnumber\\b|\\brank\\b|\\bdegree\\b|\\blength\\b|\\bsize\\b).*\\bof\\b.*"]],
      "_Integer",
    (* 3D Graphics: "three-dimensional graphic/rendering" phrasing not caught above *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*\\b(three.dimensional|3d|3-d)\\b.*(graphic|rendering)\\b.*"]],
      "_Graphics3D",
    (* 2D Graphics: generates/creates/makes a graphic, diagram, figure, or visualization *)
    StringMatchQ[desc, RegularExpression[
      "(?i).*(generates?|makes?|creates?|produces?|plots?|displays?|shows?|renders?).*(a |an |the )?(graphic|diagram|figure|visualization)\\b.*"]],
      "_Graphics",
    True, None
  ]

(* ================================================================
   ── Part 3: Combine DV + PlaintextUsage ──
   ================================================================ *)

(*
  Build a map from arity -> best return type (first non-None for each arity)
  from PlaintextUsage overloads.  Returns an Association (possibly empty).
*)
buildArityReturnMap[puOverloads_List] :=
  Module[{pairs},
    pairs = Select[
      {Length[#["InputPatterns"]], #["ReturnPattern"]} & /@ puOverloads,
      StringQ[#[[2]]] &
    ];
    If[pairs === {}, <||>,
      Association[Rule @@@ DeleteDuplicatesBy[First][pairs]]
    ]
  ]

(*
  Return the best return type for a DV overload:
    1. From inferReturnFromRHS (Image/Audio/Video constructor in RHS).
    2. From PlaintextUsage arity-matched return type.
    3. From the first non-None PU return type (cross-arity fallback).
    4. None.
*)
bestReturnType[dvOv_Association, arityMap_Association, fallback_] :=
  Which[
    StringQ[dvOv["RetFromRHS"]], dvOv["RetFromRHS"],
    KeyExistsQ[arityMap, Length[dvOv["ArgTypes"]]], arityMap[Length[dvOv["ArgTypes"]]],
    StringQ[fallback], fallback,
    True, None
  ]

(* ================================================================
   ── Part 4: Main generation ──
   ================================================================ *)

Print["Collecting System` symbols..."];
allSyms = Names["System`*"];
Print["Total System` symbols: ", Length[allSyms]];

(* Block all interactive prompts early, before any network/cloud calls.
   InputString is the key one: Wolfram Cloud/WolframAlpha auth uses
   InputString["Password: "] which bypasses a plain Input override.
   These are re-applied after the WolframLanguageData batch call in case
   WLD resets them. *)
Unprotect[Input, InputString, Dialog, SystemDialogInput];
Input[___]             := "";
InputString[___]       := "";
Dialog[___]            := Null;
SystemDialogInput[___] := $Canceled;
Protect[Input, InputString, Dialog, SystemDialogInput];
$BatchMode = True;

Print["Fetching PlaintextUsage for all symbols in one batch call..."];
allUsages = Quiet[WolframLanguageData[allSyms, "PlaintextUsage"]];

Print["Building typed pattern associations..."];

(* Re-apply the Input/Dialog override AFTER the WLD batch call.
   WolframLanguageData may reset Input to its original definition.
   Block all interactive prompts so that any subsequent package-init code
   that tries to prompt for credentials or open a dialog gets a silent
   no-op instead of blocking stdin.
   InputString is the key one: Wolfram Cloud/WolframAlpha auth uses
   InputString["Password: "] which bypasses the Input override. *)
Unprotect[Input, InputString, Dialog, SystemDialogInput];
Input[___]             := "";
InputString[___]       := "";
Dialog[___]            := Null;
SystemDialogInput[___] := $Canceled;
Protect[Input, InputString, Dialog, SystemDialogInput];
$BatchMode = True;

(* Use Do + Module to avoid expensive With substitution overhead. *)
results = Association[{}];
Do[
  Module[{sym, usage, puOverloads, dvOvs, arityMap, fallbackRet,
          combined, informative},
    sym   = allSyms[[ii]];
    usage = allUsages[[ii]];

    (* PlaintextUsage overloads - used for return types *)
    puOverloads = If[StringQ[usage],
      DeleteDuplicatesBy[
        Flatten[parseUsageLine /@ StringSplit[usage, "\n"], 1],
        #["InputPatterns"] &
      ],
      {}
    ];

    (* DownValues overloads - primary source for input patterns.
       Use HoldComplete to resolve the symbol name without evaluating it,
       preventing side effects from OwnValue symbols like Black→GrayLevel[0]
       or $Username -> "tonya".  OwnValues check replaces the former
       Head[...] === Symbol guard.  HoldFirst on extractDVOverloads ensures
       the symbol is not evaluated at the call site either. *)
    dvOvs = With[{heldSym = ToExpression["System`" <> sym, InputForm, HoldComplete]},
      If[OwnValues @@ heldSym =!= {},
        {},   (* OwnValue symbol - skip DV extraction *)
        CheckAbort[
          Block[{$RecursionLimit = 256},
            Quiet[heldSym /. HoldComplete[s_] :> extractDVOverloads[s]]],
          {}
        ]
      ]
    ];

    arityMap    = buildArityReturnMap[puOverloads];
    fallbackRet = FirstCase[puOverloads,
      ov_ /; StringQ[ov["ReturnPattern"]] :> ov["ReturnPattern"], None];

    combined = If[dvOvs =!= {},
      (* Use DV for input types, PU for return types *)
      DeleteDuplicatesBy[
        Map[Function[{dv},
          { dv["ArgTypes"],
            bestReturnType[dv, arityMap, fallbackRet] }],
          dvOvs],
        First
      ],
      (* No DV - use PlaintextUsage overloads directly *)
      {#["InputPatterns"], #["ReturnPattern"]} & /@ puOverloads
    ];

    (* Cross-overload return-type propagation.
       If every overload that already has a typed return agrees on exactly one
       type, fill the None slots with that type.  This handles arity-variants
       of the same function, e.g. StringReplace[s,r] returning _String while
       StringReplace[s,r,n] had None after the first-pass inference. *)
    Module[{typedRets, unifType},
      typedRets = Cases[combined[[All, 2]], _String];
      unifType  = DeleteDuplicates[typedRets];
      If[Length[unifType] === 1,
        combined = Map[
          Function[{ov}, If[ov[[2]] === None, {ov[[1]], unifType[[1]]}, ov]],
          combined
        ]
      ]
    ];

    (* Keep only overloads that provide some type info *)
    informative = Select[combined,
      Function[{ov},
        ov[[1]] === {}            ||  (* zero-arg form *)
        AnyTrue[ov[[1]], StringQ] ||  (* at least one typed arg *)
        StringQ[ov[[2]]]              (* known return type *)
      ]
    ];

    If[informative =!= {},
      AssociateTo[results, sym -> informative]
    ]
  ],
  {ii, Length[allSyms]}
];

(*
  Apply hardcoded overrides for well-known identity/passthrough functions and
  functions with conditional return types.

  "_[1]"  sentinel  - return type equals the type of the first argument.
                     Echo[x] returns x; Identity[x] returns x.

  For conditional functions (If, Which, Switch) the return type truly depends
  on which branch runs, so None (unknown) is the honest choice.  Their arity
  and input patterns are recorded accurately so argument-type diagnostics work.
*)
$identityOverrides = <|
  (* Echo prints its first arg then returns it unchanged *)
  "Echo" -> {
    {{None},             "_[1]"},   (* Echo[expr]           *)
    {{None, None},       "_[1]"},   (* Echo[expr, label]    *)
    {{None, None, None}, "_[1]"}    (* Echo[expr, label, f] *)
  },
  (* Identity simply returns its argument *)
  "Identity"        -> {{{None}, "_[1]"}},
  (* EchoTiming returns expr (timing is a side-effect list) *)
  "EchoTiming"      -> {{{None}, "_[1]"}, {{None, None}, "_[1]"}},
  (* EchoEvaluation returns result of evaluating expr *)
  "EchoEvaluation"  -> {{{None}, "_[1]"}, {{None, None}, "_[1]"}},
  (* If: condition + 1 - 2 value branches; return type unknown statically *)
  "If" -> {
    {{"_?BooleanQ", None},             "_[2] | Null"},         (* If[cond, yes]              *)
    {{"_?BooleanQ", None, None},       "_[2] | _[3]"},         (* If[cond, yes, no]          *)
    {{"_?BooleanQ", None, None, None}, "_[2] | _[3] | _[4]"}   (* If[cond, yes, no, missing] *)
  },
  (* Which and Switch are variadic with alternating argument structure.
     Return-type inference is handled at runtime in inferPatternFromRHS /
     resolveCallReturnPattern by name, so no fixed-arity overload entries are
     stored here.  An empty list prevents auto-generated overloads (which would
     be incorrect and could cause false diagnostic positives) from being emitted. *)
  "Which"  -> {},
  "Switch" -> {},

  (* ── Error closures ── *)
  "Enclose" -> {
    {{None}, "_[1] | _Failure"},
    {{None, None}, "_[1] | _[2]"},
    {{None, None, None}, "_[1] | _[2]"}
  },
  "Confirm" -> {
    {{None}, "_[1] | _Failure"},
    {{None, None}, "_[1] | _Failure"},
    {{None, None, None}, "_[1] | _Failure"}
  },
  "ConfirmMatch" -> {
    {{None, None}, "_[1] | _Failure"},
    {{None, None, None}, "_[1] | _Failure"},
    {{None, None, None, None}, "_[1] | _Failure"}
  },
  "ConfirmBy" -> {
    {{None, "_?BooleanQ"}, "_[1] | _Failure"},
    {{None, "_?BooleanQ", None}, "_[1] | _Failure"}
  },
  "ConfirmAssert" -> {{{"_?BooleanQ"}, "Null | _Failure"}},
  "ConfirmQuiet" -> {
    {{None}, "_[1] | _Failure"},
    {{None, None}, "_[1] | _Failure"}
  },

  (* ── Numeric type-preserving / type-promoting functions ──────────────────
     Convention for numeric promotion:
       "Real"    input -> return "_Real"    (machine float in, float out)
       "Integer" input -> return "_Integer" or "_?NumericQ" depending on function
       "Complex" input -> return "_Complex" or "_?NumericQ"

     Common sentinels:
       "_?NumericQ"  - any numeric expression (exact or approximate, real or complex)
       "_Real"       - machine-precision Real
       "_Integer"    - exact Integer
  *)

  (* ── Trigonometric & hyperbolic functions (unary, Real->Real) ── *)
  "Sin"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Cos"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Tan"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Cot"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Sec"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Csc"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "ArcSin"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "ArcCos"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "ArcTan"  -> {
    {{None},       "_?NumericQ"},
    {{"Real"},     "_Real"},
    {{"Complex"},  "_Complex"},
    {{"Real","Real"}, "_Real"}   (* ArcTan[x, y] two-arg form *)
  },
  "ArcCot"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "ArcSec"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "ArcCsc"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Sinh"    -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Cosh"    -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Tanh"    -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Coth"    -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Sech"    -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Csch"    -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "ArcSinh" -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "ArcCosh" -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "ArcTanh" -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "ArcCoth" -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},

  (* ── Exponential / logarithm ── *)
  "Exp"   -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Log"   -> {
    {{None},            "_?NumericQ"},
    {{"Real"},          "_Real"},
    {{"Complex"},       "_Complex"},
    {{"Real", "Real"},  "_Real"},    (* Log[b, z] – base b, value z *)
    {{"Integer","Real"},"_Real"}
  },
  "Log2"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Log10" -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},

  (* ── Powers / roots ── *)
  "Sqrt"  -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Complex"}},
  "Power" -> {
    {{None, None},          "_?NumericQ"},
    {{"Real","Real"},       "_Real"},
    {{"Integer","Integer"}, "_?NumericQ"},   (* may be Rational for negative exponent *)
    {{"Complex","Complex"}, "_Complex"}
  },
  "Plus" -> {
    {{"Integer..."}, "_Integer"},
    {{"Real..."},    "_Real"},
    {{"Complex..."}, "_Complex"},
    {{None},         "_?NumericQ"}
  },
  "Times" -> {
    {{"Integer..."}, "_Integer"},
    {{"Real..."},    "_Real"},
    {{"Complex..."}, "_Complex"},
    {{None},         "_?NumericQ"}
  },

  (* ── Absolute value & sign ── *)
  "Abs"  -> {
    {{None},        "_?NumericQ"},
    {{"Real"},      "_Real"},
    {{"Integer"},   "_Integer"},
    {{"Complex"},   "_Real"}    (* |a+bi| is always a non-negative Real *)
  },
  "Sign" -> {
    {{None},        "_?NumericQ"},
    {{"Real"},      "_Integer"},
    {{"Integer"},   "_Integer"},
    {{"Complex"},   "_Complex"}   (* Sign[1+2I] = (1+2I)/Abs[1+2I] *)
  },
  "Clip" -> {{{None}, "_Real"}, {{"Real"}, "_Real"}, {{"Real","List"}, "_Real"}},

  (* ── Complex number parts ── *)
  "Re"        -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"}, {{"Complex"}, "_Real"},  {{"Integer"}, "_Integer"}},
  "Im"        -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Integer"},{{"Complex"}, "_Real"}, {{"Integer"}, "_Integer"}},
  "Conjugate" -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"},  {{"Complex"}, "_Complex"},{{"Integer"}, "_Integer"}},
  "Arg"       -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"},  {{"Complex"}, "_Real"},  {{"Integer"}, "_Integer"}},
  "Abs2"      -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Real"},  {{"Complex"}, "_Real"}},

  (* ── Rounding / integer extraction ── *)
  "Floor"         -> {{{None}, "_Integer"}, {{None,None}, "_?NumericQ"}, {{"Real"}, "_Integer"}, {{"Real","Real"}, "_Real"}},
  "Ceiling"       -> {{{None}, "_Integer"}, {{None,None}, "_?NumericQ"}, {{"Real"}, "_Integer"}, {{"Real","Real"}, "_Real"}},
  "Round"         -> {{{None}, "_Integer"}, {{None,None}, "_?NumericQ"}, {{"Real"}, "_Integer"}, {{"Real","Real"}, "_Real"}},
  "IntegerPart"   -> {{{None}, "_Integer"}, {{"Real"}, "_Integer"}, {{"Complex"}, "_Complex"}},
  "FractionalPart"-> {{{None}, "_?NumericQ"},{{"Real"}, "_Real"},   {{"Integer"}, "_Integer"}},
  "Chop"          -> {{{None}, "_?NumericQ"}, {{"Real"}, "_?NumericQ"}},
  "Rationalize"   -> {{{None}, "_?NumericQ"}, {{"Real"}, "_Rational"}, {{"Real","Real"}, "_Rational"}},
  "N"             -> {
    {{None},          "_Real"},   (* N[Pi], N[3], N[1/2] *)
    {{None,"Integer"},"_Real"}    (* N[Pi, 50] *)
  },

  (* ── Min / Max ── *)
  "Min" -> {
    {{"Integer..."}, "_Integer"},
    {{"Real..."},    "_Real"},
    {{"List"},       "_?NumericQ"},
    {{None},         "_?NumericQ"}
  },
  "Max" -> {
    {{"Integer..."}, "_Integer"},
    {{"Real..."},    "_Real"},
    {{"List"},       "_?NumericQ"},
    {{None},         "_?NumericQ"}
  },
  "MinMax" -> {{{None}, "_List"}, {{"List"}, "_List"}},

  (* ── Arithmetic: integer-domain functions ── *)
  "Mod"      -> {
    {{"Integer","Integer"},         "_Integer"},
    {{"Integer","Integer","Integer"},"_Integer"},
    {{"Real","Real"},               "_Real"}
  },
  "Quotient" -> {
    {{"Integer","Integer"},         "_Integer"},
    {{"Integer","Integer","Integer"},"_Integer"}
  },
  "GCD"  -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
  "LCM"  -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
  "Factorial"  -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "Factorial2" -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "Binomial"   -> {{{None,None}, "_Integer"}, {{"Integer","Integer"}, "_Integer"}},
  "Fibonacci"  -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}, {{None,None}, "_?NumericQ"}},
  "LucasL"     -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "Prime"      -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "PartitionsP"-> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "EulerPhi"   -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "MoebiusMu"  -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "DivisorSigma" -> {{{None,None}, "_Integer"}, {{"Integer","Integer"}, "_Integer"}},
  "PrimeOmega" -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "PrimeNu"    -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "JacobiSymbol" -> {{{None,None}, "_Integer"}, {{"Integer","Integer"}, "_Integer"}},
  "KroneckerSymbol" -> {{{None,None}, "_Integer"}, {{"Integer","Integer"}, "_Integer"}},
  "BitAnd"     -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
  "BitOr"      -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
  "BitXor"     -> {{{"Integer..."}, "_Integer"}, {{None}, "_Integer"}},
  "BitNot"     -> {{{None}, "_Integer"}, {{"Integer"}, "_Integer"}},
  "BitShiftLeft"  -> {{{None,None}, "_Integer"}, {{"Integer","Integer"}, "_Integer"}},
  "BitShiftRight" -> {{{None,None}, "_Integer"}, {{"Integer","Integer"}, "_Integer"}},

  (* ── Linear algebra ── *)
  "Det"         -> {{{None}, "_?NumericQ"}, {{"List"}, "_?NumericQ"}},
  "Tr"          -> {{{None}, "_?NumericQ"}, {{"List"}, "_?NumericQ"}},
  "MatrixRank"  -> {{{None}, "_Integer"},   {{"List"}, "_Integer"}},
  "Norm"        -> {{{None}, "_?NumericQ"}, {{"List"}, "_Real"}, {{"List","Integer"}, "_Real"}},
  "Eigenvalues" -> {{{None}, "_List"},      {{"List"}, "_List"}},
  "Eigenvectors"-> {{{None}, "_List"},      {{"List"}, "_List"}},
  "Inverse"     -> {{{None}, "_List"},      {{"List"}, "_List"}},
  "LinearSolve" -> {{{None,None}, "_List"}, {{"List","List"}, "_List"}},
  "NullSpace"   -> {{{None}, "_List"},      {{"List"}, "_List"}},
  "RowReduce"   -> {{{None}, "_List"},      {{"List"}, "_List"}},
  "Cross"       -> {{{None,None}, "_List"}, {{"List","List"}, "_List"}},
  "Dot"         -> {{{None,None}, "_?NumericQ"}, {{"List","List"}, "_List"}},
  "Outer"       -> {{{None,None,None}, "_List"}},
  "Inner"       -> {{{None,None,None,None}, "_List"}},

  (* ── Aggregation: list → scalar ── *)
  "Total"  -> {{{None}, "_?NumericQ"}, {{"List"}, "_?NumericQ"}},
  "Mean"   -> {{{None}, "_?NumericQ"}, {{"List"}, "_?NumericQ"}},
  "Median" -> {{{None}, "_?NumericQ"}, {{"List"}, "_?NumericQ"}},
  "Variance"         -> {{{None}, "_Real"}, {{"List"}, "_Real"}},
  "StandardDeviation"-> {{{None}, "_Real"}, {{"List"}, "_Real"}},
  "Covariance"       -> {{{None,None}, "_List"}, {{"List","List"}, "_List"}},
  "Accumulate"       -> {{{None}, "_List"}, {{"List"}, "_List"}},
  "Differences"      -> {{{None}, "_List"}, {{"List"}, "_List"}, {{None,"Integer"}, "_List"}},
  "Ratios"           -> {{{None}, "_List"}, {{"List"}, "_List"}},
  "CumulativeSum"    -> {{{None}, "_List"}},

  (* ── Numeric solving / optimization (always return list of Rules or lists) ── *)
  "Solve"        -> {{{None,None}, "_List"}},
  "NSolve"       -> {{{None,None}, "_List"}},
  "FindRoot"     -> {{{None,None}, "_List"}},
  "Reduce"       -> {{{None,None}, "_?BooleanQ"}},
  "NMinimize"    -> {{{None,None}, "_List"}},   (* {minval, {x->v,...}} *)
  "NMaximize"    -> {{{None,None}, "_List"}},
  "Minimize"     -> {{{None,None}, "_List"}},
  "Maximize"     -> {{{None,None}, "_List"}},
  "FindMinimum"  -> {{{None,None}, "_List"}},
  "FindMaximum"  -> {{{None,None}, "_List"}},

  (* ── Random number generation ── *)
  "RandomReal"    -> {
    {{},             "_Real"},
    {{None},         "_Real"},
    {{None,"Integer"},"_List"},
    {{None,"List"},   "_List"}
  },
  "RandomInteger" -> {
    {{},             "_Integer"},
    {{None},         "_Integer"},
    {{None,"Integer"},"_List"},
    {{None,"List"},   "_List"}
  },
  "RandomComplex" -> {
    {{},             "_Complex"},
    {{None},         "_Complex"},
    {{None,"Integer"},"_List"},
    {{None,"List"},   "_List"}
  },
  "RandomChoice"  -> {{{None}, "_?True"}, {{"List"}, None}},  (* type depends on list elements *)

  (* ── Numeric predicates (guaranteed boolean) ── *)
  "PrimeQ"       -> {{{None}, "_?BooleanQ"}, {{"Integer"}, "_?BooleanQ"}, {{"Integer","Integer"}, "_?BooleanQ"}},
  "EvenQ"        -> {{{None}, "_?BooleanQ"}},
  "OddQ"         -> {{{None}, "_?BooleanQ"}},
  "IntegerQ"     -> {{{None}, "_?BooleanQ"}},
  "NumberQ"      -> {{{None}, "_?BooleanQ"}},
  "NumericQ"     -> {{{None}, "_?BooleanQ"}},
  "ExactNumberQ" -> {{{None}, "_?BooleanQ"}},
  "InexactNumberQ"-> {{{None}, "_?BooleanQ"}},
  "RealValuedNumberQ" -> {{{None}, "_?BooleanQ"}},
  "PositiveQ"    -> {{{None}, "_?BooleanQ"}},
  "NonNegativeQ" -> {{{None}, "_?BooleanQ"}},
  "NegativeQ"    -> {{{None}, "_?BooleanQ"}},
  "NonPositiveQ" -> {{{None}, "_?BooleanQ"}},

  (* ── Comparison operators (always return boolean) ── *)
  "Greater"       -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "GreaterEqual"  -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "Less"          -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "LessEqual"     -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "Equal"         -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "Unequal"       -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "SameQ"         -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "UnsameQ"       -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "OrderedQ"      -> {{{None}, "_?BooleanQ"}},
  "MemberQ"       -> {{{None, None}, "_?BooleanQ"}},
  "FreeQ"         -> {{{None, None}, "_?BooleanQ"}},
  "SubsetQ"       -> {{{None, None}, "_?BooleanQ"}},
  "ContainsAll"   -> {{{None, None}, "_?BooleanQ"}},
  "ContainsAny"   -> {{{None, None}, "_?BooleanQ"}},
  "ContainsNone"  -> {{{None, None}, "_?BooleanQ"}},
  "MatchQ"        -> {{{None, None}, "_?BooleanQ"}},
  "StringMatchQ"  -> {{{None, None}, "_?BooleanQ"}},
  "StringContainsQ"-> {{{None, None}, "_?BooleanQ"}},
  "StringStartsQ" -> {{{None, None}, "_?BooleanQ"}},
  "StringEndsQ"   -> {{{None, None}, "_?BooleanQ"}},
  "ArrayQ"        -> {{{None}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "MatrixQ"       -> {{{None}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "VectorQ"       -> {{{None}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "SquareMatrixQ" -> {{{None}, "_?BooleanQ"}},
  "SymmetricMatrixQ" -> {{{None}, "_?BooleanQ"}},
  "HermitianMatrixQ" -> {{{None}, "_?BooleanQ"}},
  "DiagonalMatrixQ"  -> {{{None}, "_?BooleanQ"}},

  (* ── Logical operators (always return boolean) ── *)
  "And"     -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "Or"      -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "Not"     -> {{{None}, "_?BooleanQ"}},
  "Xor"     -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "Nand"    -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "Nor"     -> {{{"..."}, "_?BooleanQ"}, {{None, None}, "_?BooleanQ"}},
  "Implies" -> {{{None, None}, "_?BooleanQ"}},
  "TrueQ"   -> {{{None}, "_?BooleanQ"}},
  "BooleanQ"-> {{{None}, "_?BooleanQ"}}
|>;

(* Merge overrides: replace auto-generated entry if override exists *)
KeyValueMap[
  Function[{sym, ovs}, AssociateTo[results, sym -> ovs]],
  $identityOverrides
];

(*
  Name-based inference pass (final fallback).
  Applied only to symbols whose every overload still has a None return type,
  i.e. all prior inference sources failed.  Rules are conservative: only
  symbol-naming conventions that are reliable across the whole System` namespace.

  Math functions that end in Q but are NOT boolean predicates are excluded.
  The complete list was determined by querying WolframLanguageData[..., "FunctionalityAreas"]
  for all System`*Q symbols and selecting those in the "MathFunctions" area:
    BilateralHypergeometricPFQ, EllipticNomeQ, HypergeometricPFQ,
    InverseEllipticNomeQ, LegendreQ, MarcumQ, PartitionsQ, QHypergeometricPFQ.
  Note: PartitionsQ returns an Integer ("gives the number q(n) of partitions"),
  not a boolean - the improved integer regex below handles return type correctly.

  String* exceptions for functions that return non-strings:
    StringExpression, StringToByteArray, StringToStream, StringPartition,
    StringTakeDrop, StringTemplate, StringSkeleton.
*)
$nameInferenceExcludeQ =
  {"BilateralHypergeometricPFQ", "EllipticNomeQ", "HypergeometricPFQ",
   "InverseEllipticNomeQ", "LegendreQ", "MarcumQ",
   "PartitionsQ", "QHypergeometricPFQ"};
$nameInferenceExcludeString =
  {"StringExpression", "StringToByteArray", "StringToStream",
   "StringPartition", "StringTakeDrop", "StringTemplate",
   "StringSkeleton", "StringMatchQ", "StringContainsQ",
   "StringFreeQ", "StringStartsQ", "StringEndsQ", "StringQ",
   "StringCases", "StringSplit", "StringCount", "StringApply"};
$nameInferenceExcludeList = {"SampledSoundList"};

KeyValueMap[
  Function[{sym, ovs},
    Module[{inferredRet, updated},
      (* Only apply when ALL overloads still lack a return type *)
      If[AnyTrue[ovs, StringQ[#[[2]]] &], Return[]];
      inferredRet = Which[
        (* Q-suffix predicates - excluding special math functions *)
        StringEndsQ[sym, "Q"] && !MemberQ[$nameInferenceExcludeQ, sym],
          "_?BooleanQ",
        (* String* prefix: functions that construct or manipulate strings *)
        StringStartsQ[sym, "String"] &&
          !MemberQ[$nameInferenceExcludeString, sym],
          "_String",
        (* *List suffix: functions that return a list *)
        StringEndsQ[sym, "List"] &&
          !MemberQ[$nameInferenceExcludeList, sym],
          "_List",
        (* *Count suffix: functions that return a count integer *)
        StringEndsQ[sym, "Count"],
          "_Integer",
        (* *Plot3D / *Chart3D suffix: three-dimensional plots *)
        StringEndsQ[sym, "Plot3D"] || StringEndsQ[sym, "Chart3D"],
          "_Graphics3D",
        (* *Plot / *Chart suffix: two-dimensional plots *)
        StringEndsQ[sym, "Plot"] || StringEndsQ[sym, "Chart"],
          "_Graphics",
        True, None
      ];
      If[StringQ[inferredRet],
        updated = Map[
          Function[{ov}, If[ov[[2]] === None, {ov[[1]], inferredRet}, ov]],
          ovs
        ];
        AssociateTo[results, sym -> updated]
      ]
    ]
  ],
  results
];

Print["Symbols with typed patterns: ", Length[results]];

With[{
  totalOvs    = Total[Length /@ Values[results]],
  typedArgOvs = Total[Map[
    Function[{ovs}, Length[Select[ovs, AnyTrue[#[[1]], StringQ] &]]],
    Values[results]]],
  typedRetOvs = Total[Map[
    Function[{ovs}, Length[Select[ovs, StringQ[#[[2]]] &]]],
    Values[results]]]
},
  Print["Total overloads: ", totalOvs];
  Print["Overloads with typed arg(s): ", typedArgOvs,
    " (", Round[100 typedArgOvs / totalOvs], "%)"];
  Print["Overloads with typed return:  ", typedRetOvs,
    " (", Round[100 typedRetOvs / totalOvs], "%)"];
];

outPath = FileNameJoin[{
  DirectoryName[DirectoryName[$InputFileName]],
  "..", "LSPServer", "Resources", "BuiltinInputPatterns.wl"
}];

If[!DirectoryQ[DirectoryName[outPath]],
  CreateDirectory[DirectoryName[outPath]]
];

Print["Writing: ", outPath];

stream = OpenWrite[outPath];

WriteString[stream,
  "(* Auto-generated by CodeTools/Generate/GenerateBuiltinPatterns.wl *)\n"
  <> "(* Regenerate: wolframscript -file CodeTools/Generate/GenerateBuiltinPatterns.wl *)\n\n"
  <> "(* Each entry maps a symbol name to a list of overloads.\n"
  <> "   Each overload is {inputPatternList, returnPatternString}.\n"
  <> "   inputPatternList entries: type string (\"String\", \"Integer\", \"Real\",\n"
  <> "     \"Image\", \"Audio\", \"Video\", \"Association\", \"List\", \"Graph\",\n"
  <> "     \"SparseArray\", \"Symbol\", \"Complex\", \"Rational\", \"Rule\",\n"
  <> "     \"RuleDelayed\", \"Quantity\", \"_?BooleanQ\") or None (untyped).\n"
  <> "     {} means zero arguments.\n"
  <> "   returnPatternString: \"_Integer\", \"_String\", \"_?BooleanQ\", \"_List\",\n"
  <> "     \"_Image\", \"_Audio\", \"_Real\", \"_Association\", \"_Graph\",\n"
  <> "     \"_Quantity\", \"_[1]\" (return type = type of arg 1, e.g. Echo or Identity),\n"
  <> "     or None.\n"
  <> "   Sources: DownValues (input patterns), WolframLanguageData PlaintextUsage\n"
  <> "     (return types + PU-only fallback), cross-overload propagation,\n"
  <> "     and name-based conventions ( *Q, String*, *List, *Count, *Plot, *Chart ). *)\n\n"
];

WriteString[stream, "$BuiltinPatterns = <|\n"];
KeyValueMap[
  Function[{sym, overloads},
    WriteString[stream,
      "  \"" <> sym <> "\" -> "
      <> ToString[InputForm[overloads]] <> ",\n"
    ]
  ],
  results
];
WriteString[stream, "  Nothing\n|>;\n"];

Close[stream];
Print["Done. Output: ", outPath];
