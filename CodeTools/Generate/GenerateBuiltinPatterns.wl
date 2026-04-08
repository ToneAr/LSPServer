(*
GenerateBuiltinPatterns.wl

Generates LSPServer/Resources/BuiltinInputPatterns.wl for every System` symbol.

Eight complementary inference sources are used, in priority order:
1. DownValues of each symbol - exact WL input patterns from actual definitions.
	~3400 System` symbols have non-empty DownValues.
2. RHS analysis of DownValue rules - $knownFunctionReturnTypes map is used to
	infer the return type when the RHS is a direct call to a well-known function
	(e.g. StringJoin, Table, Length).  Deeper RHS analysis traverses
	Module/Block/With bodies, If/Which conditionals, and nested function
	calls to find the actual return expression.
3. WolframLanguageData["sym", "PlaintextUsage"] - prose description parsed with
	verb/phrase regexes for return type inference; also used as fallback for
	symbols without useful DownValues.
4. TypeSystem`$Signatures - formal type inference rules from the Wolfram
	TypeSystem (104 functions) are translated to our format for ground-truth
	return type information.
5. Attributes-based inference:
	a. NumericFunction attribute -> return _?NumericQ for numeric inputs
	   (367 functions).
	b. Listable attribute -> List input produces List output (490 functions).
6. SyntaxInformation["ArgumentsPattern"] - arity and argument structure
	for ~5300 symbols; used to generate missing overloads and validate
	argument counts.
7. Post-processing passes:
	a. Cross-overload propagation: when all typed overloads of a symbol agree
		on one return type, that type is filled into the remaining None overloads.
	b. Name-based fallback: symbol-naming conventions ( *Q, String*, *List,
		*Count, *Plot, *Chart) applied as a last resort.
8. Hardcoded overrides for identity/passthrough functions and functions
	with conditional return types (If, Echo, Switch, etc.).

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
			"Quantity", "DateObject", "TimeObject",
			"ByteArray", "File", "URL", "NumericArray"};

normalizeType[t_String] := If[MemberQ[$knownTypes, t], t, None]
normalizeType[None]      := None

(*
Infer a type string from a PatternTest predicate function.
e.g.  _?StringQ -> "String",  _?IntegerQ -> "Integer"
*)
inferTypeFromTest[f_] :=
Switch[
	If[Developer`SymbolQ[f], SymbolName[f], ""],
	"StringQ" | "Internal`SymbolNameQ",                                  "String",
	"IntegerQ" | "MachineIntegerQ" | "Internal`NonNegativeIntegerQ" |
	"Internal`PositiveIntegerQ" | "EvenQ" | "OddQ" | "PrimeQ",          "Integer",
	"NumberQ" | "NumericQ" | "InexactNumberQ" | "ExactNumberQ" |
	"PositiveQ" | "NonNegativeQ" | "RealQ" | "MachineNumberQ" |
	"Internal`RealValuedNumberQ" | "Internal`RealValuedNumericQ" |
	"Positive" | "NonNegative" | "Negative" | "NonPositive",             "Real",
	"ComplexQ",                                                "Complex",
	"RationalQ",                                               "Rational",
	"ImageQ" | "Image`ValidImageQ",                            "Image",
	"AudioQ",                                                  "Audio",
	"VideoQ",                                                  "Video",
	"ListQ" | "VectorQ" | "MatrixQ" | "ArrayQ",               "List",
	"AssociationQ",                                            "Association",
	"GraphQ" | "IGGraphQ",                                     "Graph",
	"SparseArrayQ",                                            "SparseArray",
	"SymbolQ" | "Developer`SymbolQ",                           "Symbol",
	"RuleQ",                                                   "Rule",
	"QuantityQ" | "Internal`QuantityQ",                        "Quantity",
	"BooleanQ" | "TrueQ",                                     "_?BooleanQ",
	_,                                                         None
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
Verbatim[BlankSequence][___]                           |
Verbatim[BlankNullSequence][___]                       |
Verbatim[Pattern][_, Verbatim[BlankSequence][___]]     |
Verbatim[Pattern][_, Verbatim[BlankNullSequence][___]] |
Verbatim[OptionsPattern][___]                          |
Verbatim[Pattern][_, Verbatim[OptionsPattern][___]]
]

(*
True if the arg is a typed or untyped BlankSequence/BlankNullSequence
(excludes OptionsPattern, which is also variadic but contributes no type info).
*)
isSeqArg[arg_] :=
	MatchQ[arg,
		Verbatim[BlankSequence][___]                         |
		Verbatim[BlankNullSequence][___]                     |
		Verbatim[Pattern][_, Verbatim[BlankSequence][___]]   |
		Verbatim[Pattern][_, Verbatim[BlankNullSequence][___]]
	]

(* True if the arg is a BlankSequence (1+), not BlankNullSequence (0+) *)
isBSArg[arg_] :=
	MatchQ[arg,
		Verbatim[BlankSequence][___]                     |
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
"ReverseSort", "ReverseSortBy", "Discard",
"RotateLeft", "RotateRight",
"MapAt", "ReplacePart",
(* Also string-returning for string input: Join, PadLeft, PadRight *)
"Join", "PadLeft", "PadRight",
(* Preserves outer head (not necessarily List) *)
"Flatten", "DeleteCases",
(* Structure-preserving expression operations *)
"Transpose",
(* Association-preserving *)
"KeySort", "KeySortBy", "KeyDrop", "KeyTake",
"AssociationMap", "Merge"
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
"DateString"        -> "_String", "FileHash"          -> "_String",
"SymbolName"        -> "_String", "Context"           -> "_String",
"InputForm"         -> "_String", "OutputForm"        -> "_String",
"FullForm"          -> "_String", "TeXForm"           -> "_String",
"MathMLForm"        -> "_String", "CForm"             -> "_String",
"FortranForm"       -> "_String",
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
"Subdivide"         -> "_List",   "FactorInteger"     -> "_List",
"IntegerDigits"     -> "_List",   "RealDigits"        -> "_List",
"Characters"        -> "_List",   "CharacterRange"    -> "_List",
"StringSplit"       -> "_List",   "StringCases"       -> "_List",
"StringPosition"    -> "_List",
"Normal"            -> "_List",
"Dimensions"        -> "_List",
"MinimalBy"         -> "_List",   "MaximalBy"         -> "_List",
"TakeLargest"       -> "_List",   "TakeSmallest"      -> "_List",
"TakeLargestBy"     -> "_List",   "TakeSmallestBy"    -> "_List",
"ConstantArray"     -> "_List",
"Ordering"          -> "_List",
"Counts"            -> "_Association", "CountsBy"      -> "_Association",
"GroupBy"           -> "_Association",
"AssociationThread" -> "_Association",
(* Integer-returning: always produce an Integer regardless of input *)
"Length"            -> "_Integer", "StringLength"      -> "_Integer",
"Depth"             -> "_Integer", "LeafCount"         -> "_Integer",
"ByteCount"         -> "_Integer", "Hash"              -> "_Integer",
"Floor"             -> "_Integer", "Ceiling"           -> "_Integer",
"Round"             -> "_Integer", "IntegerPart"       -> "_Integer",
"Quotient"          -> "_Integer", "Mod"               -> "_Integer",
"BitLength"         -> "_Integer", "DigitCount"        -> "_Integer",
"IntegerLength"     -> "_Integer", "Count"             -> "_Integer",
"StringCount"       -> "_Integer",
"Boole"             -> "_Integer",
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
(* Boolean-returning *)
"TrueQ"             -> "_?BooleanQ", "FreeQ"           -> "_?BooleanQ",
"MemberQ"           -> "_?BooleanQ", "MatchQ"          -> "_?BooleanQ",
"OrderedQ"          -> "_?BooleanQ", "SubsetQ"         -> "_?BooleanQ",
"FileExistsQ"       -> "_?BooleanQ", "DirectoryQ"      -> "_?BooleanQ",
"StringQ"           -> "_?BooleanQ", "IntegerQ"        -> "_?BooleanQ",
"NumberQ"           -> "_?BooleanQ", "NumericQ"        -> "_?BooleanQ",
"ListQ"             -> "_?BooleanQ", "AssociationQ"    -> "_?BooleanQ",
"AtomQ"             -> "_?BooleanQ",
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
Deeper analysis: traverses Module/Block/With bodies and If/Which conditionals
to find the actual return expression, instead of stopping at scoping wrappers.
*)

(*
Helper: infer return type from a held expression (the "body" of a rule or
scoping construct).  This is the recursive workhorse that traverses through
Module/Block/With, CompoundExpression, and conditional forms.
*)
inferReturnFromHeldExpr[held_, firstArgVar_:Null] :=
Replace[held, {
	(* Constructor heads *)
	Hold[Image[___]]       :> "_Image",
	Hold[Audio[___]]       :> "_Audio",
	Hold[Video[___]]       :> "_Video",
	Hold[Graph[___]]       :> "_Graph",
	Hold[Quantity[___]]    :> "_Quantity",
	Hold[SparseArray[___]] :> "_SparseArray",
	Hold[Association[___]] :> "_Association",
	Hold[DateObject[___]]  :> "_DateObject",
	Hold[TimeObject[___]]  :> "_TimeObject",
	Hold[ByteArray[___]]   :> "_ByteArray",
	(* Predicate-style: RHS is literally True or False *)
	Hold[True]           :> "_?BooleanQ",
	Hold[False]          :> "_?BooleanQ",
	(* Direct identity: RHS IS the first arg variable *)
	Hold[s_Symbol] /; (firstArgVar =!= Null && s === firstArgVar) :> "_[1]",
	(* Echo-like: CompoundExpression ending with the first arg variable *)
	Hold[Verbatim[CompoundExpression][___, s_Symbol]] /;
	(firstArgVar =!= Null && s === firstArgVar) :> "_[1]",
	(* Input-preserving function applied to first arg *)
	Hold[f_Symbol[s_Symbol, ___]] /;
	(firstArgVar =!= Null && s === firstArgVar &&
	MemberQ[$inputPreservingFunctions, SymbolName[f]]) :> "_[1]",
	(* Direct call to a function with a known fixed return type *)
	Hold[f_Symbol[___]] /;
	KeyExistsQ[$knownFunctionReturnTypes, SymbolName[f]] :>
		$knownFunctionReturnTypes[SymbolName[f]],

	(* ── Deeper analysis: traverse scoping constructs ── *)

	(* Module[{...}, body] / Block[{...}, body] / With[{...}, body]:
	   recurse into the body (the last part of the scoping form). *)
	Hold[Verbatim[Module][_, body_]] :>
		inferReturnFromHeldExpr[Hold[body], firstArgVar],
	Hold[Verbatim[Block][_, body_]] :>
		inferReturnFromHeldExpr[Hold[body], firstArgVar],
	Hold[Verbatim[With][_, body_]] :>
		inferReturnFromHeldExpr[Hold[body], firstArgVar],

	(* CompoundExpression: the return value is the last expression *)
	Hold[Verbatim[CompoundExpression][___, last_]] :>
		inferReturnFromHeldExpr[Hold[last], firstArgVar],

	(* If[cond, then, else]: if both branches agree on a type, use that type *)
	Hold[Verbatim[If][_, then_, else_]] :>
		With[{t1 = inferReturnFromHeldExpr[Hold[then], firstArgVar],
		      t2 = inferReturnFromHeldExpr[Hold[else], firstArgVar]},
		If[StringQ[t1] && t1 === t2, t1, None]
		],
	Hold[Verbatim[If][_, then_, else_, missing_]] :>
		With[{t1 = inferReturnFromHeldExpr[Hold[then], firstArgVar],
		      t2 = inferReturnFromHeldExpr[Hold[else], firstArgVar]},
		If[StringQ[t1] && t1 === t2, t1, None]
		],

	(* Condition[expr, test]: the value is expr *)
	Hold[Verbatim[Condition][expr_, _]] :>
		inferReturnFromHeldExpr[Hold[expr], firstArgVar],

	(* Nested Module/Block inside CompoundExpression (common pattern):
	   CompoundExpression[..., Module[{...}, body]] *)
	Hold[Verbatim[CompoundExpression][___,
		Verbatim[Module][_, body_]]] :>
		inferReturnFromHeldExpr[Hold[body], firstArgVar],
	Hold[Verbatim[CompoundExpression][___,
		Verbatim[Block][_, body_]]] :>
		inferReturnFromHeldExpr[Hold[body], firstArgVar],

	_                    :> None
}]

inferReturnFromRHSSafe[rule_RuleDelayed, firstArgVar_:Null] :=
	inferReturnFromHeldExpr[Extract[rule, {2}, Hold], firstArgVar]

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
	(* ── Date/Time returns ── *)
	StringMatchQ[desc, RegularExpression[
	"(?i).*(gives?|returns?|yields?|outputs?).*(a |an |the )?(date|time|datetime|dateobject|timeobject)\\b.*"]],
	"_DateObject",
	(* ── Quantity returns ── *)
	StringMatchQ[desc, RegularExpression[
	"(?i).*(gives?|returns?|yields?|outputs?).*(a |an |the )?quantity\\b.*"]],
	"_Quantity",
	(* ── SparseArray returns ── *)
	StringMatchQ[desc, RegularExpression[
	"(?i).*(gives?|returns?|yields?|outputs?|creates?|constructs?).*(a |an |the )?sparse\\s*array\\b.*"]],
	"_SparseArray",
	(* ── "tests whether" / "determines whether" / "checks whether" phrasing → boolean ── *)
	StringMatchQ[desc, RegularExpression[
	"(?i).*(tests?|determines?|checks?|verifies?|validates?)\\s+(whether|if|that)\\b.*"]],
	"_?BooleanQ",
	(* ── "is equivalent to" True/False ── *)
	StringMatchQ[desc, RegularExpression[
	"(?i).*\\bis\\s+(equivalent|identical|equal)\\s+to\\s+(True|False)\\b.*"]],
	"_?BooleanQ",
	(* ── "represents" phrasing for constructor-like functions ── *)
	(* "represents an image" *)
	StringMatchQ[desc, RegularExpression[
	"(?i).*\\brepresents\\b.*(an? |the )?(image|picture|bitmap)\\b.*"]],
	"_Image",
	(* "represents a graph" *)
	StringMatchQ[desc, RegularExpression[
	"(?i).*\\brepresents\\b.*(a |an |the )?graph\\b.*"]],
	"_Graph",
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

(* Load TypeSystem for $Signatures integration in Part 4b *)
Needs["TypeSystem`"];

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
	preventing side effects from OwnValue symbols like Black->GrayLevel[0]
	or $Username -> "tonya".  OwnValues check replaces the former
	Head[...] === Symbol guard.  HoldFirst on extractDVOverloads ensures
	the symbol is not evaluated at the call site either. *)
	dvOvs = With[{heldSym = ToExpression["System`" <> sym, InputForm, HoldComplete]},
	If[Quiet[OwnValues @@ heldSym, General::readp] =!= {},
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

(* ================================================================
── Part 4b: TypeSystem`$Signatures integration ──
   Probe functions with standard input types via TypeSystem`TypeApply
   to derive ground-truth return types for ~104 common functions.
================================================================ *)

Print["Integrating TypeSystem`$Signatures (", Length[TypeSystem`$Signatures], " functions)..."];

(*
Convert a TypeSystem type expression to our return-type string.
Only handles the simple/common cases that produce a fixed output type.
Note: IntegerT/RealT/StringT etc. are aliases that evaluate to Atom[Integer]
etc., so we match on the evaluated Atom[...] forms.
*)
tsTypeToReturn[TypeSystem`Atom[Integer]]       := "_Integer"
tsTypeToReturn[TypeSystem`Atom[Real]]          := "_Real"
tsTypeToReturn[TypeSystem`Atom[String]]        := "_String"
tsTypeToReturn[TypeSystem`Atom[Complex]]       := "_Complex"
tsTypeToReturn[TypeSystem`Atom[Rational]]      := "_Rational"
tsTypeToReturn[TypeSystem`Atom[Boolean]]       := "_?BooleanQ"
tsTypeToReturn[TypeSystem`Vector[_, ___]]      := "_List"
tsTypeToReturn[TypeSystem`Assoc[_, _, ___]]    := "_Association"
tsTypeToReturn[TypeSystem`Tuple[___]]          := "_List"
tsTypeToReturn[TypeSystem`Struct[___]]         := "_Association"
tsTypeToReturn[_]                              := None

(*
Convert a TypeSystem type expression to our argument type string.
*)
tsTypeToArgStr[TypeSystem`Atom[Integer]]       := "Integer"
tsTypeToArgStr[TypeSystem`Atom[Real]]          := "Real"
tsTypeToArgStr[TypeSystem`Atom[String]]        := "String"
tsTypeToArgStr[TypeSystem`Atom[Complex]]       := "Complex"
tsTypeToArgStr[TypeSystem`Atom[Rational]]      := "Rational"
tsTypeToArgStr[TypeSystem`Atom[Boolean]]       := "_?BooleanQ"
tsTypeToArgStr[TypeSystem`Vector[_, ___]]      := "List"
tsTypeToArgStr[TypeSystem`Assoc[_, _, ___]]    := "Association"
tsTypeToArgStr[_]                              := None

(*
Probe types: standard inputs to test against each function.
Each is {TypeSystemExpr, ourArgTypeString}.
*)
$tsProbeTypes = {
	TypeSystem`Atom[Integer],
	TypeSystem`Atom[Real],
	TypeSystem`Atom[String],
	TypeSystem`Atom[Complex],
	TypeSystem`Vector[TypeSystem`Atom[Integer], TypeSystem`UnknownLength],
	TypeSystem`Vector[TypeSystem`Atom[String], TypeSystem`UnknownLength],
	TypeSystem`Assoc[TypeSystem`Atom[String], TypeSystem`Atom[Integer], TypeSystem`UnknownLength]
};

(*
For each function in TypeSystem`$Signatures, probe it with standard 1-arg and
2-arg type combinations using TypeApply.  Collect successful {argTypes, retType}
overloads and merge them into the results.
*)
tsIntegrationCount = 0;
KeyValueMap[
Function[{sym, rules},
	With[{symName = SymbolName[sym]},
	Module[{tsOverloads = {}, retStr, argStr},
		(* 1-arg probes *)
		Do[
		retStr = tsTypeToReturn[Quiet[TypeSystem`TypeApply[sym, {pt}]]];
		If[StringQ[retStr],
			argStr = tsTypeToArgStr[pt];
			AppendTo[tsOverloads, {{argStr}, retStr}]
		],
		{pt, $tsProbeTypes}
		];
		(* 2-arg probes: common combinations *)
		Do[
		retStr = tsTypeToReturn[Quiet[TypeSystem`TypeApply[sym, {pt1, pt2}]]];
		If[StringQ[retStr],
			AppendTo[tsOverloads, {{tsTypeToArgStr[pt1], tsTypeToArgStr[pt2]}, retStr}]
		],
		{pt1, $tsProbeTypes},
		{pt2, {TypeSystem`Atom[Integer], TypeSystem`Atom[String],
		       TypeSystem`Vector[TypeSystem`Atom[Integer], TypeSystem`UnknownLength],
		       TypeSystem`AnyType}}
		];
		(* Also probe zero-arg *)
		retStr = tsTypeToReturn[Quiet[TypeSystem`TypeApply[sym, {}]]];
		If[StringQ[retStr],
		AppendTo[tsOverloads, {{}, retStr}]
		];

		tsOverloads = DeleteDuplicatesBy[tsOverloads, First];
		If[tsOverloads =!= {},
		If[KeyExistsQ[results, symName],
			(* Merge: back-fill return types for existing overloads where we have None,
			   and add new typed overloads *)
			Module[{existing = results[symName],
			        tsRetTypes = DeleteDuplicates[Cases[tsOverloads[[All, 2]], _String]]},
			(* Back-fill return type if TypeSystem agrees on a single type *)
			If[Length[tsRetTypes] === 1,
				existing = Map[
				Function[{ov}, If[ov[[2]] === None, {ov[[1]], tsRetTypes[[1]]}, ov]],
				existing
				]
			];
			(* Add overloads not already present *)
			Module[{newOvs = Select[tsOverloads,
				Function[{tsOv}, !MemberQ[existing[[All, 1]], tsOv[[1]]]]
			]},
				If[newOvs =!= {},
				AssociateTo[results, symName -> Join[existing, newOvs]]
				]
			];
			],
			AssociateTo[results, symName -> tsOverloads]
		];
		tsIntegrationCount++
		]
	]
	]
],
TypeSystem`$Signatures
];
Print["  TypeSystem enriched ", tsIntegrationCount, " symbols."];

(* ================================================================
── Part 4c: Attributes-based inference ──
   NumericFunction -> return _?NumericQ for numeric inputs.
   Listable -> List input produces List output.
================================================================ *)

Print["Applying Attributes-based inference..."];

(*
Collect the NumericFunction and Listable attributes for all System` symbols.
Built once, then used in the loop below.
*)
$numericFunctionSyms = {};
$listableSyms = {};
Do[
	With[{heldSym = ToExpression[s, InputForm, HoldComplete]},
	(* TimeConstrained prevents autoloading of heavy packages (e.g. SystemModeling)
	   that some System` symbols trigger on first evaluation. 2 s is generous;
	   a plain Attributes lookup takes microseconds. *)
	With[{attrs = Quiet[TimeConstrained[Attributes @@ heldSym, 2, {}]]},
		If[MemberQ[attrs, NumericFunction], AppendTo[$numericFunctionSyms, s]];
		If[MemberQ[attrs, Listable], AppendTo[$listableSyms, s]]
	]
	],
	{s, allSyms}
];
Print["  NumericFunction symbols: ", Length[$numericFunctionSyms]];
Print["  Listable symbols: ", Length[$listableSyms]];

(*
For NumericFunction symbols that still lack a return type: set _?NumericQ.
This is a guaranteed semantic property: NumericFunction means the function
returns a numeric value when all arguments are numeric.
*)
numericEnriched = 0;
Do[
	If[KeyExistsQ[results, sym],
	Module[{ovs = results[sym], updated = False},
		ovs = Map[Function[{ov},
		If[ov[[2]] === None,
			updated = True;
			{ov[[1]], "_?NumericQ"},
			ov
		]
		], ovs];
		If[updated,
		AssociateTo[results, sym -> ovs];
		numericEnriched++
		]
	],
	(* Symbol has no entry at all - create a generic one-arg numeric overload.
	   We use SyntaxInformation to determine the correct arity if available. *)
	Module[{minArgs = 1},
		Quiet[CheckAbort[TimeConstrained[
		With[{si = SyntaxInformation @@ ToExpression[sym, InputForm, HoldComplete]},
			If[si =!= {} && Head[si] === List,
			With[{ap = "ArgumentsPattern" /. si},
				If[ListQ[ap],
				minArgs = Length[Select[ap,
					!MatchQ[#, Verbatim[Optional][_, ___] | Verbatim[Optional][_] |
					Verbatim[BlankNullSequence][] | Verbatim[OptionsPattern][] |
					Verbatim[OptionsPattern][_]] &
				]]
				]
			]
			]
		],
		2, Null  (* skip on timeout - default minArgs=1 is safe *)
		], Null]];
		AssociateTo[results, sym -> {{Table[None, minArgs], "_?NumericQ"}}];
		numericEnriched++
	]
	],
	{sym, $numericFunctionSyms}
];
Print["  NumericFunction enriched ", numericEnriched, " symbols."];

(*
For Listable symbols: add a List-input overload that returns _List.
Listable means f[{a,b,c}] -> {f[a], f[b], f[c]}, so List in -> List out.
Only add this overload if no existing List-typed overload exists.
*)
listableEnriched = 0;
Do[
	If[KeyExistsQ[results, sym],
	Module[{ovs = results[sym], hasListOverload},
		hasListOverload = AnyTrue[ovs, Function[{ov},
		Length[ov[[1]]] >= 1 && ov[[1]][[1]] === "List"
		]];
		If[!hasListOverload && Length[ovs] > 0,
		(* Create a List-input overload mirroring the first overload's arity *)
		With[{baseOv = First[ovs]},
			If[Length[baseOv[[1]]] >= 1,
			Module[{listArgs = baseOv[[1]]},
				listArgs[[1]] = "List";
				AppendTo[ovs, {listArgs, "_List"}];
				AssociateTo[results, sym -> ovs];
				listableEnriched++
			]
			]
		]
		]
	]
	],
	{sym, $listableSyms}
];
Print["  Listable enriched ", listableEnriched, " symbols."];

(* ================================================================
── Part 4d: SyntaxInformation arity-based gap filling ──
   Use ArgumentsPattern to create overloads for symbols that have
   no DV or PU data but do have SyntaxInformation.
================================================================ *)

Print["Filling arity gaps from SyntaxInformation..."];

(*
Parse SyntaxInformation "ArgumentsPattern" to extract {minArgs, maxArgs}.
Returns Missing[] if the symbol has no SyntaxInformation.
*)
parseSIArity[patt_List] := Module[{required = 0, optional = 0, variadic = False},
	Do[
	Which[
		MatchQ[arg, Verbatim[OptionsPattern][] | Verbatim[OptionsPattern][_]],
			Null,   (* options don't count as positional args *)
		MatchQ[arg, Verbatim[BlankSequence][] |
			Verbatim[Pattern][_, Verbatim[BlankSequence][]]],
			variadic = True; required += 1,
		MatchQ[arg, Verbatim[BlankNullSequence][] |
			Verbatim[Pattern][_, Verbatim[BlankNullSequence][]]],
			variadic = True,
		MatchQ[arg, Verbatim[Optional][_, ___] | Verbatim[Optional][_]],
			optional++,
		True,
			required++
	],
	{arg, patt}
	];
	<|"MinArgs" -> required,
	"MaxArgs" -> If[variadic, Infinity, required + optional],
	"Variadic" -> variadic|>
]
parseSIArity[_] := Missing["NoPattern"]

siEnriched = 0;
Do[
	If[!KeyExistsQ[results, sym],
	(* Symbol has no entry yet - try SyntaxInformation.
	   TimeConstrained guards against autoloading of heavy packages that some
	   System` symbols trigger (e.g. SystemModeling, CloudObject, etc.). *)
	Quiet[CheckAbort[TimeConstrained[
		With[{si = SyntaxInformation @@ ToExpression[sym, InputForm, HoldComplete]},
		If[si =!= {} && Head[si] === List,
			With[{ap = "ArgumentsPattern" /. si},
			If[ListQ[ap],
				With[{arity = parseSIArity[ap]},
				If[AssociationQ[arity],
					Module[{ovs = {}},
					(* Create overloads for min to max arity (capped at min+3 for sanity) *)
					Do[
						AppendTo[ovs, {Table[None, n], None}],
						{n, arity["MinArgs"],
						Min[arity["MaxArgs"], arity["MinArgs"] + 3]}
					];
					If[ovs =!= {},
						AssociateTo[results, sym -> ovs];
						siEnriched++
					]
					]
				]
				]
			]
			]
		]
		],
		2, Null  (* skip on timeout - prevents SystemModeling/heavy-pkg autoloads *)
	], Null]]
	],
	{sym, allSyms}
];
Print["  SyntaxInformation added arity entries for ", siEnriched, " symbols."];

(*
Apply hardcoded overrides for well-known identity/passthrough functions and
functions with conditional return types.

"_[1]"  sentinel  - return type equals the type of the first argument.
					Echo[x] returns x; Identity[x] returns x.
*)
$identityOverrides = <|
(* Echo prints its first arg then returns it unchanged *)
"Echo" -> {	{{None},             "_[1]"},   (* Echo[expr]           *)
	{{None, None},       "_[1]"},   (* Echo[expr, label]    *)
	{{None, None, None}, "_[1]"}    (* Echo[expr, label, f] *)
},
(* Identity simply returns its argument *)
"Identity"        -> {{{None}, "_[1]"}},(* EchoTiming returns expr (timing is a side-effect list) *)
"EchoTiming"      -> {{{None}, "_[1]"}, {{None, None}, "_[1]"}},(* EchoEvaluation returns result of evaluating expr *)
"EchoEvaluation"  -> {{{None}, "_[1]"}, {{None, None}, "_[1]"}},(* If: condition + 1 - 2 value branches; return type unknown statically *)
"If" -> {	{{_?BooleanQ, None},             "_[2] | Null"},         (* If[cond, yes]              *)
	{{_?BooleanQ, None, None},       "_[2] | _[3]"},         (* If[cond, yes, no]          *)
	{{_?BooleanQ, None, None, None}, "_[2] | _[3] | _[4]"}   (* If[cond, yes, no, missing] *)
},
(* Which and Switch are variadic with alternating argument structure.
	Return-type inference is handled at runtime in inferPatternFromRHS /
	resolveCallReturnPattern by name, so no fixed-arity overload entries are
	stored here.  An empty list prevents auto-generated overloads (which would
	be incorrect and could cause false diagnostic positives) from being emitted. *)
"Which"  -> {},"Switch" -> {},
(* ── Error closures ── *)
"Enclose" -> {	{{None}, "_[1] | _Failure"},
	{{None, None}, "_[1] | _[2]"},
	{{None, None, None}, "_[1] | _[2]"}
},
"Confirm" -> {	{{None}, "_[1] | _Failure"},
	{{None, None}, "_[1] | _Failure"},
	{{None, None, None}, "_[1] | _Failure"}
},
"ConfirmMatch" -> {	{{None, None}, "_[1] | _Failure"},
	{{None, None, None}, "_[1] | _Failure"},
	{{None, None, None, None}, "_[1] | _Failure"}
},
"ConfirmBy" -> {	{{None, _?BooleanQ}, "_[1] | _Failure"},
	{{None, _?BooleanQ, None}, "_[1] | _Failure"}
},
"ConfirmAssert" -> {{{_?BooleanQ}, Null | _Failure}},"ConfirmQuiet" -> {	{{None}, "_[1] | _Failure"},
	{{None, None}, "_[1] | _Failure"}
},

(* ── Numeric type-preserving / type-promoting functions ──────────────────
	Convention for numeric promotion:
	_Real    input -> return _Real    (machine float in, float out)
	_Integer input -> return _Integer or _?NumericQ depending on function
	_Complex input -> return _Complex or _?NumericQ

	Common sentinels:
	_?NumericQ  - any numeric expression (exact or approximate, real or complex)
	_Real       - machine-precision Real
	_Integer    - exact Integer
*)

(* ── Trigonometric & hyperbolic functions (unary, Real->Real) ── *)
"Sin"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Cos"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Tan"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Cot"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Sec"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Csc"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"ArcSin"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"ArcCos"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"ArcTan"  -> {	{{None},       _?NumericQ},
	{{_Real},     _Real},
	{{_Complex},  _Complex},
	{{_Real,_Real}, _Real}   (* ArcTan[x, y] two-arg form *)
},
"ArcCot"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"ArcSec"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"ArcCsc"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Sinh"    -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Cosh"    -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Tanh"    -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Coth"    -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Sech"    -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Csch"    -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"ArcSinh" -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"ArcCosh" -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"ArcTanh" -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"ArcCoth" -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},
(* ── Exponential / logarithm ── *)
"Exp"   -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Log"   -> {	{{None},            _?NumericQ},
	{{_Real},          _Real},
	{{_Complex},       _Complex},
	{{_Real, _Real},  _Real},    (* Log[b, z] – base b, value z *)
	{{_Integer,_Real},_Real}
},
"Log2"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Log10" -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},
(* ── Powers / roots ── *)
"Sqrt"  -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Complex}},"Power" -> {	{{None, None},          _?NumericQ},
	{{_Real,_Real},       _Real},
	{{_Integer,_Integer}, _?NumericQ},   (* may be Rational for negative exponent *)
	{{_Complex,_Complex}, _Complex}
},
"Plus" -> {	{{__Integer}, _Integer},
	{{__Real},    _Real},
	{{__Complex}, _Complex},
	{{None},         _?NumericQ}
},
"Times" -> {	{{__Integer}, _Integer},
	{{__Real},    _Real},
	{{__Complex}, _Complex},
	{{None},         _?NumericQ}
},

(* ── Absolute value & sign ── *)
"Abs"  -> {	{{None},        _?NumericQ},
	{{_Real},      _Real},
	{{_Integer},   _Integer},
	{{_Complex},   _Real}    (* |a+bi| is always a non-negative Real *)
},
"Sign" -> {	{{None},        _?NumericQ},
	{{_Real},      _Integer},
	{{_Integer},   _Integer},
	{{_Complex},   _Complex}   (* Sign[1+2I] = (1+2I)/Abs[1+2I] *)
},
"Clip" -> {{{None}, _Real}, {{_Real}, _Real}, {{_Real,_List}, _Real}},
(* ── Complex number parts ── *)
"Re"        -> {{{None}, _?NumericQ}, {{_Real}, _Real}, {{_Complex}, _Real},  {{_Integer}, _Integer}},"Im"        -> {{{None}, _?NumericQ}, {{_Real}, _Integer},{{_Complex}, _Real}, {{_Integer}, _Integer}},"Conjugate" -> {{{None}, _?NumericQ}, {{_Real}, _Real},  {{_Complex}, _Complex},{{_Integer}, _Integer}},"Arg"       -> {{{None}, _?NumericQ}, {{_Real}, _Real},  {{_Complex}, _Real},  {{_Integer}, _Integer}},"Abs2"      -> {{{None}, _?NumericQ}, {{_Real}, _Real},  {{_Complex}, _Real}},
(* ── Rounding / integer extraction ── *)
"Floor"         -> {{{None}, _Integer}, {{None,None}, _?NumericQ}, {{_Real}, _Integer}, {{_Real,_Real}, _Real}},"Ceiling"       -> {{{None}, _Integer}, {{None,None}, _?NumericQ}, {{_Real}, _Integer}, {{_Real,_Real}, _Real}},"Round"         -> {{{None}, _Integer}, {{None,None}, _?NumericQ}, {{_Real}, _Integer}, {{_Real,_Real}, _Real}},"IntegerPart"   -> {{{None}, _Integer}, {{_Real}, _Integer}, {{_Complex}, _Complex}},"FractionalPart"-> {{{None}, _?NumericQ},{{_Real}, _Real},   {{_Integer}, _Integer}},"Chop"          -> {{{None}, _?NumericQ}, {{_Real}, _?NumericQ}},"Rationalize"   -> {{{None}, _?NumericQ}, {{_Real}, _Rational}, {{_Real,_Real}, _Rational}},"N"             -> {	{{None},          _Real},   (* N[Pi], N[3], N[1/2] *)
	{{None,_Integer},_Real}    (* N[Pi, 50] *)
},

(* ── Min / Max ── *)
"Min" -> {	{{__Integer}, _Integer},
	{{__Real},    _Real},
	{{_List},       _?NumericQ},
	{{None},         _?NumericQ}
},
"Max" -> {	{{__Integer}, _Integer},
	{{__Real},    _Real},
	{{_List},       _?NumericQ},
	{{None},         _?NumericQ}
},
"MinMax" -> {{{None}, _List}, {{_List}, _List}},
(* ── Arithmetic: integer-domain functions ── *)
"Mod"      -> {	{{_Integer,_Integer},         _Integer},
	{{_Integer,_Integer,_Integer},_Integer},
	{{_Real,_Real},               _Real}
},
"Quotient" -> {	{{_Integer,_Integer},         _Integer},
	{{_Integer,_Integer,_Integer},_Integer}
},
"GCD"  -> {{{__Integer}, _Integer}, {{None}, _Integer}},"LCM"  -> {{{__Integer}, _Integer}, {{None}, _Integer}},"Factorial"  -> {{{None}, _Integer}, {{_Integer}, _Integer}},"Factorial2" -> {{{None}, _Integer}, {{_Integer}, _Integer}},"Binomial"   -> {{{None,None}, _Integer}, {{_Integer,_Integer}, _Integer}},"Fibonacci"  -> {{{None}, _Integer}, {{_Integer}, _Integer}, {{None,None}, _?NumericQ}},"LucasL"     -> {{{None}, _Integer}, {{_Integer}, _Integer}},"Prime"      -> {{{None}, _Integer}, {{_Integer}, _Integer}},"PartitionsP"-> {{{None}, _Integer}, {{_Integer}, _Integer}},"EulerPhi"   -> {{{None}, _Integer}, {{_Integer}, _Integer}},"MoebiusMu"  -> {{{None}, _Integer}, {{_Integer}, _Integer}},"DivisorSigma" -> {{{None,None}, _Integer}, {{_Integer,_Integer}, _Integer}},"PrimeOmega" -> {{{None}, _Integer}, {{_Integer}, _Integer}},"PrimeNu"    -> {{{None}, _Integer}, {{_Integer}, _Integer}},"JacobiSymbol" -> {{{None,None}, _Integer}, {{_Integer,_Integer}, _Integer}},"KroneckerSymbol" -> {{{None,None}, _Integer}, {{_Integer,_Integer}, _Integer}},"BitAnd"     -> {{{__Integer}, _Integer}, {{None}, _Integer}},"BitOr"      -> {{{__Integer}, _Integer}, {{None}, _Integer}},"BitXor"     -> {{{__Integer}, _Integer}, {{None}, _Integer}},"BitNot"     -> {{{None}, _Integer}, {{_Integer}, _Integer}},"BitShiftLeft"  -> {{{None,None}, _Integer}, {{_Integer,_Integer}, _Integer}},"BitShiftRight" -> {{{None,None}, _Integer}, {{_Integer,_Integer}, _Integer}},
(* ── Linear algebra ── *)
"Det"         -> {{{None}, _?NumericQ}, {{_List}, _?NumericQ}},"Tr"          -> {{{None}, _?NumericQ}, {{_List}, _?NumericQ}},"MatrixRank"  -> {{{None}, _Integer},   {{_List}, _Integer}},"Norm"        -> {{{None}, _?NumericQ}, {{_List}, _Real}, {{_List,_Integer}, _Real}},"Eigenvalues" -> {{{None}, _List},      {{_List}, _List}},"Eigenvectors"-> {{{None}, _List},      {{_List}, _List}},"Inverse"     -> {{{None}, _List},      {{_List}, _List}},"LinearSolve" -> {{{None,None}, _List}, {{_List,_List}, _List}},"NullSpace"   -> {{{None}, _List},      {{_List}, _List}},"RowReduce"   -> {{{None}, _List},      {{_List}, _List}},"Cross"       -> {{{None,None}, _List}, {{_List,_List}, _List}},"Dot"         -> {{{None,None}, _?NumericQ}, {{_List,_List}, _List}},"Outer"       -> {{{None,None,None}, _List}},"Inner"       -> {{{None,None,None,None}, _List}},
(* ── Aggregation: list -> scalar ── *)
"Total"  -> {{{None}, _?NumericQ}, {{_List}, _?NumericQ}},"Mean"   -> {{{None}, _?NumericQ}, {{_List}, _?NumericQ}},"Median" -> {{{None}, _?NumericQ}, {{_List}, _?NumericQ}},"Variance"         -> {{{None}, _Real}, {{_List}, _Real}},"StandardDeviation"-> {{{None}, _Real}, {{_List}, _Real}},"Covariance"       -> {{{None,None}, _List}, {{_List,_List}, _List}},"Accumulate"       -> {{{None}, _List}, {{_List}, _List}},"Differences"      -> {{{None}, _List}, {{_List}, _List}, {{None,_Integer}, _List}},"Ratios"           -> {{{None}, _List}, {{_List}, _List}},"CumulativeSum"    -> {{{None}, _List}},
(* ── Numeric solving / optimization (always return list of Rules or lists) ── *)
"Solve"        -> {{{None,None}, _List}},"NSolve"       -> {{{None,None}, _List}},"FindRoot"     -> {{{None,None}, _List}},"Reduce"       -> {{{None,None}, _?BooleanQ}},"NMinimize"    -> {{{None,None}, _List}},   (* {minval, {x->v,...}} *)"NMaximize"    -> {{{None,None}, _List}},"Minimize"     -> {{{None,None}, _List}},"Maximize"     -> {{{None,None}, _List}},"FindMinimum"  -> {{{None,None}, _List}},"FindMaximum"  -> {{{None,None}, _List}},
(* ── Random number generation ── *)
"RandomReal"    -> {	{{},             _Real},
	{{None},         _Real},
	{{None,_Integer},_List},
	{{None,_List},   _List}
},
"RandomInteger" -> {	{{},             _Integer},
	{{None},         _Integer},
	{{None,_Integer},_List},
	{{None,_List},   _List}
},
"RandomComplex" -> {	{{},             _Complex},
	{{None},         _Complex},
	{{None,_Integer},_List},
	{{None,_List},   _List}
},
"RandomChoice"  -> {{{None}, _?True}, {{_List}, None}},  (* type depends on list elements *)
(* ── Numeric predicates (guaranteed boolean) ── *)
"PrimeQ"       -> {{{None}, _?BooleanQ}, {{_Integer}, _?BooleanQ}, {{_Integer,_Integer}, _?BooleanQ}},"EvenQ"        -> {{{None}, _?BooleanQ}},"OddQ"         -> {{{None}, _?BooleanQ}},"IntegerQ"     -> {{{None}, _?BooleanQ}},"NumberQ"      -> {{{None}, _?BooleanQ}},"NumericQ"     -> {{{None}, _?BooleanQ}},"ExactNumberQ" -> {{{None}, _?BooleanQ}},"InexactNumberQ"-> {{{None}, _?BooleanQ}},"RealValuedNumberQ" -> {{{None}, _?BooleanQ}},"PositiveQ"    -> {{{None}, _?BooleanQ}},"NonNegativeQ" -> {{{None}, _?BooleanQ}},"NegativeQ"    -> {{{None}, _?BooleanQ}},"NonPositiveQ" -> {{{None}, _?BooleanQ}},
(* ── Comparison operators (always return boolean) ── *)
"Greater"       -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"GreaterEqual"  -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"Less"          -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"LessEqual"     -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"Equal"         -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"Unequal"       -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"SameQ"         -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"UnsameQ"       -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"OrderedQ"      -> {{{None}, _?BooleanQ}},"MemberQ"       -> {{{None, None}, _?BooleanQ}},"FreeQ"         -> {{{None, None}, _?BooleanQ}},"SubsetQ"       -> {{{None, None}, _?BooleanQ}},"ContainsAll"   -> {{{None, None}, _?BooleanQ}},"ContainsAny"   -> {{{None, None}, _?BooleanQ}},"ContainsNone"  -> {{{None, None}, _?BooleanQ}},"MatchQ"        -> {{{None, None}, _?BooleanQ}},"StringMatchQ"  -> {{{None, None}, _?BooleanQ}},"StringContainsQ"-> {{{None, None}, _?BooleanQ}},"StringStartsQ" -> {{{None, None}, _?BooleanQ}},"StringEndsQ"   -> {{{None, None}, _?BooleanQ}},"ArrayQ"        -> {{{None}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"MatrixQ"       -> {{{None}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"VectorQ"       -> {{{None}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"SquareMatrixQ" -> {{{None}, _?BooleanQ}},"SymmetricMatrixQ" -> {{{None}, _?BooleanQ}},"HermitianMatrixQ" -> {{{None}, _?BooleanQ}},"DiagonalMatrixQ"  -> {{{None}, _?BooleanQ}},
(* ── Logical operators (always return boolean) ── *)
"And"     -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"Or"      -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"Not"     -> {{{None}, _?BooleanQ}},"Xor"     -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"Nand"    -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"Nor"     -> {{{__}, _?BooleanQ}, {{None, None}, _?BooleanQ}},"Implies" -> {{{None, None}, _?BooleanQ}},"TrueQ"   -> {{{None}, _?BooleanQ}},"BooleanQ"-> {{{None}, _?BooleanQ}},
(* ═══ Auto-generated entries from systematic function research ═══ *)
"Audio" -> {  {{_List, _Integer}, _Audio},
  {{_List, None},      _Audio},
  {{None},              _Audio},
  {{None, None},        _Audio},
  {{None, None, None},        _Audio},
  {{None, None, None, None},  _Audio}
},
"AudioAmplify" -> {  {{_Audio, None}, _Audio},
  {{_Video, None}, _Video}
},
"AudioAnnotate" -> {  {{_Audio, None},    _Audio},
  {{_Audio, _Rule},  _Audio}
},
"AudioAnnotationLookup" -> {  {{_Audio},              _List},
  {{_Audio, None},        _List},
  {{_Audio, _Rule},      _List},
  {{_Audio, _Rule, None},_List}
},
"AudioBlockMap" -> {  {{None, _Audio, None},       None},
  {{None, _Audio, _List},     None},
  {{None, _Audio, _Quantity}, None}
},
"AudioCapture" -> {  {{},      _Audio},
  {{None},  _Audio}
},
"AudioChannelCombine" -> {  {{_List}, _Audio}
},
"AudioChannelMix" -> {  {{_Audio},        _Audio},
  {{_Audio, None},  _Audio},
  {{_Video, None},  _Video}
},
"AudioChannels" -> {  {{_Audio}, _Integer},
  {{_Video}, _Integer}
},
"AudioChannelSeparate" -> {  {{_Audio}, _List}
},
"AudioData" -> {  {{_Audio},        _List},
  {{_Audio, None},  _List}
},
"AudioDelay" -> {  {{_Audio, None},        _Audio},
  {{_Audio, None, None},  _Audio}
},
"AudioDelete" -> {  {{_Audio, None},   _Audio},
  {{_Audio, _List}, _Audio}
},
"AudioDistance" -> {  {{_Audio, _Audio},        _Real},
  {{_Audio, _Audio, None},  _Real}
},
"AudioFade" -> {  {{_Audio},         _Audio},
  {{_Audio, None},   _Audio},
  {{_Audio, _List}, _Audio}
},
"AudioFrequencyShift" -> {  {{_Audio, None},        _Audio},
  {{_Audio, None, None},  _Audio}
},
"AudioGenerator" -> {  {{},               _Audio},
  {{_String},       _Audio},
  {{_String, None}, _Audio},
  {{None},           _Audio},
  {{None, None},     _Audio}
},
"AudioIdentify" -> {  {{_Audio},        None},
  {{_Audio, None},  None}
},
"AudioInsert" -> {  {{_Audio, _Audio, None}, _Audio},
  {{_Audio, None,   None},  _Audio}
},
"AudioIntervals" -> {  {{_Audio, None},        _List},
  {{_Audio, None, None},  _List}
},
"AudioJoin" -> {  {{__Audio}, _Audio},
  {{_List},     _Audio}
},
"AudioLength" -> {  {{_Audio}, _Quantity},
  {{_Video}, _Quantity}
},
"AudioLocalMeasurements" -> {  {{_Audio, _String},  _Association},
  {{_Audio, _List},    _List},
  {{_Audio, None},      _Association}
},
"AudioLoudness" -> {  {{_Audio},        _Real},
  {{_Audio, None},  _Real}
},
"AudioMeasurements" -> {  {{_Audio, _String},  None},
  {{_Audio, _List},    _Association},
  {{_Audio, None},      None}
},
"AudioNormalize" -> {  {{_Audio},        _Audio},
  {{_Audio, None},  _Audio}
},
"AudioOverlay" -> {  {{_List},        _Audio},
  {{_List, None},  _Audio}
},
"AudioPad" -> {  {{_Audio},         _Audio},
  {{_Audio, None},   _Audio},
  {{_Audio, _List}, _Audio}
},
"AudioPan" -> {  {{_Audio},        _Audio},
  {{_Audio, None},  _Audio}
},
"AudioPartition" -> {  {{_Audio, None},        _List},
  {{_Audio, None, None},  _List},
  {{_Audio, _List},      _List}
},
"AudioPause" -> {  {{},      None},
  {{None},  None}
},
"AudioPitchShift" -> {  {{_Audio, None},        _Audio},
  {{_Audio, None, None},  _Audio}
},
"AudioPlay" -> {  {{_Audio},        None},
  {{_Audio, None},  None},
  {{None},           None}
},
"AudioRecord" -> {  {{},      _Audio},
  {{None},  _Audio}
},
"AudioReplace" -> {  {{_Audio, _Rule},  _Audio},
  {{_Audio, _List},  _Audio},
  {{_Audio, None},    _Audio}
},
"AudioResample" -> {  {{_Audio, _Integer}, _Audio},
  {{_Audio, None},      _Audio}
},
"AudioReverb" -> {  {{_Audio},        _Audio},
  {{_Audio, None},  _Audio}
},
"AudioReverse" -> {  {{_Audio}, _Audio}
},
"AudioSampleRate" -> {  {{_Audio}, _Integer},
  {{_Video}, _Integer}
},
"AudioSpectralMap" -> {  {{None, _Audio},        _Audio},
  {{None, _Audio, None},  _Audio}
},
"AudioSpectralTransformation" -> {  {{None, _Audio},        _Audio},
  {{None, _Audio, None},  _Audio},
  {{None, _Video},        _Video}
},
"AudioSplit" -> {  {{_Audio, None},   _List},
  {{_Audio, _List}, _List}
},
"AudioStop" -> {  {{},      None},
  {{None},  None}
},
"AudioStream" -> {  {{None}, None}
},
"AudioStreams" -> {  {{},               _List},
  {{_Audio},        _List},
  {{_Audio, None},  _List}
},
"AudioTimeStretch" -> {  {{_Audio, None}, _Audio},
  {{_Video, None}, _Video}
},
"AudioTrackApply" -> {  {{None, _Video},        _Audio},
  {{None, _Video, None},  _Audio}
},
"AudioTrim" -> {  {{_Audio},         _Audio},
  {{_Audio, None},   _Audio},
  {{_Audio, _List}, _List}
},
"AudioType" -> {  {{_Audio}, _String}
},
"Image" -> {  {{_List}, _Image},
  {{None}, _Image},
  {{None, None}, _Image}
},
"Image3D" -> {  {{_List}, _Image},
  {{None}, _Image},
  {{None, None}, _Image}
},
"Image3DProjection" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"Image3DSlices" -> {  {{_Image}, _List},
  {{_Image, _Integer}, _Image},
  {{_Image, _List}, _List},
  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"ImageAccumulate" -> {  {{_Image}, _Image}
},
"ImageAdd" -> {  {{_Image, None}, _Image},
  {{_Image, _Image}, _Image},
  {{_Image, __}, _Image}
},
"ImageAdjust" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image},
  {{_Image, None, None, None}, _Image}
},
"ImageAlign" -> {  {{_Image, _Image}, _Image},
  {{_Image, _List}, _List},
  {{_List}, _Image}
},
"ImageApply" -> {  {{None, _Image}, _Image},
  {{None, _List}, _Image}
},
"ImageApplyIndexed" -> {  {{None, _Image}, _Image},
  {{None, _List}, _Image}
},
"ImageAspectRatio" -> {  {{_Image}, _Real},
  {{None}, _Real}
},
"ImageAssemble" -> {  {{_List}, _Image},
  {{_List, None}, _Image}
},
"ImageAugmentationLayer" -> {  {{_List}, None},
  {{None}, None}
},
"ImageBoundingBoxes" -> {  {{_Image}, _List},
  {{_Image, None}, _List}
},
"ImageCapture" -> {  {{}, _Image}
},
"ImageCases" -> {  {{_Image}, _List},
  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"ImageChannels" -> {  {{_Image}, _Integer},
  {{None}, _Integer}
},
"ImageClip" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImageCollage" -> {  {{_List}, _Image},
  {{_List, None}, _Image},
  {{_List, None, None}, _Image}
},
"ImageColorSpace" -> {  {{_Image}, _String},
  {{None}, _String}
},
"ImageCompose" -> {  {{_Image, _Image}, _Image},
  {{_Image, _List}, _Image},
  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image},
  {{_Image, None, None, None}, _Image}
},
"ImageContents" -> {  {{_Image}, _List},
  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"ImageConvolve" -> {  {{_Image, None}, _Image}
},
"ImageCooccurrence" -> {  {{_Image, _Integer}, _List},
  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"ImageCorners" -> {  {{_Image}, _List},
  {{_Image, None}, _List},
  {{_Image, None, None}, _List},
  {{_Image, None, None, None}, _List}
},
"ImageCorrelate" -> {  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImageCorrespondingPoints" -> {  {{_Image, _Image}, _List}
},
"ImageCrop" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImageData" -> {  {{_Image}, _List},
  {{_Image, None}, _List}
},
"ImageDeconvolve" -> {  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImageDemosaic" -> {  {{_Image, None}, _Image}
},
"ImageDifference" -> {  {{_Image, _Image}, _Image},
  {{_Image, None}, _Image}
},
"ImageDimensions" -> {  {{_Image}, _List},
  {{None}, _List}
},
"ImageDisplacements" -> {  {{_List}, _List},
  {{_List, None}, _List},
  {{None}, _List}
},
"ImageDistance" -> {  {{_Image, _Image}, _Real},
  {{_Image, _Image, None}, _Real},
  {{_Image, _Image, None, None}, _Real}
},
"ImageEffect" -> {  {{_Image, _String}, _Image},
  {{_Image, _List}, _Image},
  {{_Image, None}, _Image}
},
"ImageExposureCombine" -> {  {{_List}, _Image},
  {{_List, None}, _Image}
},
"ImageFeatureTrack" -> {  {{_List}, _List},
  {{_List, None}, _List},
  {{None}, _List},
  {{None, None}, _List}
},
"ImageFileApply" -> {  {{None, None, None}, None}
},
"ImageFileFilter" -> {  {{None, None, None, None}, None}
},
"ImageFileScan" -> {  {{None, None}, None}
},
"ImageFilter" -> {  {{None, _Image, None}, _Image},
  {{None, _Image, None, None}, _Image}
},
"ImageFocusCombine" -> {  {{_List}, _Image},
  {{_List, None}, _Image}
},
"ImageForestingComponents" -> {  {{_Image}, _List},
  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"ImageForwardTransformation" -> {  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImageGraphics" -> {  {{_Image}, _Graphics},
  {{_Image, None}, _Graphics}
},
"ImageHistogram" -> {  {{_Image}, _Graphics},
  {{_Image, None}, _Graphics},
  {{_Image, None, None}, _Graphics}
},
"ImageIdentify" -> {  {{_Image}, None},
  {{_Image, None}, None},
  {{_Image, None, _Integer}, _List},
  {{_Image, None, _Integer, None}, _List}
},
"ImageKeypoints" -> {  {{_Image}, _List},
  {{_Image, None}, _List}
},
"ImageLevels" -> {  {{_Image}, _List},
  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"ImageLines" -> {  {{_Image}, _List},
  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"ImageMarker" -> {  {{None}, None},
  {{None, None}, None}
},
"ImageMeasurements" -> {  {{_Image, _String}, _Real},
  {{_Image, _List}, _Association},
  {{_Image, None}, None},
  {{_List, None}, _List}
},
"ImageMesh" -> {  {{_Image}, None}
},
"ImageMultiply" -> {  {{_Image, None}, _Image},
  {{_Image, _Image}, _Image},
  {{_Image, __}, _Image}
},
"ImagePad" -> {  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImagePartition" -> {  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"ImagePeriodogram" -> {  {{_Image}, _Graphics},
  {{_Image, None}, _Graphics},
  {{_Image, None, None}, _Graphics}
},
"ImagePerspectiveTransformation" -> {  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImagePosition" -> {  {{_Image}, _List},
  {{_Image, None}, _List}
},
"ImagePyramid" -> {  {{_Image}, None},
  {{_Image, None}, None},
  {{_Image, None, None}, None},
  {{_Image, None, None, None}, None}
},
"ImagePyramidApply" -> {  {{None, None}, None},
  {{None, _List}, None}
},
"ImageRecolor" -> {  {{_Image, _Rule}, _Image},
  {{_Image, _List}, _Image},
  {{_Image, None}, _Image}
},
"ImageReflect" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image}
},
"ImageResize" -> {  {{_Image, _Integer}, _Image},
  {{_Image, _List}, _Image},
  {{_Image, None}, _Image}
},
"ImageRestyle" -> {  {{_Image, _Image}, _Image},
  {{_Image, _List}, _Image},
  {{_Image, None}, _Image}
},
"ImageRotate" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImageSaliencyFilter" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image}
},
"ImageScaled" -> {  {{_List}, None},
  {{_List, _List}, None}
},
"ImageScan" -> {  {{None, _Image}, None}
},
"ImageSegmentationComponents" -> {  {{_Image}, _List},
  {{_Image, None}, _List}
},
"ImageSegmentationFilter" -> {  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImageStitch" -> {  {{_List}, _Image},
  {{_List, None}, _Image}
},
"ImageSubtract" -> {  {{_Image, None}, _Image},
  {{_Image, _Image}, _Image},
  {{_Image, __}, _Image}
},
"ImageSynthesize" -> {  {{_String}, _Image},
  {{_Image}, _Image},
  {{None}, _Image},
  {{_String, _Integer}, _List},
  {{_Image, _Integer}, _List},
  {{None, _Integer}, _List}
},
"ImageTake" -> {  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image},
  {{_Image, None, None, None}, _Image}
},
"ImageTransformation" -> {  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ImageTrim" -> {  {{_Image, None}, _Image},
  {{_Image, _List}, _List},
  {{_Image, None, None}, _Image}
},
"ImageType" -> {  {{_Image}, _String},
  {{None}, _String}
},
"ImageValue" -> {  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"ImageValuePositions" -> {  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"Video" -> {  {{None},       _Video},   (* Video["file.mp4"] / Video[File[...]] *)
  {{None, None}, _Video}    (* Video[source, {w,h}] – with explicit size *)
},
"VideoCapture" -> {  {{},     _Video},   (* VideoCapture[] – default device *)
  {{None}, _Video}    (* VideoCapture[source] *)
},
"VideoCombine" -> {  {{_List},       _Video},   (* VideoCombine[{v1, v2, ...}] *)
  {{_List, None}, _Video}    (* VideoCombine[{v1, v2, ...}, opts] *)
},
"VideoDelete" -> {  {{_Video, None},   _Video},   (* VideoDelete[video, t] *)
  {{_Video, _List}, _Video}    (* VideoDelete[video, {t1, t2}] *)
},
"VideoDisplacements" -> {  {{_Video},  _List},   (* VideoDisplacements[video] *)
  {{_List},   _List}    (* VideoDisplacements[{v1, v2, ...}] *)
},
"VideoEncyclopedia" -> {  {{},          None},   (* VideoEncyclopedia[] – browse *)
  {{None},      None},   (* VideoEncyclopedia["query"] *)
  {{None, None},None}    (* VideoEncyclopedia["query", props] *)
},
"VideoExtractFrames" -> {  {{_Video},          _List},   (* VideoExtractFrames[video] – all frames *)
  {{_Video, None},    _List},   (* VideoExtractFrames[video, t] *)
  {{_Video, _List},  _List}    (* VideoExtractFrames[video, {t1, t2}] *)
},
"VideoFrameList" -> {  {{_Video},            _List},   (* VideoFrameList[video] – all frames *)
  {{_Video, _Integer}, _List},   (* VideoFrameList[video, n] – n frames *)
  {{_Video, None},      _List}    (* VideoFrameList[video, spec] *)
},
"VideoFrameMap" -> {  {{None, _Video},       _Video},   (* VideoFrameMap[f, video] *)
  {{None, _Video, None}, _Video}    (* VideoFrameMap[f, video, opts] *)
},
"VideoFrames" -> {  {{_Video}, _Integer},   (* VideoFrames[video] – frame count *)
  {{None},    _Integer}    (* VideoFrames[any source] *)
},
"VideoGenerator" -> {  {{_String},             _Video},   (* VideoGenerator["Noise"] *)
  {{_String, None},       _Video},   (* VideoGenerator["type", duration] *)
  {{_String, None, None}, _Video}    (* VideoGenerator["type", duration, size] *)
},
"VideoIdentify" -> {  {{_Video},       None},   (* VideoIdentify[video] *)
  {{_Video, None}, None}    (* VideoIdentify[video, props] *)
},
"VideoInsert" -> {  {{_Video, _Video, None}, _Video},   (* VideoInsert[v, clip, t] *)
  {{_Video, None, None},    _Video}    (* VideoInsert[v, clips, {t1,...}] *)
},
"VideoJoin" -> {  {{__Video}, _Video},   (* VideoJoin[v1, v2, ...] – variadic *)
  {{_List},     _Video}    (* VideoJoin[{v1, v2, ...}] – from list *)
},
"VideoMap" -> {  {{None, _Video},       _Video},   (* VideoMap[f, video] *)
  {{None, _Video, None}, _Video}    (* VideoMap[f, video, opts] *)
},
"VideoMapTimeSeries" -> {  {{None, _Video},       None},   (* VideoMapTimeSeries[f, video] *)
  {{None, _Video, None}, None}    (* VideoMapTimeSeries[f, video, opts] *)
},
"VideoMeasurements" -> {  {{_Video, None},        None},   (* VideoMeasurements[video, prop] *)
  {{_Video, None, None},  None}    (* VideoMeasurements[video, prop, t] *)
},
"VideoOverlay" -> {  {{_List},       _Video},   (* VideoOverlay[{v1, v2, ...}] *)
  {{_List, None}, _Video}    (* VideoOverlay[{v1, v2, ...}, opts] *)
},
"VideoPad" -> {  {{_Video, None},       _Video},   (* VideoPad[video, duration] *)
  {{_Video, _List},     _Video},   (* VideoPad[video, {before, after}] *)
  {{_Video, None, None}, _Video}    (* VideoPad[video, spec, opts] *)
},
"VideoPartition" -> {  {{_Video, None},        _List},   (* VideoPartition[video, dur] *)
  {{_Video, None, None},  _List}    (* VideoPartition[video, dur, offset] *)
},
"VideoPause" -> {  {{},     None},   (* VideoPause[] – pause current playback *)
  {{None}, None}    (* VideoPause[vid] *)
},
"VideoPlay" -> {  {{_Video},       None},   (* VideoPlay[video] *)
  {{_Video, None}, None}    (* VideoPlay[video, opts] *)
},
"VideoRecord" -> {  {{},           _Video},   (* VideoRecord[] – default device, interactive *)
  {{None},       _Video},   (* VideoRecord[duration] *)
  {{None, None}, _Video}    (* VideoRecord[source, duration] *)
},
"VideoReplace" -> {  {{_Video, _Rule}, _Video},   (* VideoReplace[video, {t1,t2}->clip] *)
  {{_Video, None},   _Video}    (* VideoReplace[video, rules] *)
},
"VideoSplit" -> {  {{_Video, None},   _List},   (* VideoSplit[video, t] *)
  {{_Video, _List}, _List}    (* VideoSplit[video, {t1, t2, ...}] *)
},
"VideoStop" -> {  {{},     None},   (* VideoStop[] – stop current playback *)
  {{None}, None}    (* VideoStop[vid] *)
},
"VideoStream" -> {  {{},           None},   (* VideoStream[] – current stream handle *)
  {{None},       None},   (* VideoStream[source] *)
  {{None, None}, None}    (* VideoStream[source, opts] *)
},
"VideoStreams" -> {  {{},     _List},   (* VideoStreams[] – all streams *)
  {{None}, _List}    (* VideoStreams[filter] *)
},
"VideoTrim" -> {  {{_Video, _List}, _Video},   (* VideoTrim[video, {t1, t2}] *)
  {{_Video, None},   _Video}    (* VideoTrim[video, t] *)
},
"Graph" -> {  {{_List},        _Graph},   (* Graph[{e1,e2,...}] edge list *)
  {{_List,_List}, _Graph},   (* Graph[{v1,...},{e1,...}] vertices+edges *)
  {{None},          _Graph}    (* Graph[data] from adjacency matrix, rules, etc. *)
},
"GraphAssortativity" -> {  {{_Graph},         _Real},  (* by degree *)
  {{_Graph,_String},_Real},  (* by vertex property *)
  {{_Graph,_List},  _Real},  (* by vertex partition *)
  {{_Graph,_Rule},  _Real}   (* by explicit vertex -> value map *)
},
"GraphDiameter" -> {  {{_Graph}, _?NumericQ}
},
"GraphRadius" -> {  {{_Graph}, _?NumericQ}
},
"GraphReciprocity" -> {  {{_Graph}, _Real}
},
"GraphAutomorphismGroup" -> {  {{_Graph}, None}
},
"GraphCenter" -> {  {{_Graph}, _List}
},
"GraphHub" -> {  {{_Graph}, _List}
},
"GraphPeriphery" -> {  {{_Graph}, _List}
},
"GraphEmbedding" -> {  {{_Graph},         _List},  (* default layout *)
  {{_Graph,None},    _List},  (* named embedding method *)
  {{_Graph,None,None},_List}  (* method + dimension *)
},
"GraphDistanceMatrix" -> {  {{_Graph},       _List},    (* all-pairs distance matrix *)
  {{_Graph,_List},_List}     (* row subset for specified vertices *)
},
"GraphDistance" -> {  {{_Graph,None,None},       _?NumericQ},  (* g, s, t -> single distance *)
  {{_Graph,None},             _List},       (* g, s   -> distances to all verts *)
  {{_Graph,_List,_List},    _List}        (* g, {s1,...},{t1,...} -> distances *)
},
"GraphAdjacencyMatrix" -> {  {{_Graph}, _SparseArray}
},
"GraphIncidenceMatrix" -> {  {{_Graph}, _SparseArray}
},
"GraphWeightedAdjacencyMatrix" -> {  {{_Graph}, _SparseArray}
},
"GraphSignedAdjacencyMatrix" -> {  {{_Graph}, _SparseArray}
},
"GraphComplement" -> {  {{_Graph}, _Graph}
},
"GraphDifference" -> {  {{_Graph,_Graph}, _Graph}
},
"GraphDisjointUnion" -> {  {{_Graph,_Graph}, _Graph},
  {{__Graph},       _Graph}
},
"GraphIntersection" -> {  {{_Graph,_Graph}, _Graph},
  {{__Graph},       _Graph}
},
"GraphJoin" -> {  {{_Graph,_Graph}, _Graph}
},
"GraphPower" -> {  {{_Graph,_Integer}, _Graph}
},
"GraphProduct" -> {  {{_Graph,_Graph},         _Graph},  (* Cartesian product by default *)
  {{_Graph,_Graph,_String},_Graph}   (* named product type *)
},
"GraphUnion" -> {  {{_Graph,_Graph}, _Graph},
  {{__Graph},       _Graph}
},
"GraphPlot" -> {  {{_Graph}, _Graphics},  (* from Graph object *)
  {{_List},  _Graphics}   (* from edge list or adjacency matrix *)
},
"GraphPlot3D" -> {  {{_Graph}, _Graphics3D},
  {{_List},  _Graphics3D}
},
"GraphPropertyValue" -> {  {{_Graph,None}, None},  (* g, propName -> value *)
  {{None,None},    None}   (* {g,v/e}, propName -> value *)
},
"GraphQ" -> {  {{None}, _?BooleanQ}
},
"GraphLayout" -> {},"VertexAdd" -> {  {{_Graph,None},   _Graph},  (* single vertex *)
  {{_Graph,_List}, _Graph}   (* list of vertices *)
},
"VertexContract" -> {  {{_Graph,_List}, _Graph}   (* merge vertex set into one *)
},
"VertexDelete" -> {  {{_Graph,None},   _Graph},
  {{_Graph,_List}, _Graph}
},
"VertexReplace" -> {  {{_Graph,_List}, _Graph}   (* g, {v1->w1, v2->w2, ...} *)
},
"VertexConnectivity" -> {  {{_Graph},           _Integer},  (* global connectivity *)
  {{_Graph,None,None}, _Integer}   (* s-t vertex connectivity *)
},
"VertexCorrelationSimilarity" -> {  {{_Graph,None,None}, _Real}
},
"VertexCosineSimilarity" -> {  {{_Graph,None,None}, _Real}
},
"VertexDiceSimilarity" -> {  {{_Graph,None,None}, _Real}
},
"VertexJaccardSimilarity" -> {  {{_Graph,None,None}, _Real}
},
"VertexCount" -> {  {{_Graph},      _Integer},  (* all vertices *)
  {{_Graph,None}, _Integer}   (* matching pattern *)
},
"VertexCoverQ" -> {  {{_Graph,_List}, _?BooleanQ}
},
"VertexQ" -> {  {{_Graph,None}, _?BooleanQ}
},
"VertexDegree" -> {  {{_Graph},      _List},    (* {deg_v1, deg_v2, ...} *)
  {{_Graph,None}, _Integer}  (* degree of single vertex v *)
},
"VertexInDegree" -> {  {{_Graph},      _List},
  {{_Graph,None}, _Integer}
},
"VertexOutDegree" -> {  {{_Graph},      _List},
  {{_Graph,None}, _Integer}
},
"VertexEccentricity" -> {  {{_Graph},      _List},       (* eccentricity of every vertex *)
  {{_Graph,None}, _?NumericQ}   (* eccentricity of vertex v *)
},
"VertexIndex" -> {  {{_Graph,None}, _Integer}
},
"VertexInComponent" -> {  {{_Graph,None},            _List},  (* vertices with path TO v *)
  {{_Graph,_List},          _List},  (* with path to any of {v1,...} *)
  {{_Graph,None,_Integer},  _List},  (* within k steps *)
  {{_Graph,_List,_Integer},_List}
},
"VertexOutComponent" -> {  {{_Graph,None},            _List},  (* vertices reachable FROM v *)
  {{_Graph,_List},          _List},
  {{_Graph,None,_Integer},  _List},  (* within k steps *)
  {{_Graph,_List,_Integer},_List}
},
"VertexList" -> {  {{_Graph},      _List},   (* all vertices *)
  {{_Graph,None}, _List}    (* filtered by pattern *)
},
"EdgeAdd" -> {  {{_Graph,None},   _Graph},
  {{_Graph,_List}, _Graph}
},
"EdgeContract" -> {  {{_Graph,None},   _Graph},
  {{_Graph,_List}, _Graph}
},
"EdgeDelete" -> {  {{_Graph,None},   _Graph},
  {{_Graph,_List}, _Graph}
},
"EdgeBetweennessCentrality" -> {  {{_Graph}, _List}
},
"EdgeConnectivity" -> {  {{_Graph},           _Integer},  (* global *)
  {{_Graph,None,None}, _Integer}   (* s-t edge connectivity *)
},
"EdgeCount" -> {  {{_Graph},      _Integer},
  {{_Graph,None}, _Integer}  (* matching pattern *)
},
"EdgeCoverQ" -> {  {{_Graph,_List}, _?BooleanQ}
},
"EdgeQ" -> {  {{_Graph,None}, _?BooleanQ}
},
"EdgeWeightedGraphQ" -> {  {{_Graph}, _?BooleanQ}
},
"EdgeIndex" -> {  {{_Graph,None}, _Integer}  (* 1-based index into EdgeList *)
},
"EdgeList" -> {  {{_Graph},      _List},   (* all edges as DirectedEdge/UndirectedEdge *)
  {{_Graph,None}, _List}    (* filtered by pattern *)
},
"EdgeRules" -> {  {{_Graph}, _List}         (* all edges as Rule / TwoWayRule *)
},
"EdgeWeight" -> {},"DateDifference" -> {    {{None, None},       _Quantity},
    {{None, None, None}, _Quantity}
},
"DateFunction" -> {    {{None}, None}
},
"DateHistogram" -> {    {{None},       _Graphics},
    {{None, None}, _Graphics}    (* explicit granularity / bin spec *)
},
"DateInterval" -> {    {{None}, None}
},
"DateList" -> {    {{},     _List},    (* current date/time *)
    {{None}, _List}     (* date spec -> 6-element list *)
},
"DateListLogPlot" -> {    {{None},       _Graphics},
    {{None, None}, _Graphics}
},
"DateListPlot" -> {    {{None},       _Graphics},
    {{None, None}, _Graphics}
},
"DateObject" -> {    {{},           None},    (* current date/time *)
    {{None},       None},    (* date spec *)
    {{None, None}, None}     (* date spec + granularity *)
},
"DateOverlapsQ" -> {    {{None, None}, _?BooleanQ}
},
"DatePartition" -> {    {{None, None},       _List},
    {{None, None, None}, _List}    (* with offset *)
},
"DatePlus" -> {    {{None, None},       None},    (* date + n  or  date + {n, unit} *)
    {{None, None, None}, None}     (* date + list of increments *)
},
"DateRange" -> {    {{None, None},       _List},
    {{None, None, None}, _List}
},
"DateTicksFormat" -> {    {{None}, None}
},
"DateValue" -> {    {{None},         _Integer},    (* single unit from current date/time *)
    {{None, None},   _Integer},    (* date spec + single unit *)
    {{None, _List}, _List},       (* date spec + {unit, unit, ...} *)
    {{None, None, None}, _Integer} (* date spec + unit + calendar system *)
},
"DateWithinQ" -> {    {{None, None}, _?BooleanQ}
},
"TimeDirectionQ" -> {    {{None}, _?BooleanQ}
},
"TimeGoes" -> {    {{None}, None}
},
"TimeInterval" -> {    {{None}, None}
},
"TimeIntervalIntersection" -> {    {{None, None}, None},
    {{__},      None}
},
"TimeIntervalMemberQ" -> {    {{None, None}, _?BooleanQ}
},
"TimeIntervalUnion" -> {    {{None, None}, None},
    {{__},      None}
},
"TimeObject" -> {    {{},           None},
    {{None},       None},
    {{None, None}, None}
},
"TimeSeries" -> {    {{None},             None},
    {{None, None},       None},    (* values + timestamp list or {tmin, dt} spec *)
    {{None, None, None}, None}     (* values + tspec + metadata *)
},
"TimeSeriesAggregate" -> {    {{None, None},       None},
    {{None, None, None}, None}     (* with explicit aggregation function *)
},
"TimeSeriesForecast" -> {    {{None},       None},
    {{None, None}, None}
},
"TimeSeriesInsert" -> {    {{None, None}, None}
},
"TimeSeriesInvertibility" -> {    {{None}, _?BooleanQ}
},
"TimeSeriesMap" -> {    {{None, None}, None}
},
"TimeSeriesMapThread" -> {    {{None, None}, None}
},
"TimeSeriesMappedQ" -> {    {{None, None}, _?BooleanQ}
},
"TimeSeriesMean" -> {    {{None}, _?NumericQ}
},
"TimeSeriesModelFit" -> {    {{None},       None},
    {{None, None}, None}
},
"TimeSeriesResampled" -> {    {{None, None},       None},
    {{None, None, None}, None}
},
"TimeSeriesResample" -> {    {{None},       None},
    {{None, None}, None}
},
"TimeSeriesRescale" -> {    {{None},       None},
    {{None, None}, None}    (* with explicit target range *)
},
"TimeSeriesShift" -> {    {{None, None}, None}
},
"TimeSeriesSlice" -> {    {{None, None}, None}
},
"TimeSeriesThread" -> {    {{None},      None},    (* list of time series *)
    {{_List},    None}
},
"TimeSeriesWindow" -> {    {{None, None}, None}
},
"TimeUsed" -> {    {{}, _Real}
},
"TimeValue" -> {    {{None},             _Real},
    {{None, None},       _Real},
    {{None, None, None}, _Real}
},
"TimeZoneConvert" -> {    {{None, None}, None}
},
"TimeZoneOffset" -> {    {{None},       _Quantity},
    {{None, None}, _Quantity}
},
"PositiveSemidefiniteMatrixQ" -> {{{None}, _?BooleanQ}},"BlockLowerTriangularMatrix" -> {{{None}, _List}, {{None, None}, _List}},"BlockUpperTriangularMatrix" -> {{{None}, _List}, {{None, None}, _List}},"SingularValueDecomposition" -> {    {{None}, _List},                (* SVD[m]     → {U, Sigma, V} *)
    {{_List}, _List},
    {{None, None}, _List}           (* SVD[m, k]  → k sing. values *)
},
"LowerTriangularize" -> {    {{None}, _List},
    {{_List}, _List},
    {{None, None}, _List},          (* LowerTriangularize[m, k]     *)
    {{_List, _Integer}, _List}
},
"UpperTriangularize" -> {    {{None}, _List},
    {{_List}, _List},
    {{None, None}, _List},
    {{_List, _Integer}, _List}
},
"SingularValueList" -> {    {{None}, _List},                (* all singular values          *)
    {{_List}, _List},
    {{None, None}, _List}           (* SingularValueList[m, k]      *)
},
"MatrixConditionNumber" -> {{{None}, _Real}, {{None, None}, _Real}}, (* same, explicit p-norm form *)"MatrixNorm"            -> {{{None}, _Real}, {{None, None}, _Real}}, (* induced/Frobenius norm *)"CharacteristicPolynomial" -> {{{None, None}, None}},   (* det(λI − A), polynomial in 2nd arg *)"ComplexExpand"            -> {{{None}, None}, {{None, None}, None}}, (* simplifies assuming real vars *)"CoordinateTransformData" -> {    {{None}, None},                   (* returns a CoordTransformData object *)
    {{None, None}, None}
},
"CoordinateTransform" -> {    {{None, None}, _List},          (* CoordinateTransform[{"sys1","sys2"}, pt] *)
    {{_List, _List}, _List}
},
"DecompositionMatrix" -> {    {{None, None}, _List},          (* DecompositionMatrix[group, chi] *)
    {{None, None, None}, _List}
},
"EigenvectorCentrality" -> {    {{_Graph}, _List},
    {{_Graph, None}, _List},       (* "In" | "Out" weight spec *)
    {{None}, _List}
},
"GerschgorinDisks" -> {{{None}, _List}, {{_List}, _List}},"GesselViennotLatticePathMatrix" -> {{{None}, _List}, {{None, None}, _List}},"WignerDMatrix" -> {    {{None, None}, _List},          (* WignerDMatrix[j, {α,β,γ}] → full (2j+1)×(2j+1) matrix *)
    {{_List, None}, _Complex}      (* WignerDMatrix[{j,m,mp}, angles] → single D^j_{m,mp} element *)
},
"BlochVector" -> {{{None}, _List}},"MatrixPlot" -> {{{None}, _Graphics}, {{_List}, _Graphics}},"AbsoluteFileName" -> {  {{_String}, _String}          (* canonical absolute path *)
},
"ExpandFileName" -> {  {{_String}, _String}          (* expands ~, env-vars, relative refs *)
},
"FileNameSplit" -> {  {{_String}, _List}            (* split path into component list *)
},
"FileNameTake" -> {  {{_String}, _String},         (* last component *)
  {{_String, _Integer}, _String}, (* first/last n components *)
  {{_String, _List}, _String}  (* span of components *)
},
"FileNameDrop" -> {  {{_String}, _String},
  {{_String, _Integer}, _String} (* drop first/last n components *)
},
"FileNames" -> {  {{}, _List},
  {{_String}, _List},
  {{_List}, _List},
  {{_String, _String}, _List},
  {{_String, _List}, _List},
  {{_String, _String, _Integer}, _List},
  {{_String, _List, _Integer}, _List}
},
"FileByteCount" -> {  {{_String}, _Integer}
},
"FileDate" -> {  {{_String}, None},
  {{_String, _String}, None}     (* property: "Creation", "Access", … *)
},
"FileInformation" -> {  {{_String}, _List}
},
"FileType" -> {  {{_String}, None}
},
"FindFile" -> {  {{_String}, _String}
},
"CreateDirectory" -> {  {{}, _String},
  {{_String}, _String}
},
"CreateFile" -> {  {{}, _String},
  {{_String}, _String}
},
"DeleteFile" -> {  {{_String}, None},
  {{_List}, None}
},
"DeleteDirectory" -> {  {{_String}, None}
},
"CopyFile" -> {  {{_String, _String}, _String}
},
"MoveFile" -> {  {{_String, _String}, _String}
},
"CopyDirectory" -> {  {{_String, _String}, _String}
},
"MoveDirectory" -> {  {{_String, _String}, _String}
},
"SetDirectory" -> {  {{_String}, _String},         (* returns previous directory *)
  {{}, _String}                  (* resets to $HomeDirectory *)
},
"ResetDirectory" -> {  {{}, _String}                  (* pops stack; returns restored dir *)
},
"HomeDirectory" -> {  {{}, _String}
},
"ParentDirectory" -> {  {{}, _String},
  {{_String}, _String}
},
"DirectoryListing" -> {  {{}, _List},
  {{_String}, _List}
},
"Import" -> {  {{_String}, None},
  {{_String, _String}, None},
  {{_String, _List}, None}
},
"ImportString" -> {  {{_String, _String}, None},
  {{_String, _String, _List}, None}
},
"Export" -> {  {{_String, None}, _String},
  {{_String, None, _String}, _String}
},
"ReadList" -> {  {{_String}, _List},
  {{_String, None}, _List},
  {{_String, None, _Integer}, _List}
},
"ReadString" -> {  {{_String}, _String},         (* whole file *)
  {{None}, _String}              (* from InputStream *)
},
"ReadLine" -> {  {{None}, _String}              (* one line from stream *)
},
"BinaryReadList" -> {  {{_String}, _List},
  {{None}, _List},
  {{_String, None}, _List},
  {{None, None}, _List},
  {{_String, None, _Integer}, _List},
  {{None, None, _Integer}, _List}
},
"BinaryRead" -> {  {{None}, None},
  {{None, None}, None}
},
"BinaryWrite" -> {  {{None, None}, None}
},
"Write" -> {  {{None, __}, None}
},
"WriteString" -> {  {{None, _String}, None},
  {{None, __}, None}
},
"OpenRead" -> {  {{_String}, None}
},
"OpenWrite" -> {  {{}, None},
  {{_String}, None}
},
"OpenAppend" -> {  {{_String}, None}
},
"Close" -> {  {{None}, _String}
},
"Streams" -> {  {{}, _List},
  {{_String}, _List}
},
"Association" -> {  {{}, _Association},
  {{_Rule}, _Association},
  {{_Rule, __}, _Association},
  {{_List}, _Association}
},
"AssociationMap" -> {  {{None, _List}, _Association},
  {{_Symbol, _List}, _Association},
  {{None, _Association}, _Association}
},
"JoinAcross" -> {  {{_List, _List, _String}, _List},
  {{_List, _List, _List}, _List},
  {{_List, _List, _String, _String}, _List} (* with join type *)
},
"KeyDrop" -> {  {{_Association, None}, _Association},
  {{_Association, _List}, _Association},
  {{_List, None}, _List},              (* list-of-assocs form *)
  {{_List, _List}, _List}
},
"KeyExistsQ" -> {  {{_Association, None}, _?BooleanQ}
},
"KeyMap" -> {  {{None, _Association}, _Association},
  {{_Symbol, _Association}, _Association}
},
"KeyMemberQ" -> {  {{_Association, None}, _?BooleanQ},
  {{_List, None}, _?BooleanQ}
},
"KeySelect" -> {  {{_Association, None}, _Association},
  {{_Association, _Symbol}, _Association}
},
"KeySort" -> {  {{_Association}, _Association}
},
"KeySortBy" -> {  {{_Association, None}, _Association},
  {{_Association, _Symbol}, _Association}
},
"KeyTake" -> {  {{_Association, _List}, _Association},
  {{_Association, None}, _Association},
  {{_List, _List}, _List}             (* list-of-assocs -> list *)
},
"KeyValueMap" -> {  {{None, _Association}, _List},
  {{_Symbol, _Association}, _List}
},
"Lookup" -> {  {{_Association, None}, None},
  {{_Association, None, None}, None},   (* with default *)
  {{_List, None}, _List},             (* list-of-assocs -> list of values *)
  {{_List, None, None}, _List}
},
"CoefficientArrays" -> {  {{None, None}, _List},            (* polys, vars *)
  {{None, None, None}, _List}       (* polys, vars, options *)
},
"CoefficientList" -> {  {{None, None}, _List},            (* poly, var → flat coeff list *)
  {{None, _List}, _List},          (* poly, {vars} → multidim array *)
  {{None, _List, _List}, _List}   (* … with explicit dimensions *)
},
"CoefficientRules" -> {  {{None, _List}, _List},          (* poly, {vars} → {{exp...}->coeff, ...} *)
  {{None, _List, None}, _List}     (* … with monomial ordering *)
},
"Collect" -> {  {{None, None}, None},
  {{None, None, None}, None}          (* with transform function f *)
},
"Cyclotomic" -> {  {{_Integer, None}, None}
},
"Discriminant" -> {  {{None, None}, None},
  {{None, None, None}, None}          (* three-arg form *)
},
"Exponent" -> {  {{None, None}, _Integer},
  {{None, None, None}, None}          (* Exponent[p, x, List] → {1,3,...} *)
},
"ExpandDenominator" -> {  {{None}, None}                      (* expands denominator in place *)
},
"ExpandNumerator" -> {  {{None}, None}
},
"Factor" -> {  {{None}, None},
  {{None, None}, None}                (* with options, e.g. Modulus->p *)
},
"FactorList" -> {  {{None}, _List},
  {{None, None}, _List}             (* with options *)
},
"FactorTerms" -> {  {{None}, None},
  {{None, None}, None},               (* factor w.r.t. specific variable *)
  {{None, _List}, None}
},
"FactorTermsList" -> {  {{None}, _List},
  {{None, None}, _List},
  {{None, _List}, _List}
},
"GroebnerBasis" -> {  {{None, _List}, _List},          (* eqns/polys, vars *)
  {{None, _List, _List}, _List}   (* eqns, vars, parameters *)
},
"HornerForm" -> {  {{None}, None},
  {{None, None}, None}                (* with explicit variable *)
},
"IrreduciblePolynomialQ" -> {  {{None}, _?BooleanQ},
  {{None, None}, _?BooleanQ}        (* options-list as second arg *)
},
"MinimalPolynomial" -> {  {{None}, None},                     (* returns Function[...] *)
  {{None, None}, None}                (* MinimalPolynomial[x, var] → poly in var *)
},
"MonomialList" -> {  {{None}, _List},                  (* monomials of poly *)
  {{None, _List}, _List},          (* poly, {vars} *)
  {{None, _List, None}, _List}     (* with monomial ordering *)
},
"NestWhile" -> {  {{None, None, None}, None},
  {{None, None, None, _Integer}, None},   (* with look-back count *)
  {{None, None, None, _Integer, _Integer}, None}  (* with max iterations *)
},
"Numerator" -> {  {{None}, None}
},
"Denominator" -> {  {{None}, None}
},
"PolynomialGCD" -> {  {{None, None}, None},
  {{__}, None}                     (* variadic: more than two args *)
},
"PolynomialLCM" -> {  {{None, None}, None},
  {{__}, None}
},
"PolynomialMod" -> {  {{None, None}, None},
  {{None, None, _List}, None}
},
"PolynomialQuotient" -> {  {{None, None, None}, None}          (* p, q, var *)
},
"PolynomialQuotientRemainder" -> {  {{None, None, None}, _List}
},
"PolynomialReduce" -> {  {{None, _List, _List}, _List}
},
"PolynomialRemainder" -> {  {{None, None, None}, None}
},
"RootReduce" -> {  {{None}, None}
},
"Together" -> {  {{None}, None}
},
"Apart" -> {  {{None}, None},
  {{None, None}, None}                (* Apart[expr, var] *)
},
"Cancel" -> {  {{None}, None}
},
"BaseForm" -> {  {{None, _Integer}, None}           (* n, base → formatted output object *)
},
"DecimalForm" -> {  {{None}, None},
  {{None, None}, None}                (* with digit-count spec *)
},
"EngineeringForm" -> {  {{None}, None},
  {{None, None}, None}
},
"NumberForm" -> {  {{None, None}, None},               (* expr, n — n significant digits *)
  {{None, _List}, None}              (* with {n, d} digits spec *)
},
"PaddedForm" -> {  {{None, None}, None},
  {{None, _List}, None}
},
"ScientificForm" -> {  {{None}, None},
  {{None, None}, None}
},
"ByteArray" -> {  {{_List}, None},                   (* ByteArray[{byte, ...}] *)
  {{_String}, None}                  (* ByteArray["base64string"] *)
},
"CarmichaelLambda" -> {  {{_Integer}, _Integer},
  {{None}, _Integer}
},
"ChineseRemainder" -> {  {{_List, _List}, _Integer}
},
"CoprimeQ" -> {  {{None, None}, _?BooleanQ},
  {{__}, _?BooleanQ}
},
"Divisors" -> {  {{_Integer}, _List},
  {{None}, _List}
},
"DivisorSum" -> {  {{_Integer, None}, _?NumericQ},
  {{None, None}, _?NumericQ},
  {{None, None, None}, _?NumericQ}  (* with divisibility test *)
},
"ExtendedGCD" -> {  {{_Integer, _Integer}, _List},
  {{None, None}, _List},
  {{__Integer}, _List}
},
"FromDigits" -> {  {{_List}, _Integer},
  {{_List, _Integer}, _Integer},  (* with explicit base *)
  {{_String}, _Integer},
  {{_String, _Integer}, _Integer}
},
"IntegerExponent" -> {  {{_Integer, _Integer}, _Integer},
  {{None, None}, _Integer}
},
"IntegerName" -> {  {{_Integer}, _String},
  {{_Integer, _String}, _String},  (* with language, e.g. "French" *)
  {{None}, _String},
  {{None, None}, _String}
},
"IntegerReverse" -> {  {{_Integer}, _Integer},
  {{_Integer, _Integer}, _Integer}, (* base *)
  {{None}, _Integer},
  {{None, None}, _Integer}
},
"JacobiP" -> {  {{None, None, None, _Real}, _Real},
  {{None, None, None, None}, _?NumericQ}   (* symbolic → NumericQ only if numeric inputs *)
},
"MantissaExponent" -> {  {{_Real}, _List},
  {{None}, _List},
  {{_Real, _Integer}, _List},     (* with base b *)
  {{None, None}, _List}
},
"NextPrime" -> {  {{_Integer}, _Integer},
  {{_Integer, _Integer}, _Integer},
  {{None}, _Integer},
  {{None, None}, _Integer}
},
"NumberDigit" -> {  {{None, _Integer}, _Integer},
  {{None, None}, _Integer},
  {{None, _Integer, _Integer}, _Integer},
  {{None, None, None}, _Integer}
},
"Overflow" -> {  {{}, _Real}
},
"Underflow" -> {  {{}, _Real}
},
"PowerMod" -> {  {{None, None, None}, _Integer},
  {{_Integer, _Integer, _Integer}, _Integer}
},
"PrimePi" -> {  {{_Integer}, _Integer},
  {{_Real}, _Integer},
  {{None}, _Integer}
},
"PrimePowerQ" -> {  {{_Integer}, _?BooleanQ},
  {{None}, _?BooleanQ}
},
"RandomColor" -> {  {{}, None},
  {{None}, None},                     (* with color space spec *)
  {{_Integer}, None},                (* n colors → list of colors *)
  {{None, _Integer}, None}
},
"RandomDate" -> {  {{}, None},
  {{None}, None},                     (* range spec *)
  {{_List}, None}
},
"RandomEntity" -> {  {{None}, None},
  {{None, _Integer}, None}           (* n entities → list *)
},
"RandomFunction" -> {  {{None, _List}, None},
  {{None, _List, _Integer}, None}   (* k realizations *)
},
"RandomGeoPosition" -> {  {{}, None},
  {{None}, None},                     (* region spec *)
  {{_Integer}, None},                (* n positions → list *)
  {{None, _Integer}, None}
},
"RandomGraph" -> {  {{_List}, _Graph},               (* {n, m} *)
  {{_List, _Integer}, _List},     (* k random graphs *)
  {{None}, _Graph},
  {{None, _Integer}, _List}
},
"RandomImage" -> {  {{}, _Image},
  {{None}, _Image},                 (* intensity range *)
  {{None, _List}, _Image},         (* range, {width, height} *)
  {{None, _List, None}, _Image}    (* with color space option *)
},
"RandomPermutation" -> {  {{_Integer}, None},                (* Cycles object of S_n *)
  {{None}, None}                      (* from a permutation group *)
},
"RandomPoint" -> {  {{None}, _List},                  (* single point as {x,y,...} *)
  {{None, _Integer}, _List},       (* n points *)
  {{None, _List}, _List}           (* {n1, n2, ...} shaped array *)
},
"RandomPolygon" -> {  {{_Integer}, None},
  {{_List}, None},                   (* {n, method-spec} *)
  {{None}, None}
},
"RandomPolyhedron" -> {  {{None}, None}
},
"RandomPrime" -> {  {{_Integer}, _Integer},          (* prime up to n *)
  {{_List}, _Integer},             (* prime in range {a, b} *)
  {{None}, _Integer},
  {{_Integer, _Integer}, _List},  (* n primes in range *)
  {{_List, _Integer}, _List}
},
"RandomSample" -> {  {{None}, _List},
  {{None, _Integer}, _List},
  {{_List}, _List},
  {{_List, _Integer}, _List}
},
"RandomSeed" -> {  {{}, None}
},
"RandomVariate" -> {  {{None}, _?NumericQ},             (* scalar dist → Real; approx., may be List *)
  {{None, _Integer}, _List},       (* n samples *)
  {{None, _List}, _List}           (* matrix shape *)
},
"RandomWalkProcess" -> {  {{None}, None},
  {{None, None}, None}
},
"RandomWord" -> {  {{}, _String},
  {{None}, _String},                (* with part-of-speech spec *)
  {{_Integer}, _List},             (* n words *)
  {{None, _Integer}, _List}        (* part-of-speech + count *)
},
"SeedRandom" -> {  {{}, None},
  {{_Integer}, None},
  {{None}, None}
},
"AASTriangle" -> {},"AbsArg" -> {  {{None}, _List}
},
"AbsoluteCorrelation" -> {  {{_List, _List}, _Real},
  {{_List},         _List}
},
"AbsoluteCorrelationFunction" -> {  {{_List, _Integer}, _Real},
  {{_List, _List},    _List}
},
"AbsoluteTiming" -> {  {{None}, _List}
},
"AccountingForm" -> {},"Accuracy" -> {  {{None}, _Real}
},
"AddSides" -> {},"AddTo" -> {},"Adjugate" -> {  {{_List}, _List}
},
"AffineTransform" -> {},"AiryAi" -> {  {{None}, _?NumericQ}
},
"AiryAiPrime" -> {  {{None}, _?NumericQ}
},
"AiryAiZero" -> {  {{_Integer},           _Real},
  {{_Integer, _Integer}, _Real}   (* radix form *)
},
"AiryBi" -> {  {{None}, _?NumericQ}
},
"AiryBiPrime" -> {  {{None}, _?NumericQ}
},
"AiryBiZero" -> {  {{_Integer},           _Real},
  {{_Integer, _Integer}, _Real}
},
"AllMatch" -> {  {{_List, None},          _?BooleanQ},
  {{_List, None, None},    _?BooleanQ}
},
"AllSameBy" -> {  {{_List, None}, _?BooleanQ}
},
"AllTrue" -> {  {{_List, None},       _?BooleanQ},
  {{_List, None, None}, _?BooleanQ}
},
"AlphabeticOrder" -> {  {{None, None},       _Integer},
  {{None, None, None}, _Integer}   (* with language/locale *)
},
"AlphabeticSort" -> {  {{_List},       _List},
  {{_List, None}, _List}   (* with language *)
},
"AlternatingFactorial" -> {  {{_Integer}, _Integer}
},
"AlphaChannel" -> {  {{_Image}, _Image},
  {{_Video}, _Video}
},
"AnglePath" -> {  {{_List},        _List},
  {{_List, _List}, _List}    (* initial point + angles *)
},
"AnglePath3D" -> {  {{_List},        _List},
  {{_List, _List}, _List}
},
"AngleVector" -> {  {{None},       _List},
  {{None, None}, _List}   (* {r, theta} form *)
},
"AnyMatch" -> {  {{_List, None},       _?BooleanQ},
  {{_List, None, None}, _?BooleanQ}
},
"AnySameBy" -> {  {{_List, None}, _?BooleanQ}
},
"AnyTrue" -> {  {{_List, None},       _?BooleanQ},
  {{_List, None, None}, _?BooleanQ}
},
"Apply" -> {},"Append" -> {  {{_List,        None}, _List},
  {{_Association, None}, _Association}
},
"AppendTo" -> {  {{None, None}, _List}
},
"ArithmeticGeometricMean" -> {  {{None, None}, _?NumericQ}
},
"ArrayDepth" -> {  {{None}, _Integer}
},
"ArrayFlatten" -> {  {{_List},          _List},
  {{_List, _Integer}, _List}
},
"ArrayPad" -> {  {{_List, None},       _List},
  {{_List, None, None}, _List}
},
"ArrayResample" -> {  {{_List,  _List}, _List},
  {{_Image, _List}, _Image}
},
"Assuming" -> {},"Attributes" -> {  {{None},     _List},
  {{_String}, _List}
},
"BesselI" -> {  {{None, None}, _?NumericQ}
},
"BesselJ" -> {  {{None, None}, _?NumericQ}
},
"BesselK" -> {  {{None, None}, _?NumericQ}
},
"BesselY" -> {  {{None, None}, _?NumericQ}
},
"BesselJZero" -> {  {{_Integer, _Integer},           _Real},
  {{_Integer, _Integer, _Integer}, _Real}
},
"BesselYZero" -> {  {{_Integer, _Integer},           _Real},
  {{_Integer, _Integer, _Integer}, _Real}
},
"BernoulliDistribution" -> {},"Beta" -> {  {{None, None},             _?NumericQ},
  {{None, None, None},       _?NumericQ},
  {{None, None, None, None}, _?NumericQ}
},
"BetaDistribution" -> {},"BooleanConvert" -> {},"BooleanFunction" -> {},"BooleanMinimize" -> {},"BooleanTable" -> {  {{None, _List},       _List},
  {{None, _List, __}, _List}
},
"BooleanVariables" -> {  {{None}, _List}
},
"CarlsonRC" -> {  {{None, None}, _?NumericQ}
},
"CarlsonRD" -> {  {{None, None, None}, _?NumericQ}
},
"CarlsonRF" -> {  {{None, None, None}, _?NumericQ}
},
"CarlsonRG" -> {  {{None, None, None}, _?NumericQ}
},
"CarlsonRJ" -> {  {{None, None, None, None}, _?NumericQ}
},
"Catch" -> {},"CauchyDistribution" -> {},"CharacterCode" -> {  {{_String}, _Integer},   (* single character *)
  {{_List},   _List}       (* list of characters *)
},
"CheckAll" -> {},"CheckAbort" -> {},"ChiSquareDistribution" -> {},"ClosenessCentrality" -> {  {{_Graph},       _List},
  {{_Graph, None}, _List}
},
"Coefficient" -> {  {{None, None},          None},
  {{None, None, _Integer}, None}
},
"CompilePrint" -> {},"Complex" -> {},"ComplexInfinity" -> {},"Composition" -> {  {{None, __}, None}
},
"Compress" -> {  {{None}, _String}
},
"ConstantRegionQ" -> {  {{None}, _?BooleanQ}
},
"Dataset" -> {},"Decrypt" -> {},"DeleteCases" -> {  {{_List, None},             _List},
  {{_List, None, None},       _List},
  {{_List, None, None, None}, _List}
},
"DeleteDuplicates" -> {  {{_List},       _List},
  {{_List, None}, _List}
},
"DeleteDuplicatesBy" -> {  {{_List, None}, _List}
},
"DeleteMissing" -> {  {{_List},             _List},
  {{_List, None},       _List},
  {{_List, None, None}, _List}
},
"Diagonal" -> {  {{_List},           _List},
  {{_List, _Integer}, _List}   (* k-th super/sub-diagonal *)
},
"DiagonalMatrix" -> {  {{_List},                     _List},
  {{_List, _Integer},          _List},
  {{_List, _Integer, _Integer}, _List}
},
"DirectedEdge" -> {},"DirectedInfinity" -> {},"DirichletL" -> {  {{None, None, None}, _?NumericQ}
},
"DirichletEta" -> {  {{None}, _?NumericQ}
},
"DirichletBeta" -> {  {{None}, _?NumericQ}
},
"DirichletLambda" -> {  {{None}, _?NumericQ}
},
"Dispatch" -> {},"Drop" -> {  {{_List, None}, _List}
},
"DSolve" -> {  {{None, None, None},   _List},
  {{None, None, _List}, _List}
},
"DSolveValue" -> {  {{None, None, None},   None},
  {{None, None, _List}, None}
},
"EllipticE" -> {  {{None},       _?NumericQ},   (* EllipticE[m]      — complete *)
  {{None, None}, _?NumericQ}    (* EllipticE[phi, m] — incomplete *)
},
"EllipticF" -> {  {{None, None}, _?NumericQ}
},
"EllipticK" -> {  {{None}, _?NumericQ}
},
"EllipticPi" -> {  {{None, None},       _?NumericQ},
  {{None, None, None}, _?NumericQ}
},
"EllipticTheta" -> {  {{None, None, None},       _?NumericQ},
  {{None, None, None, None}, _?NumericQ}
},
"EllipticLog" -> {  {{None, None}, None}
},
"Entropy" -> {  {{_List}, _?NumericQ}
},
"EntropyFilter" -> {  {{_Image, None}, _Image}
},
"EulerGamma" -> {},"ExpIntegralE" -> {  {{None, None}, _?NumericQ}
},
"ExpIntegralEi" -> {  {{None}, _?NumericQ}
},
"ExpandAll" -> {  {{None}, None},
  {{None, None}, None}    (* ExpandAll[expr, patt] *)
},
"Extract" -> {  {{None, None}, None},
  {{None, None, None}, None}   (* Extract[expr, pos, h] — wraps in h *)
},
"FindCurvePath" -> {  {{_List}, _List}
},
"FindCycle" -> {  {{None},             _List},
  {{None, None},       _List},   (* FindCycle[g, k] — cycles of length k *)
  {{None, None, None}, _List}    (* FindCycle[g, k, Overlaps->...] *)
},
"FindFit" -> {  {{_List, None, None, None}, _List}
},
"FindInstance" -> {  {{None, None},             _List},
  {{None, None, None},       _List},   (* over a domain *)
  {{None, None, None, None}, _List}    (* n solutions *)
},
"FindPath" -> {  {{None, None, None},       _List},
  {{None, None, None, None}, _List}    (* FindPath[g, s, t, k] — paths up to length k *)
},
"FindPermutation" -> {  {{None, None}, None}
},
"FindSpanningTree" -> {  {{None}, _Graph},
  {{None, None}, _Graph}
},
"FixedPoint" -> {  {{None, None}, None},
  {{None, None, None}, None}   (* FixedPoint[f, expr, max] *)
},
"FixedPointList" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"Flatten" -> {  {{_List},       _List},
  {{_List, None}, _List}    (* Flatten[list, n] or Flatten[list, n, h] *)
},
"FlattenAt" -> {  {{None, None}, None}
},
"Fold" -> {  {{None, None, _List}, None}
},
"FormFunction" -> {  {{None, None}, None}
},
"FrameTicks" -> {},"FrenetSerretSystem" -> {  {{None, None},       _List},
  {{None, None, None}, _List}   (* explicit parameter range *)
},
"FromContinuedFraction" -> {  {{_List}, _Rational}
},
"FromLetterNumber" -> {  {{_Integer}, _String},
  {{_List},    _List}     (* FromLetterNumber[{n1,n2,...}] — list of strings *)
},
"FromRomanNumeral" -> {  {{_String}, _Integer}
},
"FunctionInterpolation" -> {  {{None, None}, None}
},
"GammaDistribution" -> {  {{None, None}, None}
},
"GammaRegularized" -> {  {{None, None},       _Real},
  {{None, None, None}, _Real}   (* GammaRegularized[a, z0, z1] — difference form *)
},
"GeometricMean" -> {  {{_List}, _?NumericQ}
},
"GeneratingFunction" -> {  {{None, None, None},       None},
  {{None, None, None, None}, None}   (* GeneratingFunction[expr, {n, n0}, x] *)
},
"GeometricDistribution" -> {  {{None}, None}
},
"Get" -> {  {{_String}, None}
},
"Graphics" -> {  {{None}, _Graphics},
  {{None, None}, _Graphics}   (* Graphics[prims, opts] *)
},
"Graphics3D" -> {  {{None}, _Graphics3D},
  {{None, None}, _Graphics3D}
},
"HarmonicMean" -> {  {{_List}, _?NumericQ}
},
"HarmonicNumber" -> {  {{_Integer}, _Rational},
  {{None, None}, _?NumericQ}
},
"Head" -> {  {{None}, None}
},
"Hypergeometric0F1" -> {  {{None, None}, _?NumericQ}
},
"Hypergeometric1F1" -> {  {{None, None, None}, _?NumericQ}
},
"Hypergeometric2F1" -> {  {{None, None, None, None}, _?NumericQ}
},
"HypergeometricPFQ" -> {  {{_List, _List, None}, _?NumericQ}
},
"HypergeometricU" -> {  {{None, None, None}, _?NumericQ}
},
"HypergeometricPFQRegularized" -> {  {{_List, _List, None}, _?NumericQ}
},
"Integrate" -> {  {{None, None}, None},          (* indefinite — expression *)
  {{None, None}, _?NumericQ}   (* definite with {x,a,b} — numeric when limits numeric *)
},
"IntegrateChangeVariables" -> {  {{None, None, None, None}, None}
},
"IntegerPartitions" -> {  {{_Integer},             _List},
  {{_Integer, None},       _List},   (* IntegerPartitions[n, k] — up to k parts *)
  {{_Integer, None, None}, _List}    (* IntegerPartitions[n, k, s] — with parts from s *)
},
"Interpolation" -> {  {{_List}, None}
},
"InterpolatingPolynomial" -> {  {{_List, None}, None}
},
"InverseFourier" -> {  {{_List}, _List}
},
"InverseZTransform" -> {  {{None, None, None}, None}
},
"InverseLaplaceTransform" -> {  {{None, None, None}, None}
},
"InverseFunction" -> {  {{None}, None}
},
"JacobiAmplitude" -> {  {{None, None}, _?NumericQ}
},
"JacobiCD" -> { {{None, None}, _?NumericQ} },"JacobiCN" -> { {{None, None}, _?NumericQ} },"JacobiCS" -> { {{None, None}, _?NumericQ} },"JacobiDC" -> { {{None, None}, _?NumericQ} },"JacobiDN" -> { {{None, None}, _?NumericQ} },"JacobiDS" -> { {{None, None}, _?NumericQ} },"JacobiNC" -> { {{None, None}, _?NumericQ} },"JacobiND" -> { {{None, None}, _?NumericQ} },"JacobiNS" -> { {{None, None}, _?NumericQ} },"JacobiSC" -> { {{None, None}, _?NumericQ} },"JacobiSD" -> { {{None, None}, _?NumericQ} },"JacobiSN" -> { {{None, None}, _?NumericQ} },"JacobiZeta" -> {  {{None, None}, _?NumericQ}
},
"Join" -> {  {{_List, _List},              _List},
  {{_List, __},               _List},
  {{_Association, _Association}, _Association},
  {{_Association, __},         _Association}
},
"LaplaceTransform" -> {  {{None, None, None}, None}
},
"LerchPhi" -> {  {{None, None, None}, _?NumericQ}
},
"Limit" -> {  {{None, _Rule}, None},
  {{None, _Rule, None}, None}   (* Limit[expr, x->a, Direction->...] *)
},
"LinearProgramming" -> {  {{_List, _List, _List},       _List},
  {{_List, _List, _List, None}, _List}   (* with variable bounds *)
},
"ListConvolve" -> {  {{_List, _List},       _List},
  {{_List, _List, None}, _List}   (* with cyclic options *)
},
"ListCorrelate" -> {  {{_List, _List},       _List},
  {{_List, _List, None}, _List}
},
"ListDensityPlot" -> {  {{_List}, _Graphics},
  {{_List, __}, _Graphics}
},
"ListLinePlot" -> {  {{_List}, _Graphics},
  {{_List, __}, _Graphics}
},
"ListLogLinearPlot" -> {  {{_List}, _Graphics},
  {{_List, __}, _Graphics}
},
"ListLogLogPlot" -> {  {{_List}, _Graphics},
  {{_List, __}, _Graphics}
},
"ListPlot" -> {  {{_List}, _Graphics},
  {{_List, __}, _Graphics}
},
"ListPolarPlot" -> {  {{_List}, _Graphics},
  {{_List, __}, _Graphics}
},
"ListVectorPlot" -> {  {{_List}, _Graphics},
  {{_List, __}, _Graphics}
},
"ListPlot3D" -> {  {{_List}, _Graphics3D},
  {{_List, __}, _Graphics3D}
},
"ListPointPlot3D" -> {  {{_List}, _Graphics3D},
  {{_List, __}, _Graphics3D}
},
"ListVectorPlot3D" -> {  {{_List}, _Graphics3D},
  {{_List, __}, _Graphics3D}
},
"LogGamma" -> {  {{None}, _?NumericQ}
},
"LogIntegral" -> {  {{None}, _?NumericQ}
},
"Map" -> {  {{None, _List}, _List},              (* Map f over list → list of f[elem] results *)
  {{None, _List, None}, _List},        (* Map at levelspec → list *)
  {{None, None}, None}                    (* Map over non-list expr → same head, opaque *)
},
"MapApply" -> {  {{None, _List}, _List},              (* f @@@ list → list with head replaced by f at level 1 *)
  {{None, _List, None}, _List}         (* at explicit levelspec *)
},
"MapAt" -> {  {{None, None, None}, None}              (* MapAt[f, expr, pos] → expr with part at pos modified *)
},
"MapIndexed" -> {  {{None, _List}, _List},              (* MapIndexed[f, list] → {f[e1,{1}], f[e2,{2}], …} *)
  {{None, _List, None}, _List}         (* at levelspec *)
},
"MathieuC" -> {  {{None, None, None}, _?NumericQ}      (* MathieuC[a, q, z] → even Mathieu function; NumericFunction *)
},
"MathieuCPrime" -> {  {{None, None, None}, _?NumericQ}      (* d/dz MathieuC[a, q, z]; NumericFunction *)
},
"MathieuS" -> {  {{None, None, None}, _?NumericQ}      (* MathieuS[b, q, z] → odd Mathieu function; NumericFunction *)
},
"MathieuSPrime" -> {  {{None, None, None}, _?NumericQ}      (* d/dz MathieuS[b, q, z]; NumericFunction *)
},
"MathieuCharacteristicA" -> {  {{None, None}, _?NumericQ}            (* MathieuCharacteristicA[r, q] → a_r; NumericFunction *)
},
"MathieuCharacteristicB" -> {  {{None, None}, _?NumericQ}            (* MathieuCharacteristicB[r, q] → b_r; NumericFunction *)
},
"MeijerG" -> {  {{None, None, None}, _?NumericQ},     (* MeijerG[{{a…},{a…}},{{b…},{b…}}, z] → G-function; NumericFunction *)
  {{None, None, None, None}, _?NumericQ} (* with auxiliary index list *)
},
"MersennePrimeExponent" -> {  {{_Integer}, _Integer}               (* MersennePrimeExponent[n] → n-th Mersenne prime exponent *)
},
"Multinomial" -> {  {{__Integer}, _Integer}            (* Multinomial[n1,n2,…] → (n1+n2+…)!/(n1! n2! …) as Integer *)
},
"MultiplicativeOrder" -> {  {{_Integer, _Integer}, _Integer},   (* MultiplicativeOrder[k, n] → smallest m: k^m ≡ 1 mod n *)
  {{_Integer, _Integer, _List}, _Integer} (* generalized: k^m ≡ r_i mod n for some r_i *)
},
"NDSolve" -> {  {{None, None, _List}, _List}         (* NDSolve[eqns, y, {x,a,b}] → {{y→InterpolatingFunction[…]}} *)
},
"NDSolveValue" -> {  {{None, None, _List}, None}            (* NDSolveValue[eqns, expr, {x,a,b}] → InterpolatingFunction or expr value *)
},
"NestWhileList" -> {  {{None, None, None}, _List},          (* NestWhileList[f, x, test] → list of all intermediates *)
  {{None, None, None, _Integer}, _List},
  {{None, None, None, _Integer, _Integer}, _List}
},
"NIntegrate" -> {  {{None, _List}, _Real},              (* NIntegrate[f, {x,a,b}] → numerical definite integral (Real) *)
  {{None, _List, _List}, _Real}       (* multi-dimensional: {x,a,b},{y,c,d},… *)
},
"NoneTrue" -> {  {{_List, None}, _?BooleanQ}          (* NoneTrue[list, pred] → True iff no element satisfies pred *)
},
"NumericalOrder" -> {  {{None, None}, _Integer}              (* NumericalOrder[e1,e2] → -1, 0, or 1 *)
},
"OwnValues" -> {  {{_Symbol}, _List},                  (* OwnValues[sym] → {HoldPattern[sym] :> val, …} *)
  {{_String}, _List}                   (* OwnValues["sym"] → same via string name *)
},
"Part" -> {  {{None, None}, None},                   (* expr[[i]] → extracted part, type depends on expr *)
  {{None, None, None}, None}              (* expr[[i,j,…]] → deep part extraction *)
},
"PartitionsQ" -> {  {{_Integer}, _Integer}               (* PartitionsQ[n] → q(n): partitions into distinct parts *)
},
"Pick" -> {  {{_List, _List}, _List},            (* Pick[list, sel] → elems of list where sel is True *)
  {{_List, _List, None}, _List}       (* Pick[list, sel, v] → elems where sel == v *)
},
"PowerExpand" -> {  {{None}, None},                         (* PowerExpand[expr] → expanded power/log expression *)
  {{None, _List}, None}                  (* restricted to given variables *)
},
"Prepend" -> {  {{_List, None}, _List},              (* Prepend[list, elem] → {elem, rest…} *)
  {{_Association, _Rule}, _Association} (* Prepend[assoc, key->val] → assoc with new first key *)
},
"PrependTo" -> {  {{_Symbol, None}, _List}             (* PrependTo[x, elem] → mutates x; returns new list *)
},
"ProductLog" -> {  {{None}, _?NumericQ},                 (* ProductLog[z] → principal branch of Lambert W; NumericFunction *)
  {{_Integer, None}, _?NumericQ}       (* ProductLog[k, z] → k-th branch *)
},
"PseudoInverse" -> {  {{_List}, _List}                     (* PseudoInverse[m] → Moore-Penrose pseudoinverse (matrix) *)
},
"Quiet" -> {  {{None}, None},                         (* Quiet[expr] → result of expr with messages suppressed *)
  {{None, None}, None}                    (* Quiet[expr, {s::t,…}] → suppress specific messages *)
},
"Reap" -> {  {{None}, _List},                      (* Reap[expr] → {result, {{sown…}}} always a 2-elem list *)
  {{None, None}, _List},                (* Reap[expr, patt] → filtered by tag pattern *)
  {{None, None, None}, _List}           (* Reap[expr, patt, f] → with wrapper f[tag,{vals}] *)
},
"Replace" -> {  {{None, None}, None},                   (* Replace[expr, rules] → try transform entire expr *)
  {{None, None, None}, None}              (* Replace[expr, rules, levelspec] → at given levels *)
},
"ReplaceAll" -> {  {{None, None}, None}                    (* expr /. rules → transform all matching subexpressions *)
},
"ReplacePart" -> {  {{None, None}, None}                    (* ReplacePart[expr, {pos->new}] → expr with part replaced *)
},
"ReplaceRepeated" -> {  {{None, None}, None}                    (* expr //. rules → apply rules until no change *)
},
"Rest" -> {  {{_List}, _List}                     (* Rest[list] → list with first element dropped *)
},
"Rotate" -> {  {{None, None}, None},                   (* Rotate[g, θ] → rotated Graphics primitive/object *)
  {{None, None, None}, None}              (* Rotate[g, θ, {x,y}] → around a given origin point *)
},
"RotateLeft" -> {  {{_List}, _List},                    (* RotateLeft[list] → cycled one position left *)
  {{_List, _Integer}, _List},         (* cycled n positions left *)
  {{_List, _List}, _List}             (* multi-level rotation by {n1,n2,…} *)
},
"RotateRight" -> {  {{_List}, _List},                    (* RotateRight[list] → cycled one position right *)
  {{_List, _Integer}, _List},         (* cycled n positions right *)
  {{_List, _List}, _List}             (* multi-level rotation by {n1,n2,…} *)
},
"RuleDelayed" -> {  {{None, None}, None}                    (* lhs :> rhs → delayed rule (HoldRest, SequenceHold) *)
},
"SatisfiabilityCount" -> {  {{None}, _Integer},                   (* SatisfiabilityCount[bf] → count of satisfying variable assignments *)
  {{None, _List}, _Integer}            (* with explicit variables list *)
},
"SatisfiabilityInstances" -> {  {{None}, _List},                      (* SatisfiabilityInstances[bf] → one satisfying assignment *)
  {{None, _List}, _List},              (* with variable list *)
  {{None, _List, _Integer}, _List}    (* m distinct satisfying instances *)
},
"SatisfiableQ" -> {  {{None}, _?BooleanQ},                 (* SatisfiableQ[bf] → True if at least one assignment satisfies bf *)
  {{None, _List}, _?BooleanQ}          (* with explicit variables *)
},
"Scan" -> {  {{None, None}, None},                   (* Scan[f, expr] → Null; f applied for side effects *)
  {{None, None, None}, None}              (* at levelspec *)
},
"Select" -> {  {{_List, None}, _List},              (* Select[list, f] → {elems for which f[elem] is True} *)
  {{_List, None, _Integer}, _List}    (* first n selected elements *)
},
"Sequence" -> {  {{__}, None}                         (* Sequence[e1,e2,…] → spliced at call site; head is Sequence *)
},
"SeriesCoefficient" -> {  {{None, None}, None},                   (* SeriesCoefficient[s, n] → nth coefficient; may be symbolic *)
  {{None, _List}, None}                  (* SeriesCoefficient[s, {x, x0, n}] → coefficient at x0 *)
},
"SeriesData" -> {  {{None, None, _List, _Integer, _Integer, _Integer}, None} (* internal series representation; opaque *)
},
"Sort" -> {  {{_List}, _List},                    (* Sort[list] → canonically sorted list *)
  {{_List, None}, _List}               (* Sort[list, p] → sorted by predicate p *)
},
"SortBy" -> {  {{_List, None}, _List},              (* SortBy[list, f] → sorted by f[elem] values *)
  {{_List, None, None}, _List}         (* with explicit ordering function *)
},
"SparseArray" -> {  {{_List}, _SparseArray},             (* SparseArray[{pos->val,…}] → sparse array *)
  {{_List, _List}, _SparseArray},     (* with explicit dimensions *)
  {{_List, _List, None}, _SparseArray} (* with explicit default value *)
},
"Subtract" -> {  {{None, None}, _?NumericQ}            (* a - b; Listable and NumericFunction *)
},
"SubtractSides" -> {  {{None}, None},                         (* SubtractSides[rel] → zero-RHS form of equation *)
  {{None, None}, None}                    (* SubtractSides[rel, x] or SubtractSides[rel1, rel2] *)
},
"Sum" -> {  {{None, _List}, None},                 (* Sum[f, {i,a,b}] → symbolic/closed-form sum *)
  {{None, _List, _List}, None}          (* multiple iterator sum *)
},
"TableForm" -> {  {{_List}, None}                        (* TableForm[list] → formatted display object, not a typed value *)
},
"Throw" -> {  {{None}, None},                         (* Throw[val] → non-local transfer; Catch returns val *)
  {{None, None}, None}                    (* Throw[val, tag] → tagged throw for matching Catch *)
},
"ToCharacterCode" -> {  {{_String}, _List},                  (* ToCharacterCode["str"] → list of Unicode code points *)
  {{_String, _String}, _List}         (* ToCharacterCode["str", "encoding"] *)
},
"ToExpression" -> {  {{_String}, None},                     (* ToExpression["str"] → parsed+evaluated; type unknown *)
  {{_String, None}, None},               (* with explicit InputForm/etc. *)
  {{_String, None, None}, None}          (* with head wrapper (e.g. HoldComplete) *)
},
"ToRules" -> {  {{None}, _List}                       (* ToRules[eqns] → {var->val,…} rules from Roots/Reduce output *)
},
"Trace" -> {  {{None}, _List},                      (* Trace[expr] → nested list of all evaluation steps *)
  {{None, None}, _List}                 (* Trace[expr, form|sym] → filtered trace *)
},
"TransformedField" -> {  {{None, None, _Rule}, None}            (* TransformedField[t, f, {x…}->{y…}] → field in new coords *)
},
"Transpose" -> {  {{_List}, _List},                    (* Transpose[m] → swap first two levels of tensor/matrix *)
  {{_List, _List}, _List},            (* Transpose[m, {n1,n2,…}] → permute level order *)
  {{_List, _Integer}, _List}          (* Transpose[m, k] → cycle levels k positions *)
},
"TruncatedDistribution" -> {  {{_List, None}, None}                  (* TruncatedDistribution[{xmin,xmax}, dist] → dist object *)
},
"Uncompress" -> {  {{_String}, None}                      (* Uncompress["str"] → decompressed expression; type varies *)
},
"VectorAngle" -> {  {{_List, _List}, _Real}             (* VectorAngle[u, v] → angle between vectors in radians (Real) *)
},
"WhittakerM" -> {  {{None, None, None}, _?NumericQ}      (* WhittakerM[k,m,z] → M_{k,m}(z); Listable, NumericFunction *)
},
"WhittakerW" -> {  {{None, None, None}, _?NumericQ}      (* WhittakerW[k,m,z] → W_{k,m}(z); Listable, NumericFunction *)
},
"With" -> {  {{_List, None}, None}                  (* With[{x=x0,…}, expr] → expr with constants substituted *)
},
"WolframAlpha" -> {  {{_String}, None},                     (* WolframAlpha["query"] → result pods; type varies by format *)
  {{_String, None}, None}                (* WolframAlpha["query", format] → formatted output *)
},
"Zeta" -> {  {{None}, _?NumericQ},                 (* Zeta[s] → Riemann ζ(s); Listable, NumericFunction *)
  {{None, None}, _?NumericQ}            (* Zeta[s, a] → Hurwitz ζ(s,a) *)
},
"ZTransform" -> {  {{None, None, None}, None},             (* ZTransform[expr, n, z] → Z-transform expression *)
  {{None, _List, _List}, None}          (* multidimensional Z-transform *)
},

(* ═══ Wave 3: Additional function signatures ═══ *)
"CMYKColor" -> {  {{_Real, _Real, _Real, _Real}, None},       (* CMYKColor[c, m, y, k] *)
  {{_Real, _Real, _Real, _Real, _Real}, None} (* with opacity *)
},
"ColorBalance" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image}  (* with reference color/target *)
},
"ColorCombine" -> {  {{_List}, _Image},        (* combine grayscale channel images *)
  {{_List, _String}, _Image}  (* with explicit colorspace *)
},
"ColorConvert" -> {  {{None, _String}, None},    (* color directive -> converted color *)
  {{_Image, _String}, _Image}  (* image colorspace conversion *)
},
"ColorData" -> {  {{}, _List},               (* ColorData[] -> list of named collections *)
  {{_String}, None},          (* "scheme" -> ColorDataFunction *)
  {{_String, _String}, None} (* "scheme", "property" -> property value *)
},
"ColorDataFunction" -> {  {{__}, None}              (* ColorDataFunction[range, ...][param] -> color *)
},
"ColorDetect" -> {  {{_Image, None}, _Image}  (* mask image of detected color regions *)
},
"ColorDistance" -> {  {{None, None}, _Real},     (* two color directives -> perceptual distance *)
  {{_List, None}, _List},   (* list of colors vs one color *)
  {{_Image, None}, _Image}  (* pixelwise distance image *)
},
"Colorize" -> {  {{None}, _Image}           (* integer matrix or image -> pseudocolor image *)
},
"ColorNegate" -> {  {{None}, "_[1]"}             (* image->image, color->color, video->video *)
},
"ColorProfileData" -> {  {{__}, None}              (* ICC color profile representation *)
},
"ColorQuantize" -> {  {{_Image}, _Image},
  {{_Image, _Integer}, _Image},   (* at most n colors *)
  {{_Image, _List}, _Image}       (* use specified color palette *)
},
"ColorReplace" -> {  {{_Image, None}, _Image},        (* replace color with transparent *)
  {{_Image, None, None}, _Image}   (* with explicit distance threshold *)
},
"ColorSeparate" -> {  {{_Image}, _List},               (* list of single-channel images *)
  {{_Image, None}, _Image}         (* single named channel *)
},
"ColorSetter" -> {  {{}, None},                         (* UI widget *)
  {{None}, None}
},
"ColorSetterBox" -> {},"ColorSlider" -> {  {{}, None},                         (* UI widget *)
  {{None}, None}
},
"ColorsNear" -> {  {{None}, None},                     (* symbolic color-region specification *)
  {{None, _Real}, None},             (* with max distance *)
  {{None, _Real, None}, None}        (* with custom distance function *)
},
"ColorToneMapping" -> {  {{_Image}, _Image},
  {{_Image, _Real}, _Image},       (* with key value *)
  {{_Image, _Real, _Real}, _Image} (* key and shoulder *)
},
"DominantColors" -> {  {{None}, _List},                   (* list of dominant color directives *)
  {{None, _Integer}, _List}         (* at most n colors *)
},
"FindEdgeColoring" -> {  {{_Graph}, _List},
  {{_Graph, _List}, _List}         (* with specified color set *)
},
"FindMatchingColor" -> {  {{_Image, None}, None},             (* single color -> closest match in image *)
  {{_Image, _List}, _List}         (* list of colors -> list of matches *)
},
"FindPlanarColoring" -> {  {{_Graph}, _List},
  {{_Graph, _List}, _List}
},
"FindVertexColoring" -> {  {{_Graph}, _List},
  {{_Graph, _List}, _List}
},
"Hue" -> {  {{_Real}, None},                    (* Hue[h] *)
  {{_Real, _Real, _Real}, None},    (* Hue[h, s, b] *)
  {{_Real, _Real, _Real, _Real}, None} (* Hue[h, s, b, a] *)
},
"LABColor" -> {  {{_Real, _Real, _Real}, None},
  {{_Real, _Real, _Real, _Real}, None} (* with opacity *)
},
"LCHColor" -> {  {{_Real, _Real, _Real}, None},
  {{_Real, _Real, _Real, _Real}, None}
},
"LUVColor" -> {  {{_Real, _Real, _Real}, None},
  {{_Real, _Real, _Real, _Real}, None}
},
"RGBColor" -> {  {{_Real}, None},                    (* RGBColor[gray] *)
  {{_Real, _Real, _Real}, None},
  {{_Real, _Real, _Real, _Real}, None} (* with alpha *)
},
"SurfaceColor" -> {  {{None}, None},                      (* diffuse color only *)
  {{None, None}, None},                (* diffuse + specular *)
  {{None, None, None}, None}           (* diffuse + specular + glow *)
},
"SystemColor" -> {  {{_String}, None}
},
"ThemeColor" -> {  {{_String}, None}
},
"ToColor" -> {  {{None, None}, None}                 (* ToColor[color, colorspace] *)
},
"XYZColor" -> {  {{_Real, _Real, _Real}, None},
  {{_Real, _Real, _Real, _Real}, None}
},
"VideoExtractTracks" -> {  {{_Video}, _List},                (* all tracks: video, audio, subtitle *)
  {{_Video, _String}, _List}       (* tracks of given type *)
},
"VideoFrameFold" -> {  {{None, _Video}, _Video},         (* f, video (first frame as init) *)
  {{None, None, _Video}, _Video},   (* f, init, video *)
  {{None, None, _Video, _Integer}, _Video}  (* with partition size n *)
},
"VideoIntervals" -> {  {{_Video, None}, _List},          (* time intervals satisfying criterion *)
  {{_Video, None, _Integer}, _List} (* with frame partition size *)
},
"VideoObjectTracking" -> {  {{_Video}, None}                   (* tracking annotation result *)
},
"VideoScreenCapture" -> {  {{}, _Video},                      (* capture from main screen *)
  {{None}, _Video},                  (* specified source *)
  {{None, None}, _Video}             (* source + async Dynamic target *)
},
"VideoStabilize" -> {  {{_Video}, _Video}
},
"VideoTimeStretch" -> {  {{_Video, None}, _Video}          (* video, stretch-spec/factor *)
},
"VideoTranscode" -> {  {{_Video, _String}, _Video},     (* convert to named format *)
  {{_Video, None}, _Video},         (* convert to service spec *)
  {{_List, None}, _List}            (* batch transcode list of videos *)
},
"VideoTranscribe" -> {  {{_Video}, _Video}               (* speech -> embedded subtitle track *)
},
"Random" -> {  {{}, _Real},                       (* Random[] uniform in [0,1]; deprecated *)
  {{_Symbol}, _Real},               (* Random[Real] *)
  {{_Symbol, None}, None}             (* Random[Integer/{Complex}, range] *)
},
"RandomArrayLayer" -> {  {{None}, None}                       (* net layer producing random array *)
},
"RandomGeneratorState" -> {  {{__}, None}                      (* internal pseudorandom generator state *)
},
"RandomInstance" -> {  {{None}, None},                      (* single instance of geometric scene etc. *)
  {{None, _Integer}, _List}         (* n instances *)
},
"RandomPointConfiguration" -> {  {{None, None}, None},                (* SpatialPointData from point process *)
  {{None, None, _Integer}, _List}   (* ensemble of n configurations *)
},
"RandomTime" -> {  {{}, None},                          (* random time of day; returns TimeObject *)
  {{None}, None},                      (* Random[time period / quantity] *)
  {{None, _Integer}, _List}         (* list of n random times *)
},
"RandomTree" -> {  {{_Integer}, None},                 (* single Tree with n nodes *)
  {{_Integer, _Integer}, _List}    (* list of k trees *)
},
"AdjacencyMatrix" -> {  {{_Graph}, _SparseArray}   (* graph -> sparse 0/1 adjacency matrix *)
},
"BlockDiagonalMatrix" -> {  {{_List}, None}              (* StructuredArray head, not plain List *)
},
"BoxMatrix" -> {  {{None}, _List},
  {{None, None}, _List}       (* r, optional w *)
},
"CauchyMatrix" -> {  {{_List}, None},             (* StructuredArray head *)
  {{_List, _List}, None}
},
"CircularOrthogonalMatrixDistribution" -> {  {{_Integer}, None}
},
"CircularQuaternionMatrixDistribution" -> {  {{_Integer}, None}
},
"CircularRealMatrixDistribution" -> {  {{_Integer}, None}
},
"CircularSymplecticMatrixDistribution" -> {  {{_Integer}, None}
},
"CircularUnitaryMatrixDistribution" -> {  {{_Integer}, None}
},
"CompanionMatrix" -> {  {{None}, _List},            (* polynomial expr or coefficient list *)
  {{None, None}, _List}       (* poly, variable *)
},
"ControllabilityMatrix" -> {  {{None}, _List}             (* StateSpaceModel or {a,b} *)
},
"CrossMatrix" -> {  {{None}, _List},
  {{None, None}, _List}       (* r, optional w *)
},
"DesignMatrix" -> {  {{None, None, None}, _List} (* data, fns, vars *)
},
"DiamondMatrix" -> {  {{None}, _List},
  {{None, None}, _List}       (* r, optional w *)
},
"DiskMatrix" -> {  {{None}, _List},
  {{None, None}, _List}       (* r, optional center *)
},
"DistanceMatrix" -> {  {{_List}, _List},
  {{_List, _List}, _List}   (* cross-distance between two lists *)
},
"EdgeCycleMatrix" -> {  {{_Graph}, _SparseArray}
},
"EulerMatrix" -> {  {{_List}, _List},          (* {alpha, beta, gamma} *)
  {{_List, _List}, _List}   (* angles, rotation ordering *)
},
"FindMatrixGameStrategies" -> {  {{None}, _List}             (* MatrixGame -> list of equilibrium strategies *)
},
"FourierDCTMatrix" -> {  {{_Integer}, _List},
  {{_Integer, _Integer}, _List} (* n, optional DCT type 1-4 *)
},
"FourierDSTMatrix" -> {  {{_Integer}, _List},
  {{_Integer, _Integer}, _List} (* n, optional DST type 1-4 *)
},
"FourierMatrix" -> {  {{_Integer}, _List}
},
"GaborMatrix" -> {  {{None, None}, _List},      (* r/dims, direction/frequency *)
  {{None, None, None}, _List} (* r, k, theta *)
},
"GaussianMatrix" -> {  {{None}, _List},            (* r or {r, sigma} *)
  {{None, None}, _List}       (* r, sigma *)
},
"GaussianOrthogonalMatrixDistribution" -> {  {{_Integer}, None},
  {{_Integer, _Real}, None}   (* n, optional variance *)
},
"GaussianSymplecticMatrixDistribution" -> {  {{_Integer}, None},
  {{_Integer, _Real}, None}
},
"GaussianUnitaryMatrixDistribution" -> {  {{_Integer}, None},
  {{_Integer, _Real}, None}
},
"HadamardMatrix" -> {  {{_Integer}, _List}
},
"HankelMatrix" -> {  {{_List}, _List},
  {{_List, _List}, _List}   (* first column, optional last row *)
},
"HermitianMatrix" -> {  {{_List}, None}              (* StructuredArray head *)
},
"HilbertMatrix" -> {  {{_Integer}, _List},
  {{_List}, _List}           (* n or {m, n} for rectangular *)
},
"IdentityMatrix" -> {  {{_Integer}, _List},
  {{_List}, _List}           (* n or {m, n} for rectangular *)
},
"IncidenceMatrix" -> {  {{_Graph}, _SparseArray}
},
"InverseWishartMatrixDistribution" -> {  {{_List, _Integer}, None}   (* scale matrix Sigma, deg-of-freedom nu *)
},
"JordanMatrix" -> {  {{None, _Integer}, _List}  (* eigenvalue, block size *)
},
"KirchhoffMatrix" -> {  {{_Graph}, _SparseArray}
},
"LowerTriangularMatrix" -> {  {{_List}, None}              (* StructuredArray head *)
},
"MatrixExp" -> {  {{_List}, _List},
  {{_List, None}, _List}     (* matrix, optional scalar t *)
},
"MatrixForm" -> {  {{None}, None}                (* typeset wrapper object *)
},
"MatrixFunction" -> {  {{None, _List}, _List}     (* f, matrix *)
},
"MatrixGame" -> {  {{_List}, None}              (* payoff matrix/tensor -> MatrixGame object *)
},
"MatrixGamePayoff" -> {  {{None, None}, None},         (* game, property *)
  {{None, None, None}, None}    (* game, strat1, strat2 *)
},
"MatrixLog" -> {  {{_List}, _List}
},
"MatrixMinimalPolynomial" -> {  {{_List, None}, None}        (* matrix, variable -> symbolic polynomial *)
},
"MatrixNormalDistribution" -> {  {{_List, _List}, None},     (* mean, covariance *)
  {{_List, _List, _List}, None} (* mean, row-cov, col-cov *)
},
"MatrixPolynomialValue" -> {  {{None, _List}, _List},    (* polynomial, matrix *)
  {{None, _List, None}, _List} (* polynomial, matrix, variable *)
},
"MatrixPower" -> {  {{_List, _Integer}, _List},
  {{_List, _Integer, None}, _List} (* matrix, n, optional modulus *)
},
"MatrixPropertyDistribution" -> {  {{None, None}, None}          (* expr, {x ~ dist} -> distribution object *)
},
"MatrixSymbol" -> {  {{_String}, None},
  {{_String, _List}, None},   (* name, {rows, cols} *)
  {{_String, _List, None}, None}, (* name, dims, type *)
  {{_String, _List, None, None}, None}
},
"MatrixTDistribution" -> {  {{_List, _List, _List}, None},     (* mean, row-scatter, col-scatter *)
  {{_List, _List, _List, None}, None} (* + degrees of freedom *)
},
"ObservabilityMatrix" -> {  {{None}, _List}             (* StateSpaceModel *)
},
"OrthogonalMatrix" -> {  {{_List}, None}              (* StructuredArray head *)
},
"OutputControllabilityMatrix" -> {  {{None}, _List}             (* StateSpaceModel *)
},
"PauliMatrix" -> {  {{_Integer}, _List}        (* n in {0,1,2,3} *)
},
"PermutationMatrix" -> {  {{None}, None},               (* Permutation or list -> StructuredArray *)
  {{None, _Integer}, None}     (* perm, explicit dimension n *)
},
"ReflectionMatrix" -> {  {{_List}, _List}           (* unit normal vector *)
},
"RollPitchYawMatrix" -> {  {{_List}, _List},          (* {roll, pitch, yaw} *)
  {{_List, _List}, _List}   (* angles, rotation ordering *)
},
"RotationMatrix" -> {  {{None}, _List},            (* angle -> 2D rotation matrix *)
  {{None, _List}, _List}     (* angle, axis -> 3D rotation matrix *)
},
"SavitzkyGolayMatrix" -> {  {{None, None}, _List},      (* r, polynomial-degree *)
  {{None, None, None}, _List} (* r, degree, derivative-order *)
},
"ScalingMatrix" -> {  {{_List}, _List},          (* scale vector -> homogeneous matrix *)
  {{None, _List}, _List}     (* scalar, direction vector *)
},
"ShenCastanMatrix" -> {  {{None, None}, _List}       (* r or {r1,r2}, sigma *)
},
"ShearingMatrix" -> {  {{None, _List, _List}, _List} (* angle, shear-direction, normal *)
},
"SymmetricMatrix" -> {  {{_List}, None}              (* StructuredArray head *)
},
"ToeplitzMatrix" -> {  {{_List}, _List},
  {{_List, _List}, _List}   (* first column, optional first row *)
},
"TransformationMatrix" -> {  {{None}, _List}             (* TransformationFunction -> 4x4 homogeneous *)
},
"UnitaryMatrix" -> {  {{_List}, None}              (* StructuredArray head *)
},
"UpperTriangularMatrix" -> {  {{_List}, None}              (* StructuredArray head *)
},
"VandermondeMatrix" -> {  {{_List}, None},             (* StructuredArray head *)
  {{_List, _Integer}, None}   (* nodes, explicit column count *)
},
"VerifyMatrixGameStrategy" -> {  {{None, None}, _?BooleanQ}  (* MatrixGame, strategy -> True/False *)
},
"WeightedAdjacencyMatrix" -> {  {{_Graph}, _SparseArray}
},
"WishartMatrixDistribution" -> {  {{_List, _Integer}, None}   (* scale matrix Sigma, deg-of-freedom nu *)
},
"Graph3D" -> {  {{}, _Graph},                               (* Graph3D[] — empty 3D graph *)
  {{_List}, _Graph},                         (* vertex list *)
  {{_List, _List}, _Graph},                 (* vertices, edges *)
  {{None}, _Graph}                            (* graph expr with 3D embedding *)
},
"GraphData" -> {  {{None}, None},                               (* entity; returns property assoc *)
  {{None, None}, None},                         (* entity, property; varies *)
  {{None, None, None}, None}
},
"GraphDensity" -> {  {{None}, _Real},                            (* edge density of g *)
  {{None, _String}, _Real}                   (* with "Directed"|"Undirected" *)
},
"GraphicsArray" -> {  {{_List}, _Graphics},                      (* 1-D array of graphics *)
  {{_List, _Integer}, _Graphics},           (* array, ncols *)
  {{None}, _Graphics}
},
"GraphicsColumn" -> {  {{_List}, _Graphics},                      (* vertical stack of graphics *)
  {{None}, _Graphics}
},
"GraphicsComplex" -> {  {{_List, None}, None},                       (* coords, primitives — head preserved *)
  {{_List, _List}, None}
},
"GraphicsGrid" -> {  {{_List}, _Graphics},                      (* 2-D grid of graphics *)
  {{None}, _Graphics}
},
"GraphicsGroup" -> {  {{_List}, None},                             (* group wrapper directive; head preserved *)
  {{None}, None}
},
"GraphicsRow" -> {  {{_List}, _Graphics},                      (* horizontal row of graphics *)
  {{None}, _Graphics}
},
"GraphLinkEfficiency" -> {  {{None}, _Real},                            (* global link efficiency scalar *)
  {{None, None}, _Real}                       (* between two vertices *)
},
"GraphPropertyDistribution" -> {  {{None, None}, None},                         (* returns a distribution object *)
  {{None, None, None}, None}
},
"GraphSum" -> {  {{_List}, _Graph},                         (* graph-theoretic sum of list *)
  {{None, __}, _Graph}                     (* variadic form *)
},
"GraphTree" -> {  {{None}, _Graph},                           (* spanning tree of a graph *)
  {{None, None}, _Graph}                      (* from specified root vertex *)
},
"VertexChromaticNumber" -> {  {{None}, _Integer}                          (* chromatic number; min colours *)
},
"VertexComponent" -> {  {{None, None}, _List},                      (* vertices reachable from vertex s *)
  {{None, None, _Integer}, _List}            (* with depth limit *)
},
"VertexInComponentGraph" -> {  {{None, None}, _Graph},                     (* induced subgraph of in-component *)
  {{None, None, _Integer}, _Graph}
},
"VertexOutComponentGraph" -> {  {{None, None}, _Graph},
  {{None, None, _Integer}, _Graph}
},
"EdgeChromaticNumber" -> {  {{None}, _Integer}                          (* chromatic index; min edge colours *)
},
"EdgeDetect" -> {  {{_Image}, _Image},                        (* edge-detected binary image *)
  {{_Image, None}, _Image},                  (* image, threshold *)
  {{None}, _Image},
  {{None, None}, _Image}
},
"EdgeForm" -> {  {{None}, None},                               (* style directive wrapper; not a value *)
  {{}, None}
},
"EdgeTaggedGraph" -> {  {{_List, _List}, _Graph},                 (* vertices, tagged-edge list *)
  {{None, None}, _Graph},                     (* graph, tag list *)
  {{None}, _Graph}
},
"EdgeTags" -> {  {{None}, _List},                            (* tags for all edges *)
  {{None, None}, _List}                       (* tags for specified edge list *)
},
"Date" -> {  {{}, _List},                                (* current {y,m,d,h,min,s} *)
  {{None}, _List}                             (* date-spec -> date list *)
},
"DateBounds" -> {  {{None}, _List},                            (* {min, max} DateObjects *)
  {{None, _String}, _List}                   (* with granularity string *)
},
"Dated" -> {  {{None, None}, None}                          (* symbolic Dated[val, date] wrapper *)
},
"DateDistribution" -> {  {{None, None}, None},                         (* parametric date distribution object *)
  {{None}, None}
},
"DatedUnit" -> {  {{None, None}, None}                          (* DatedUnit[unit, date] symbolic form *)
},
"DatePattern" -> {  {{_List}, None},                             (* string pattern for date format list *)
  {{_List, _List}, None}                      (* format list, options list *)
},
"DateScale" -> {  {{None, None}, None},                         (* scaling spec object for time axis *)
  {{None}, None}
},
"DateSelect" -> {  {{None, None}, None},                         (* filter TimeSeries; returns TimeSeries *)
  {{None, None, None}, None}
},
"DateString" -> {  {{}, _String},                              (* current date as string *)
  {{None}, _String},                          (* DateObject -> string *)
  {{None, None}, _String},                    (* date, format -> string *)
  {{_List}, _String},                        (* date list -> string *)
  {{_List, _List}, _String}                 (* date list, format list -> string *)
},
"TimeConstrained" -> {  {{None, None}, "_[1]"},                       (* expr, t — returns expr or $Aborted *)
  {{None, None, None}, "_[1]"}                  (* expr, t, failval *)
},
"TimeDistribution" -> {  {{None, None}, None},                         (* parametric time distribution object *)
  {{None}, None}
},
"TimeRemaining" -> {  {{}, _Real}                                 (* seconds remaining in TimeConstrained *)
},
"TimesBy" -> {  {{None, None}, None}                          (* x *= n; mutating; Null return *)
},
"TimeSeriesEvents" -> {  {{None}, _List},                            (* {{date, value}, ...} pairs *)
  {{None, None}, _List}
},
"TimeSeriesModel" -> {  {{None}, None},                               (* fitted TimeSeriesModelFit object *)
  {{None, None}, None}                          (* with process spec *)
},
"TimeSeriesStructure" -> {  {{None}, None}                                (* structural metadata; varies *)
},
"TimeSeriesSummary" -> {  {{None}, None}                                (* formatted summary panel *)
},
"TimeSystemConvert" -> {  {{None, None}, None},                         (* time value, target system *)
  {{None, None, None}, None}
},
"TimeWarpingCorrespondence" -> {  {{None, None}, _List},                      (* DTW index-pair correspondence list *)
  {{None, None, None}, _List}                 (* with distance function *)
},
"TimeWarpingDistance" -> {  {{None, None}, _Real},                      (* DTW scalar distance *)
  {{None, None, None}, _Real}
},
"TimeZone" -> {  {{}, None},                                   (* current offset (Real) or name (String) *)
  {{None}, None}                                (* for specified location *)
},
"AlgebraicNumberPolynomial" -> {  {{None, None}, _?NumericQ}                  (* minimal polynomial evaluated at var *)
},
"AugmentedSymmetricPolynomial" -> {  {{None, None}, _?NumericQ},                 (* augmented symmetric poly expression *)
  {{None, None, None}, _?NumericQ}
},
"ChromaticPolynomial" -> {  {{None, None}, _?NumericQ}                  (* chromatic polynomial in variable k *)
},
"CycleIndexPolynomial" -> {  {{None, None}, _?NumericQ}                  (* Pólya cycle index polynomial *)
},
"FlowPolynomial" -> {  {{None, None}, _?NumericQ}                  (* flow polynomial of graph *)
},
"NonCommutativePolynomialReduce" -> {  {{None, _List, _List}, _List}             (* {quotients, remainder} *)
},
"NonCommutativePolynomialReduction" -> {  {{None, _List, _List}, _List}
},
"PolynomialExtendedGCD" -> {  {{None, None, None}, _List}                 (* {gcd, {s, t}} Bézout coefficients *)
},
"PolynomialHermiteDecomposition" -> {  {{_List}, _List}                           (* {U, H} Hermite normal form pair *)
},
"PolynomialHermiteReduce" -> {  {{None, None, None}, _List}                 (* {rational-part, poly-part, remainder} *)
},
"PolynomialModel" -> {  {{None, None}, None},                         (* FittedModel object *)
  {{None, _List}, None}
},
"PolynomialReduction" -> {  {{None, _List, _List}, _List},            (* {quotients, remainder} *)
  {{None, None, _List}, _List}
},
"Polynomials" -> {  {{None}, None}                                (* symbolic polynomial set; varies *)
},
"PolynomialSmithDecomposition" -> {  {{_List}, _List}                           (* {U, S, V} Smith normal form triple *)
},
"PolynomialSmithReduce" -> {  {{None, _List}, _List}                     (* reduced Smith form list *)
},
"PowerSymmetricPolynomial" -> {  {{None, None}, _?NumericQ}                  (* power-sum symmetric polynomial *)
},
"SubresultantPolynomialRemainders" -> {  {{None, None, None}, _List}                 (* subresultant PRS remainder list *)
},
"SubresultantPolynomials" -> {  {{None, None, None}, _List}                 (* full subresultant polynomial chain *)
},
"SymmetricPolynomial" -> {  {{_Integer, _List}, _?NumericQ}           (* k-th elementary symmetric poly *)
},
"TuttePolynomial" -> {  {{None, _List}, _?NumericQ}                (* Tutte T(x,y) polynomial; g, {x,y} *)
},
"DecryptFile" -> {  {{_String, None}, _String},                (* file, key -> decrypted path *)
  {{_String, None, _String}, _String}       (* with explicit output path *)
},
"EncryptFile" -> {  {{_String, None}, _String},                (* file, key -> encrypted path *)
  {{_String, None, _String}, _String}
},
"File" -> {  {{_String}, None}                            (* File[path] symbolic wrapper *)
},
"FileBaseName" -> {  {{_String}, _String}                       (* filename without extension *)
},
"FileConvert" -> {  {{_String, _String}, _String},            (* src, format -> converted path *)
  {{_String, _String, _String}, _String}   (* src, format, dst -> dst path *)
},
"FileExtension" -> {  {{_String}, _String}                       (* extension without leading dot *)
},
"FileFormat" -> {  {{_String}, _String}                       (* detected format name string *)
},
"FileFormatProperties" -> {  {{_String}, None},                           (* format -> association of properties *)
  {{_String, None}, None}                      (* specific property *)
},
"FileHash" -> {  {{_String}, _Integer},
  {{_String, _String}, _Integer},           (* explicit hash method *)
  {{_String, _String, _String}, None}        (* output-type arg; String or ByteArray *)
},
"FileNameDepth" -> {  {{_String}, _Integer}                      (* number of path components *)
},
"FileNameJoin" -> {  {{_List}, _String}                         (* join path components into string *)
},
"FileNameSetter" -> {  {{None}, None}                                (* Dynamic UI element; no pure return *)
},
"FilePrint" -> {  {{_String}, None},                           (* print file contents; Null return *)
  {{_String, _Integer}, None}                 (* with line limit *)
},
"FileSize" -> {  {{_String}, None}                            (* Quantity[n,"Bytes"]; not in ret vocab *)
},
"FileSystemMap" -> {  {{None, None}, None},                         (* f applied over filesystem tree *)
  {{None, None, _String}, None}                (* with root path string *)
},
"FileSystemScan" -> {  {{None, None}, None},                         (* traverse filesystem; side-effects *)
  {{None, None, _String}, None}
},
"FileSystemTree" -> {  {{}, None},                                   (* Tree for current directory *)
  {{_String}, None},
  {{_String, _Integer}, None}                 (* with depth limit *)
},
"FileTemplate" -> {  {{_String}, None}                            (* TemplateObject from file path *)
},
"FileTemplateApply" -> {  {{None}, _String},                          (* apply template with no slots -> string *)
  {{None, _Association}, _String},           (* template, data assoc -> string *)
  {{None, _List}, _String}
},
"GenerateFileSignature" -> {  {{_String}, None},                           (* -> assoc with signature data *)
  {{_String, None}, None}                      (* with signing key *)
},
"NotebookFileName" -> {  {{}, _String}                               (* path of currently open notebook *)
},
"RemoteFile" -> {  {{None}, None}                                (* RemoteFile[url] symbolic wrapper *)
},
"RenameFile" -> {  {{_String, _String}, _String}             (* src, dst -> new path string *)
},
"SetFileDate" -> {  {{_String}, None},                           (* set mod date to now; Null *)
  {{_String, None}, None},                     (* set to given DateObject *)
  {{_String, None, _String}, None}            (* set specific property *)
},
"SetFileFormatProperties" -> {  {{_String, None}, None}                      (* side-effectful; Null return *)
},
"ToFileName" -> {  {{_List}, _String},                        (* deprecated FileNameJoin *)
  {{_String, _String}, _String}
},
"VerifyFileSignature" -> {  {{_String, None}, _?BooleanQ},             (* file, signature -> True/False *)
  {{_String, None, None}, _?BooleanQ}        (* with public key *)
},
"CloudDirectory" -> {  {{}, _String},                              (* current cloud working directory *)
  {{_String}, _String}
},
"Directory" -> {  {{}, _String}                               (* current local working directory *)
},
"DirectoryName" -> {  {{_String}, _String},                      (* parent directory of path *)
  {{_String, _Integer}, _String}            (* n levels up *)
},
"DirectoryStack" -> {  {{}, _List}                                 (* list of directory strings *)
},
"NotebookDirectory" -> {  {{}, _String}                               (* parent directory of current notebook *)
},
"PacletDirectoryLoad" -> {  {{_String}, None},                           (* add dir to paclet search; Null *)
  {{_List}, None}
},
"PacletDirectoryUnload" -> {  {{_String}, None},
  {{_List}, None}
},
"RenameDirectory" -> {  {{_String, _String}, _String}             (* src, dst -> new path *)
},
"SetCloudDirectory" -> {  {{_String}, _String}                       (* set cloud dir, return path *)
},
"ImportByteArray" -> {  {{None, _String}, None},                     (* ByteArray, format -> depends on format *)
  {{None, _String, _List}, None}
},
"ImportedObject" -> {  {{None}, None}                                (* symbolic import-result wrapper *)
},
"ExportByteArray" -> {  {{None, _String}, None},                     (* data, format -> ByteArray (not in vocab) *)
  {{None, _String, _List}, None}
},
"ExportForm" -> {  {{None, _String}, None}                      (* expr, format -> ExportForm wrapper *)
},
"ExportString" -> {  {{None, _String}, _String},                (* data, format -> string *)
  {{None, _String, _List}, _String}
},
"AssociateTo" -> {  {{None, None}, None}                          (* mutating update; side-effectful *)
},
"AssociationComap" -> {  {{None, _Association}, _Association},      (* reverse key mapping via f *)
  {{None, None}, _Association}
},
"AssociationThread" -> {  {{_List, _List}, _Association},           (* keys list, values list *)
  {{_Rule}, _Association},                   (* keys -> values rule form *)
  {{_List}, _Association}                    (* list of key->val pairs *)
},
"Key" -> {  {{None}, None}                                (* Key[k] part-selector wrapper *)
},
"KeyComplement" -> {  {{_List}, _Association},                   (* keys in first but not rest *)
  {{_List, _List}, _Association}
},
"KeyDropFrom" -> {  {{None, None}, None}                          (* mutating drop; side-effectful *)
},
"KeyIntersection" -> {  {{_List}, _Association},                   (* shared keys across all *)
  {{_List, _List}, _Association}
},
"Keys" -> {  {{_Association}, _List},                   (* list of keys *)
  {{_List}, _List},                          (* list-of-assocs -> list-of-key-lists *)
  {{None}, _List}
},
"KeyUnion" -> {  {{_List}, _Association},                   (* union of keys with merged values *)
  {{_List, _List}, _Association}
},
"KeyValuePattern" -> {  {{_List}, None},                             (* structural pattern wrapper *)
  {{None}, None}
},
"AlgebraicNumber" -> {  {{None, _List}, None}                        (* AlgebraicNumber[root, coeffs] head *)
},
"AlgebraicNumberDenominator" -> {  {{None}, _Integer}                          (* denominator of algebraic number form *)
},
"AlgebraicNumberNorm" -> {  {{None}, _?NumericQ}                        (* field norm of algebraic number *)
},
"AlgebraicNumberTrace" -> {  {{None}, _?NumericQ}                        (* field trace of algebraic number *)
},
"AlternatingHarmonicNumber" -> {  {{_Integer}, _?NumericQ},                  (* alternating harmonic sum H_n^alt *)
  {{None}, _?NumericQ}
},
"CatalanNumber" -> {  {{_Integer}, _Integer},                    (* n-th Catalan number *)
  {{None}, _?NumericQ}
},
"ChampernowneNumber" -> {  {{}, _Real},                                (* base-10 Champernowne constant *)
  {{_Integer}, _Real}                        (* in given base *)
},
"DigitalSignature" -> {  {{None}, None}                                (* DigitalSignature[...] head/wrapper *)
},
"DigitSum" -> {  {{_Integer}, _Integer},                    (* sum of decimal digits *)
  {{_Integer, _Integer}, _Integer}          (* in given base *)
},
"FrobeniusNumber" -> {  {{_List}, _Integer}                        (* largest non-representable integer *)
},
"FromLunationNumber" -> {  {{_Integer}, None}                           (* lunation index -> DateObject *)
},
"GenerateDigitalSignature" -> {  {{None, None}, None}                          (* data, key -> DigitalSignature object *)
},
"HyperHarmonicNumber" -> {  {{_Integer, _Integer}, _?NumericQ},       (* H(n,r) hyperharmonic number *)
  {{None, None}, _?NumericQ}
},
"IntegerDigits" -> {  {{_Integer}, _List},                       (* decimal digit list *)
  {{_Integer, _Integer}, _List},            (* in given base *)
  {{_Integer, _Integer, _Integer}, _List}  (* base, with length/padding *)
},
"IntegerLength" -> {  {{_Integer}, _Integer},                    (* number of decimal digits *)
  {{_Integer, _Integer}, _Integer}          (* in given base *)
},
"IntegerString" -> {  {{_Integer}, _String},                     (* integer -> decimal string *)
  {{_Integer, _Integer}, _String},          (* in given base *)
  {{_Integer, _Integer, _Integer}, _String} (* base, with padding *)
},
"LetterNumber" -> {  {{_String}, _Integer},                     (* position in default alphabet *)
  {{_List}, _List},                          (* list of letter strings -> positions *)
  {{_String, None}, _Integer}               (* in given alphabet *)
},
"LunationNumber" -> {  {{None}, _Integer}                          (* DateObject -> lunation index *)
},
"MorphologicalEulerNumber" -> {  {{_Image}, _Integer},                      (* topological Euler number of binary image *)
  {{_Image, _Integer}, _Integer}            (* with connectivity spec *)
},
"MultipleHarmonicNumber" -> {  {{None, _Integer}, _?NumericQ},            (* multiple harmonic sum *)
  {{None, None}, _?NumericQ}
},
"NumberCompose" -> {  {{_List, _List}, _?NumericQ}              (* compose from {digits, bases} lists *)
},
"NumberDecompose" -> {  {{None, _List}, _List}                     (* {coeff, base} decomposition *)
},
"NumberExpand" -> {  {{None, _Integer}, _List},                 (* digit expansion in base b *)
  {{None}, _List}
},
"NumberFieldClassNumber" -> {  {{_List}, _Integer}                        (* class number of Q(alpha) *)
},
"NumberFieldDiscriminant" -> {  {{_List}, _Integer}                        (* discriminant of number field *)
},
"NumberFieldFundamentalUnits" -> {  {{_List}, _List}                           (* list of fundamental unit algebraic numbers *)
},
"NumberFieldIntegralBasis" -> {  {{_List}, _List}                           (* integral basis elements *)
},
"NumberFieldNormRepresentatives" -> {  {{_List, _Integer}, _List}                (* elements of given norm *)
},
"NumberFieldRegulator" -> {  {{_List}, _Real}                           (* Dirichlet regulator *)
},
"NumberFieldRootsOfUnity" -> {  {{_List}, _List}                           (* {n, root} for roots of unity *)
},
"NumberFieldSignature" -> {  {{_List}, _List}                           (* {r1, r2}: real, complex-pair counts *)
},
"PerfectNumber" -> {  {{_Integer}, _Integer}                     (* n-th perfect number *)
},
"PolygonalNumber" -> {  {{_Integer, _Integer}, _Integer},         (* k-gonal, n-th term *)
  {{_Integer}, _Integer}                     (* triangular by default *)
},
"RealAbs" -> {  {{_Real}, _Real},                          (* machine-precision absolute value *)
  {{None}, _?NumericQ}
},
"RealDigits" -> {  {{None}, _List},                            (* {digits, exponent} pair *)
  {{None, _Integer}, _List},                 (* in given base *)
  {{None, _Integer, _Integer}, _List}       (* base, precision *)
},
"RealExponent" -> {  {{None}, _Integer},                         (* base-10 exponent *)
  {{None, _Integer}, _Integer}               (* in given base *)
},
"RealSign" -> {  {{None}, _Integer}                          (* -1, 0, or 1 *)
},
"ToNumberField" -> {  {{None, None}, None},                         (* expr, field -> AlgebraicNumber *)
  {{None, _List}, None}
},
"VerifyDigitalSignature" -> {  {{None, None}, _?BooleanQ},                 (* data, sig -> True/False *)
  {{None, None, None}, _?BooleanQ}            (* with explicit public key *)
},
"AdjacentMeshCells" -> {  {{None, None, None}, _List}
},
"ArrayMesh" -> {  {{None}, None}
},
"Ball" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"BooleanRegion" -> {  {{None, _List}, None}
},
"BoundaryDiscretizeRegion" -> {  {{None}, None},
  {{None, None}, None}
},
"BoundaryMesh" -> {  {{None}, None}
},
"BoundaryMeshRegion" -> {  {{___}, None}
},
"BoundingRegion" -> {  {{None}, None},
  {{None, None}, None}
},
"CanonicalizeRegion" -> {  {{None}, None}
},
"CantorMesh" -> {  {{None}, None},
  {{None, None}, None}
},
"ConcaveHullMesh" -> {  {{_List}, None},
  {{_List, None}, None},
  {{_List, None, None}, None}
},
"Cone" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"ConicHullRegion" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"ConnectedMeshComponents" -> {  {{None}, _List}
},
"ConvexHullMesh" -> {  {{None}, None}
},
"ConvexHullRegion" -> {  {{None}, None}
},
"CSGRegion" -> {  {{None}, None},
  {{None, None}, None}
},
"CSGRegionTree" -> {  {{None}, None}
},
"DelaunayMesh" -> {  {{_List}, None}
},
"DimensionalMeshComponents" -> {  {{None}, _List}
},
"DiscretizeRegion" -> {  {{None}, None},
  {{None, None}, None}
},
"EmptyRegion" -> {  {{None}, None}
},
"FindMeshDefects" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"FindRegionTransform" -> {  {{None, None}, None}
},
"FullRegion" -> {  {{None}, None}
},
"GeoBoundsRegion" -> {  {{None}, None},
  {{None, None}, None}
},
"GeoBoundsRegionBoundary" -> {  {{None}, None},
  {{None, None}, None}
},
"GeoVisibleRegion" -> {  {{None}, None}
},
"GeoVisibleRegionBoundary" -> {  {{None}, None}
},
"GradientFittedMesh" -> {  {{_List}, None}
},
"HighlightMesh" -> {  {{None, None}, None}
},
"HighlightRegion" -> {  {{None, None}, None}
},
"ImplicitRegion" -> {  {{None, _List}, None}
},
"InverseTransformedRegion" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"MengerMesh" -> {  {{None}, None},
  {{None, None}, None}
},
"MeshCellIndex" -> {  {{None, None}, _List}
},
"MeshCells" -> {  {{None, None}, _List}
},
"MeshConnectivityGraph" -> {  {{None}, _Graph},
  {{None, None}, _Graph},
  {{None, None, None}, _Graph}
},
"MeshCoordinates" -> {  {{None}, _List}
},
"MeshPrimitives" -> {  {{None, None}, _List}
},
"MeshRegion" -> {  {{___}, None}
},
"MoleculeMesh" -> {  {{None}, None},
  {{None, None}, None}
},
"NearestMeshCells" -> {  {{__}, _List}
},
"ParametricRegion" -> {  {{_List, None}, None}
},
"ReconstructionMesh" -> {  {{_List}, None}
},
"Region" -> {  {{None}, None}
},
"RegionBinarize" -> {  {{None, None, None}, _Image},
  {{None, None, None, None}, _Image}
},
"RegionBoundary" -> {  {{None}, None}
},
"RegionBounds" -> {  {{None}, _List},
  {{None, None}, _List}
},
"RegionCentroid" -> {  {{None}, _List}
},
"RegionCongruent" -> {  {{None, None}, _?BooleanQ}
},
"RegionConvert" -> {  {{None, None}, None}
},
"RegionDifference" -> {  {{None, None}, None}
},
"RegionDilation" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"RegionDimension" -> {  {{None}, _Integer}
},
"RegionDisjoint" -> {  {{__}, _?BooleanQ}
},
"RegionDistance" -> {  {{None}, None},
  {{None, None}, _?NumericQ}
},
"RegionDistanceFunction" -> {  {{__}, None}
},
"RegionEmbeddingDimension" -> {  {{None}, _Integer}
},
"RegionEqual" -> {  {{__}, _?BooleanQ}
},
"RegionErosion" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"RegionFarthestDistance" -> {  {{None, None}, _?NumericQ}
},
"RegionFit" -> {  {{_List, None}, None},
  {{_List, None, None}, None}
},
"RegionGaussianCurvature" -> {  {{None, None}, _?NumericQ}
},
"RegionHausdorffDistance" -> {  {{None, None}, _?NumericQ}
},
"RegionImage" -> {  {{None}, _Image},
  {{None, None}, _Image}
},
"RegionIntersection" -> {  {{__}, None}
},
"RegionMaxCurvature" -> {  {{None, None}, _?NumericQ}
},
"RegionMeanCurvature" -> {  {{None, None}, _?NumericQ}
},
"RegionMeasure" -> {  {{None}, _?NumericQ},
  {{None, None}, _?NumericQ},
  {{None, None, None}, _?NumericQ}
},
"RegionMember" -> {  {{None}, None},
  {{None, None}, _?BooleanQ}
},
"RegionMemberFunction" -> {  {{__}, None}
},
"RegionMinCurvature" -> {  {{None, None}, _?NumericQ}
},
"RegionMoment" -> {  {{None, None}, _?NumericQ}
},
"RegionNearest" -> {  {{None}, None},
  {{None, None}, _List}
},
"RegionNearestFunction" -> {  {{__}, None}
},
"RegionProduct" -> {  {{__}, None}
},
"RegionResize" -> {  {{None, None}, None}
},
"RegionSimilar" -> {  {{None, None}, _?BooleanQ}
},
"RegionSymmetricDifference" -> {  {{__}, None}
},
"RegionUnion" -> {  {{__}, None}
},
"RegionWithin" -> {  {{None, None}, _?BooleanQ}
},
"RepairMesh" -> {  {{None}, None},
  {{None, None}, None}
},
"RipleyRassonRegion" -> {  {{None}, None}
},
"ShellRegion" -> {  {{None}, None},
  {{None, None}, None}
},
"SierpinskiMesh" -> {  {{None}, None},
  {{None, None}, None}
},
"SignedRegionDistance" -> {  {{None}, None},
  {{None, None}, _?NumericQ}
},
"SimplifyMesh" -> {  {{None}, None},
  {{None, None}, None}
},
"SmoothMesh" -> {  {{None}, None}
},
"SubdivisionRegion" -> {  {{___}, None}
},
"TransformedRegion" -> {  {{None, None}, None}
},
"TriangulateMesh" -> {  {{None}, None}
},
"VoronoiMesh" -> {  {{_List}, None},
  {{_List, None}, None}
},
"ClassifierFunction" -> {  {{__}, None}
},
"ClassifierInformation" -> {  {{None}, _Association},
  {{None, None}, _Association}
},
"ClassifierMeasurements" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"ClassifierMeasurementsObject" -> {  {{__}, None}
},
"LayeredGraph" -> {  {{None}, _Graph},
  {{None, None}, _Graph}
},
"LayeredGraph3D" -> {  {{None}, _Graph},
  {{None, None}, _Graph}
},
"NetAppend" -> {  {{None, None}, None}
},
"NetArray" -> {  {{}, None},
  {{None}, None}
},
"NetArrayLayer" -> {  {{}, None},
  {{None}, None}
},
"NetBidirectionalOperator" -> {  {{None}, None},
  {{None, None}, None}
},
"NetChain" -> {  {{__}, None}
},
"NetDecoder" -> {  {{None}, None}
},
"NetDelete" -> {  {{None, None}, None}
},
"NetDrop" -> {  {{None, None}, None}
},
"NetEncoder" -> {  {{None}, None}
},
"NetExternalObject" -> {  {{__}, None}
},
"NetExtract" -> {  {{None}, None},
  {{None, None}, None}
},
"NetFlatten" -> {  {{None}, None},
  {{None, _Integer}, None}
},
"NetFoldOperator" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, _List}, None}
},
"NetGANOperator" -> {  {{_List}, None},
  {{_List, None}, None}
},
"NetGraph" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, __}, None}
},
"NetInformation" -> {  {{None}, _Association},
  {{None, None}, _Association}
},
"NetInitialize" -> {  {{None}, None},
  {{None, None}, None}
},
"NetInsert" -> {  {{None, None, None}, None}
},
"NetInsertSharedArrays" -> {  {{None}, None},
  {{None, None}, None}
},
"NetJoin" -> {  {{__}, None}
},
"NetMapOperator" -> {  {{None}, None}
},
"NetMapThreadOperator" -> {  {{None}, None},
  {{None, None}, None}
},
"NetMeasurements" -> {  {{None, None, None}, None}
},
"NetModel" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"NetNestOperator" -> {  {{None, _Integer}, None}
},
"NetPairEmbeddingOperator" -> {  {{None}, None},
  {{None, None}, None}
},
"NetPort" -> {  {{None}, None},
  {{None, None}, None}
},
"NetPortGradient" -> {  {{None}, None}
},
"NetPrepend" -> {  {{None, None}, None}
},
"NetRename" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"NetReplace" -> {  {{None, None}, None}
},
"NetReplacePart" -> {  {{None, None}, None}
},
"NetSharedArray" -> {  {{None}, None}
},
"NetStateObject" -> {  {{None}, None},
  {{None, None}, None}
},
"NetTake" -> {  {{None, None}, None}
},
"NetTrain" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"NetTrainResultsObject" -> {  {{__}, None}
},
"NetUnfold" -> {  {{None}, None}
},
"NetworkPacketCapture" -> {  {{}, None},
  {{None}, None}
},
"NetworkPacketRecording" -> {  {{None}, None},
  {{None, None}, None}
},
"NetworkPacketTrace" -> {  {{None}, None},
  {{None, None}, None}
},
"PredictorFunction" -> {  {{__}, None}
},
"PredictorInformation" -> {  {{None}, _Association},
  {{None, None}, _Association}
},
"PredictorMeasurements" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"PredictorMeasurementsObject" -> {  {{__}, None}
},
"SequencePredictorFunction" -> {  {{__}, None}
},
"AbelianGroup" -> {  {{_List}, None}
},
"Abort" -> {  {{}, None}
},
"AbortKernels" -> {  {{}, None}
},
"AbortProtect" -> {  {{None}, "_[1]"}
},
"AbortScheduledTask" -> {  {{None}, None}
},
"AbsoluteCurrentValue" -> {  {{None}, None},
  {{None, None}, None}
},
"AbsoluteDashing" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"AbsoluteOptions" -> {  {{None}, _List},
  {{None, None}, _List}
},
"AbsolutePointSize" -> {  {{None}, None}
},
"AbsoluteThickness" -> {  {{None}, None}
},
"AbsoluteTime" -> {  {{}, _Real},
  {{None}, _Real}
},
"AcousticAbsorbingValue" -> {  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"AcousticImpedanceValue" -> {  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"AcousticNormalVelocityValue" -> {  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"AcousticPDEComponent" -> {  {{None, None}, None}
},
"AcousticPressureCondition" -> {  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"AcousticRadiationValue" -> {  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"AcousticSoundHardValue" -> {  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"AcousticSoundSoftCondition" -> {  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"ActionMenu" -> {  {{None, _List}, None}
},
"Activate" -> {  {{None}, "_[1]"},
  {{None, None}, "_[1]"}
},
"ActiveClassification" -> {  {{None, None}, None}
},
"ActiveClassificationObject" -> {  {{__}, None}
},
"ActivePrediction" -> {  {{None, None}, None}
},
"ActivePredictionObject" -> {  {{__}, None}
},
"AddToSearchIndex" -> {  {{None, None}, None}
},
"AddToVectorDatabase" -> {  {{None, None}, None}
},
"AddUsers" -> {  {{None, _List}, None}
},
"AdjacencyGraph" -> {  {{None}, _Graph},
  {{None, None}, _Graph}
},
"AdjustmentBox" -> {  {{None, None}, None}
},
"AdjustTimeSeriesForecast" -> {  {{None, None, None}, None}
},
"AdministrativeDivisionData" -> {  {{__}, None}
},
"AffineHalfSpace" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"AffineSpace" -> {  {{None}, None},
  {{None, None}, None}
},
"AffineStateSpaceModel" -> {  {{None}, None},
  {{None, None}, None}
},
"AggregatedEntityClass" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"AggregateRows" -> {  {{None}, _List},
  {{None, None}, _List},
  {{None, None, None}, _List}
},
"AggregationLayer" -> {  {{None}, None},
  {{None, None}, None}
},
"AircraftData" -> {  {{__}, None}
},
"AirportData" -> {  {{__}, None}
},
"AirPressureData" -> {  {{__}, None}
},
"AirSoundAttenuation" -> {  {{None, None}, _?NumericQ},
  {{None, None, None, None}, _?NumericQ}
},
"AirTemperatureData" -> {  {{__}, None}
},
"Alphabet" -> {  {{}, _List},
  {{_String}, _List}
},
"AlternatingGroup" -> {  {{_Integer}, None}
},
"Alternatives" -> {  {{__}, None}
},
"AmbientLight" -> {  {{None}, None}
},
"AnatomyData" -> {  {{__}, None}
},
"AnatomyForm" -> {  {{}, None},
  {{None}, None}
},
"AnatomyStyling" -> {  {{}, None},
  {{None}, None}
},

(* ═══ Wave 4: General functions A-B ═══ *)
"AndersonDarlingTest" -> {  {{None}, None},                                       (* single-sample *)
  {{None, None}, None},                                 (* against named distribution *)
  {{None, None, None}, None}                            (* two-sample or extra opts *)
},
"AngerJ" -> {  {{None, None}, _?NumericQ},                         (* AngerJ[nu, z] *)
  {{None, None, None}, _?NumericQ}                    (* AngerJ[nu, z, n] derivative order *)
},
"AngleBisector" -> {  {{_List}, None},                                     (* AngleBisector[{p1,p2,p3}] *)
  {{_List, None}, None}                                (* with exterior/interior spec *)
},
"AngleBracket" -> {  {{__}, None}                                       (* typesetting / ket form, variadic *)
},
"AngularGauge" -> {  {{None}, None},                                       (* single value *)
  {{__}, None}                                       (* value + range + opts *)
},
"Animate" -> {  {{None, _List}, None},                               (* Animate[expr, {t,tmin,tmax}] *)
  {{None, _List, _List}, None}                        (* multiple iterators *)
},
"AnimatedImage" -> {  {{None}, _Image}                                    (* AnimatedImage[frames] *)
},
"AnimationVideo" -> {  {{None, _List}, _Video}                            (* AnimationVideo[frames, fps_spec] *)
},
"Animator" -> {  {{None}, None},                                       (* Animator[x] *)
  {{None, _List}, None},                               (* Animator[x, {min,max}] *)
  {{None, _List, None}, None}                          (* Animator[x, {min,max,step}, ...] *)
},
"Annotate" -> {  {{None, None}, None}                                  (* Annotate[obj, annotation] *)
},
"Annotation" -> {  {{None, None}, None},                                 (* Annotation[expr, value] *)
  {{None, None, None}, None}                            (* Annotation[expr, value, tag] *)
},
"AnnotationDelete" -> {  {{None}, None},                                       (* delete all annotations *)
  {{None, None}, None}                                  (* delete by key *)
},
"AnnotationKeys" -> {  {{None}, _List}                                     (* returns list of annotation keys *)
},
"AnnotationValue" -> {  {{None, None}, None},                                 (* AnnotationValue[{obj,key}] *)
  {{None, _List}, None}                                (* multiple keys *)
},
"Annuity" -> {  {{None, None}, None},                                 (* Annuity[pmt, t] *)
  {{None, None, None}, None}                            (* Annuity[pmt, t, q] compounding *)
},
"AnnuityDue" -> {  {{None, None}, None},                                 (* AnnuityDue[pmt, t] *)
  {{None, None, None}, None}                            (* AnnuityDue[pmt, t, q] *)
},
"Annulus" -> {  {{}, None},                                           (* Annulus[] unit *)
  {{_List}, None},                                     (* Annulus[{r1,r2}] *)
  {{_List, _List}, None},                             (* Annulus[center, {r1,r2}] *)
  {{_List, _List, _List}, None}                      (* Annulus[center, {r1,r2}, {θ1,θ2}] *)
},
"AnomalyDetection" -> {  {{None}, None},                                       (* AnomalyDetection[data] → AnomalyDetectorFunction *)
  {{__}, None}                                       (* with options *)
},
"AnomalyDetectorFunction" -> {  {{None}, None},                                       (* applied to single datum *)
  {{__}, None}                                       (* applied to dataset / options *)
},
"Anticommutator" -> {  {{None, None}, None},                                 (* Anticommutator[a,b] = a.b+b.a *)
  {{None, None, None}, None}                            (* with NonCommutativeMultiply variable *)
},
"Antihermitian" -> {  {{_List}, None}                                      (* Antihermitian[{m,n}] index symmetry *)
},
"Antisymmetric" -> {  {{_List}, None}                                      (* Antisymmetric[{i1,i2,...}] *)
},
"Antonyms" -> {  {{_String}, _List}                                 (* Antonyms["word"] → list of strings *)
},
"AnyOrder" -> {  {{__}, None}                                       (* AnyOrder[p1,p2,...] sequential pattern *)
},
"AnySubset" -> {  {{None}, None},                                       (* AnySubset[pattern] *)
  {{None, None}, None}                                  (* AnySubset[pattern, n] *)
},
"ApartSquareFree" -> {  {{None}, _?NumericQ},                               (* ApartSquareFree[poly] *)
  {{None, _Symbol}, _?NumericQ}                      (* ApartSquareFree[poly, x] *)
},
"APIFunction" -> {  {{None, None}, None},                                 (* APIFunction[params, body] *)
  {{None, None, None}, None}                            (* APIFunction[params, body, fmt] *)
},
"AppellF1" -> {  {{None, None, None, None, None, None}, _?NumericQ}  (* AppellF1[a,b1,b2,c,x,y] *)
},
"AppellF2" -> {  {{None, None, None, None, None, None, None}, _?NumericQ}  (* AppellF2[a,b1,b2,c1,c2,x,y] *)
},
"AppellF3" -> {  {{None, None, None, None, None, None, None}, _?NumericQ}  (* AppellF3[a1,a2,b1,b2,c,x,y] *)
},
"AppellF4" -> {  {{None, None, None, None, None, None}, _?NumericQ}  (* AppellF4[a,b,c1,c2,x,y] *)
},
"AppendLayer" -> {  {{__}, None}                                       (* options only: Element, Input, Output *)
},
"Application" -> {  {{None, None}, None}                                  (* Application[f, args] *)
},
"ApplyReaction" -> {  {{None}, None},                                       (* ApplyReaction[rxn] *)
  {{None, None}, None},                                 (* ApplyReaction[rxn, mols] *)
  {{None, None, None}, None}                            (* ApplyReaction[rxn, mols, outspec] *)
},
"ApplySides" -> {  {{None, None}, None}                                  (* ApplySides[f, eq] both sides of eqn *)
},
"ApplyTo" -> {  {{None, None}, None}                                  (* x //= f  augmented assignment *)
},
"ArcCosDegrees" -> {  {{None}, _?NumericQ}                                (* ArcCosDegrees[x] → degrees *)
},
"ArcCotDegrees" -> {  {{None}, _?NumericQ}                                (* ArcCotDegrees[x] → degrees *)
},
"ArcCscDegrees" -> {  {{None}, _?NumericQ}                                (* ArcCscDegrees[x] → degrees *)
},
"ArcCsch" -> {  {{None}, _?NumericQ}                                (* ArcCsch[z] inverse hyperbolic csc *)
},
"ArcCurvature" -> {  {{_List, None}, _?NumericQ},                       (* ArcCurvature[{x(t),y(t)}, t] *)
  {{_List, None, None}, _?NumericQ}                  (* with evaluation point *)
},
"ARCHProcess" -> {  {{None}, None},                                       (* ARCHProcess[a] *)
  {{None, _List}, None},                               (* ARCHProcess[a, {q1,...}] *)
  {{None, _List, None}, None}                          (* with init condition *)
},
"ArcLength" -> {  {{None}, _?NumericQ},                               (* ArcLength[region] *)
  {{None, _List}, _?NumericQ},                       (* ArcLength[curve, {t,tmin,tmax}] *)
  {{None, _List, None}, _?NumericQ}                  (* parametric with metric *)
},
"ArcSecDegrees" -> {  {{None}, _?NumericQ}                                (* ArcSecDegrees[x] → degrees *)
},
"ArcSech" -> {  {{None}, _?NumericQ}                                (* ArcSech[z] inverse hyperbolic sec *)
},
"ArcSinDegrees" -> {  {{None}, _?NumericQ}                                (* ArcSinDegrees[x] → degrees *)
},
"ArcSinDistribution" -> {  {{}, None},                                           (* ArcSinDistribution[] on [0,1] *)
  {{_List}, None}                                      (* ArcSinDistribution[{a,b}] *)
},
"ArcTanDegrees" -> {  {{None}, _?NumericQ}                                (* ArcTanDegrees[x] → degrees *)
},
"Area" -> {  {{None}, _?NumericQ},                               (* Area[region] *)
  {{None, _List}, _?NumericQ},                       (* Area[{x,y,z}[u,v], {u,...}] *)
  {{None, _List, _List}, _?NumericQ},               (* two-parameter surface *)
  {{None, _List, _List, None}, _?NumericQ}          (* with assumptions/metric *)
},
"ArgMax" -> {  {{None, None}, None},                                 (* ArgMax[f, x] *)
  {{None, None, None}, None}                            (* ArgMax[{f,cons}, {x,x0}] *)
},
"ArgMin" -> {  {{None, None}, None},                        (* ArgMin[f, {x, dom}] — symbolic extremal point, head varies *)
  {{None, None, None}, None}                   (* multi-variable form *)
},
"ArgumentsOptions" -> {  {{None}, _List},                           (* {positionalArgs, options} *)
  {{None, _Integer}, _List}                 (* with expected arg count *)
},
"ARIMAProcess" -> {  {{None, None, None}, None},                  (* ARIMAProcess[{ar}, d, {ma}] — stochastic process object *)
  {{None, None, None, None}, None}             (* with variance *)
},
"ARMAProcess" -> {  {{None, None}, None},                        (* ARMAProcess[{ar}, {ma}] — stochastic process object *)
  {{None, None, None}, None}                   (* with variance *)
},
"Around" -> {  {{None, None}, None},                        (* Around[val, err] — Around object *)
  {{None, _List}, None}                       (* Around[val, {minus, plus}] *)
},
"AroundReplace" -> {  {{None, _Rule}, "_[1]"},                    (* propagates Around through expr with one rule *)
  {{None, _List}, "_[1]"},                    (* list of replacement rules *)
  {{None, _Association}, "_[1]"}              (* association of replacements *)
},
"ARProcess" -> {  {{None}, None},                              (* ARProcess[{ar}] — stochastic process object *)
  {{None, None}, None}                         (* with variance *)
},
"ARPublish" -> {  {{None}, None},                              (* ARPublish[model] — side-effecting publish; result varies *)
  {{None, None}, None}
},
"Array" -> {  {{None, _Integer}, _List},                (* Array[f, n] *)
  {{None, _List}, _List},                   (* Array[f, {m,n,...}] *)
  {{None, _Integer, _Integer}, _List},     (* Array[f, n, r] *)
  {{None, _List, None}, _List}              (* Array[f, dims, origin] *)
},
"ArrayComponents" -> {  {{None}, _List},                           (* connected components of a matrix *)
  {{None, _Integer}, _List},                (* with connectivity k *)
  {{None, _List}, _List}                    (* with explicit connectivity spec *)
},
"ArrayDot" -> {  {{None, None}, _List},                     (* ArrayDot[a, b] *)
  {{None, None, None}, _List}                (* ArrayDot[a, b, c, ...] *)
},
"ArrayExpand" -> {  {{None}, "_[1]"},                            (* expands nested array operations *)
  {{None, None}, "_[1]"}                       (* with options/level spec *)
},
"ArrayFilter" -> {  {{None, None, None}, _List},               (* ArrayFilter[f, array, r] *)
  {{None, None, _List}, _List}              (* ArrayFilter[f, array, {r1, r2}] *)
},
"ArrayReduce" -> {  {{None, None, None}, _List},               (* ArrayReduce[f, array, k] *)
  {{None, None, _List}, _List}              (* reduce along listed dimensions *)
},
"ArrayReshape" -> {  {{None, _List}, _List},                   (* ArrayReshape[a, {m,n}] *)
  {{None, _List, None}, _List}              (* with padding element *)
},
"ArrayRules" -> {  {{None}, _List},                           (* ArrayRules[SparseArray] → list of rules *)
  {{None, None}, _List}                      (* with explicit background value *)
},
"Arrays" -> {  {{None}, None},                              (* Arrays[{m,n}] — type descriptor *)
  {{None, None}, None},                        (* Arrays[dims, field] *)
  {{None, None, None}, None}                   (* Arrays[dims, field, sym] *)
},
"ArraySimplify" -> {  {{None}, "_[1]"},                            (* simplifies array expressions *)
  {{None, None}, "_[1]"}                       (* with assumptions *)
},
"ArraySymbol" -> {  {{_String}, None},                          (* ArraySymbol["name"] — symbolic array object *)
  {{_String, _List}, None}                   (* with symmetry spec *)
},
"Arrow" -> {  {{None}, None},                              (* Arrow[{p1,p2}] — graphics primitive *)
  {{None, None}, None}                         (* Arrow[curve, arrowhead-size] *)
},
"Arrowheads" -> {  {{None}, None},                              (* Arrowheads[spec] — graphics directive *)
  {{_List}, None}                             (* Arrowheads[{size, pos, g}] *)
},
"ASATriangle" -> {  {{None, None, None}, None}                   (* ASATriangle[α, s, β] — geometric region object *)
},
"Ask" -> {  {{None}, None},                              (* Ask[template] — dialog input; result varies *)
  {{None, None}, None}
},
"AskAppend" -> {  {{None, None}, None}                         (* AskAppend[cell, content] — side-effecting *)
},
"AskConfirm" -> {  {{None}, None}                               (* AskConfirm[msg] — dialog boolean; typed None, runtime Bool *)
},
"AskDisplay" -> {  {{None, None}, None}                         (* AskDisplay[expr, form] — side-effecting *)
},
"AskedValue" -> {  {{None}, None},                              (* AskedValue[template] — value previously entered *)
  {{None, None}, None}
},
"AskFunction" -> {  {{None}, None}                               (* AskFunction[f] — wraps f as interactive dialog *)
},
"AskState" -> {  {{None}, None}                               (* AskState[task] — association of answers, typed None *)
},
"AskTemplateDisplay" -> {  {{None, None}, None}                         (* AskTemplateDisplay[template, assoc] — side-effecting *)
},
"Assert" -> {  {{None}, None},                              (* Assert[expr] — returns Null or throws; opaque *)
  {{None, None}, None}                         (* Assert[expr, msg] *)
},
"AssessmentFunction" -> {  {{None}, None},                              (* AssessmentFunction[rubric] — function object *)
  {{None, None}, None}
},
"AssessmentResultObject" -> {  {{None}, None}                               (* AssessmentResultObject[assoc] — result object *)
},
"AstroAngularSeparation" -> {  {{None, None}, _Quantity},                 (* AstroAngularSeparation[obj1, obj2] → angle Quantity *)
  {{None, None, None}, _Quantity}            (* with observer location *)
},
"AstroDistance" -> {  {{None, None}, _Quantity},                 (* AstroDistance[obj1, obj2] → distance Quantity *)
  {{None, None, None}, _Quantity}            (* with observer/date *)
},
"AstroGraphics" -> {  {{None}, _Graphics},                       (* AstroGraphics[prims] — sky-chart Graphics *)
  {{None, None}, _Graphics}                  (* with options *)
},
"AstronomicalData" -> {  {{_String}, None},                          (* entity properties list *)
  {{_String, _String}, None},               (* specific property *)
  {{_String, _List}, None}                  (* list of properties *)
},
"AstroPosition" -> {  {{None}, None},                              (* AstroPosition[obj] — coordinate object *)
  {{None, None}, None}                         (* with observer/date *)
},
"AstroRiseSet" -> {  {{None}, None},                              (* AstroRiseSet[obj] — association of DateObjects *)
  {{None, None}, None}                         (* with observer *)
},
"AstroStyling" -> {  {{None}, None}                               (* AstroStyling[spec] — graphics directive *)
},
"AstroSubpoint" -> {  {{None}, None},                              (* AstroSubpoint[obj] — GeoPosition *)
  {{None, None}, None}                         (* with date *)
},
"Asymptotic" -> {  {{None, None}, _?NumericQ},               (* Asymptotic[expr, x→a] *)
  {{None, None, _Integer}, _?NumericQ}     (* with order n *)
},
"AsymptoticDSolveValue" -> {  {{None, None, None}, None},                  (* AsymptoticDSolveValue[eq, y, {x,x0,n}] — series/symbolic *)
  {{None, None, None, None}, None}
},
"AsymptoticEqual" -> {  {{None, None, None}, _?BooleanQ}           (* AsymptoticEqual[f, g, x→a] *)
},
"AsymptoticEquivalent" -> {  {{None, None, None}, _?BooleanQ}           (* AsymptoticEquivalent[f, g, x→a] *)
},
"AsymptoticExpectation" -> {  {{None, None, None}, _?NumericQ},          (* AsymptoticExpectation[expr, x→dist, n] *)
  {{None, None}, _?NumericQ}
},
"AsymptoticGreater" -> {  {{None, None, None}, _?BooleanQ}           (* AsymptoticGreater[f, g, x→a] *)
},
"AsymptoticGreaterEqual" -> {  {{None, None, None}, _?BooleanQ}           (* AsymptoticGreaterEqual[f, g, x→a] *)
},
"AsymptoticIntegrate" -> {  {{None, None, None}, _?NumericQ},          (* AsymptoticIntegrate[f, {x,x0,n}, ...] *)
  {{None, None, None, None}, _?NumericQ}     (* with integration variable and point *)
},
"AsymptoticLess" -> {  {{None, None, None}, _?BooleanQ}           (* AsymptoticLess[f, g, x→a] *)
},
"AsymptoticLessEqual" -> {  {{None, None, _Rule}, _?BooleanQ}       (* f ≲ g, x -> Infinity *)
},
"AsymptoticOutputTracker" -> {  {{None, None}, None}                       (* control-systems object; opaque *)
},
"AsymptoticProbability" -> {  {{None, _Rule}, _?NumericQ}             (* pred, x -> {dist, range} *)
},
"AsymptoticProduct" -> {  {{None, _List, _Rule}, _?NumericQ}     (* f, {i,imin,imax}, x->dir *)
},
"AsymptoticRSolveValue" -> {  {{None, None, _List}, _?NumericQ}       (* eqn, f[n], {n, n0, dir} *)
},
"AsymptoticSolve" -> {  {{None, None, _Rule}, _List}            (* eqn, y[x], x->dir -> {{y->...},...} *)
},
"AsymptoticSum" -> {  {{None, _List, _Rule}, _?NumericQ}     (* f, {i,imin,imax}, x->dir *)
},
"AsynchronousTaskObject" -> {  {{None}, None}                             (* opaque task handle; not constructed directly *)
},
"AsynchronousTasks" -> {  {{}, _List}                              (* list of all running async tasks *)
},
"Atom" -> {  {{_Integer}, None},                       (* Atom[6]   — atomic number *)
  {{_String},  None}                        (* Atom["C"] — element symbol *)
},
"AttachCell" -> {  {{None, None, None}, None}                 (* notebook, cell, attach-spec; Null *)
},
"AttentionLayer" -> {  {{},     None},                            (* zero-arg *)
  {{None}, None}                             (* with size/options *)
},
"AugmentedPolyhedron" -> {  {{None}, None}                             (* base polyhedron; opaque geometric object *)
},
"AuthenticationDialog" -> {  {{None}, None}                             (* service/type; UI dialog; Null *)
},
"Autocomplete" -> {  {{_String, _Integer}, None}              (* text, cursor pos; triggers UI; Null *)
},
"AutocompletionFunction" -> {  {{None}, None}                             (* completion-function object *)
},
"AutocorrelationTest" -> {  {{None},        None},                     (* data -> HypothesisTestData *)
  {{None, None},  None}                      (* data, property *)
},
"AutoRefreshed" -> {  {{None},        None},                     (* expr; dynamic refresh wrapper *)
  {{None, None},  None}                      (* expr, interval *)
},
"AutoSubmitting" -> {  {{None}, None}                             (* form wrapper; Null *)
},
"AxiomaticTheory" -> {  {{_String}, None}                         (* theory name; axiom-set expression *)
},
"AxisObject" -> {  {{None}, None}                             (* chart axis specification; opaque *)
},
"BabyMonsterGroupB" -> {  {{}, None}                                 (* finite group object *)
},
"Backslash" -> {  {{None, None}, None}                       (* operator form / typeset form *)
},
"Band" -> {  {{None},        None},                     (* Band[{i,j}]      — sparse spec *)
  {{None, None},  None}                      (* Band[{i,j}, val] — with fill value *)
},
"BandpassFilter" -> {  {{None, _List},           _List},       (* data, {ω1,ω2} *)
  {{None, _List, _Integer},_List}        (* data, {ω1,ω2}, order n *)
},
"BandstopFilter" -> {  {{None, _List},           _List},       (* data, {ω1,ω2} *)
  {{None, _List, _Integer},_List}        (* data, {ω1,ω2}, order n *)
},
"BarabasiAlbertGraphDistribution" -> {  {{_Integer, _Integer}, None}             (* n, k; graph-distribution object *)
},
"BarcodeImage" -> {  {{None, _String},        _Image},       (* code, format — "EAN13" etc. *)
  {{None, _String, None},  _Image}        (* code, format, size *)
},
"BarcodeRecognize" -> {  {{None},          _String},              (* image -> decoded barcode string *)
  {{None, _String},_String}               (* image, expected format *)
},
"BaringhausHenzeTest" -> {  {{None},       None},                      (* data -> HypothesisTestData *)
  {{None, None}, None}                       (* data, property *)
},
"BarLegend" -> {  {{None},       None},                      (* colorscheme spec; graphic element *)
  {{None, None}, None}                       (* colorscheme, {min, max} *)
},
"BarlowProschanImportance" -> {  {{None, None}, _List}                    (* reliability-dist, time t -> per-component list *)
},
"BarnesG" -> {  {{_Integer}, _?NumericQ},
  {{_Real},    _?NumericQ},
  {{None},      _?NumericQ}                (* complex z — Barnes G-function *)
},
"BartlettHannWindow" -> {  {{_Real}, _?NumericQ},                  (* window value at x in [-1/2, 1/2] *)
  {{None},   _?NumericQ}
},
"BartlettWindow" -> {  {{_Real}, _?NumericQ},
  {{None},   _?NumericQ}
},
"BaseDecode" -> {  {{_String},          None},               (* base64 string -> ByteArray (not in ret vocab) *)
  {{_String, _String},None}                (* string, base name *)
},
"BaseEncode" -> {  {{None},          _String},              (* ByteArray -> base64 string *)
  {{None, _String},_String}               (* bytes, base name e.g. "Base32" *)
},
"BasicRecurrentLayer" -> {  {{_Integer},       None},                 (* n outputs; net layer object *)
  {{_Integer, None}, None}
},
"BatchNormalizationLayer" -> {  {{},     None},
  {{None}, None}                             (* net layer object *)
},
"BatesDistribution" -> {  {{_Integer, _List}, None}                (* n, {a,b}; distribution object *)
},
"BattleLemarieWavelet" -> {  {{},          None},
  {{_Integer}, None}                        (* order n; wavelet object *)
},
"BayesianMaximization" -> {  {{None, None}, None}                       (* f, vars -> BayesianMaximizationObject *)
},
"BayesianMaximizationObject" -> {  {{None}, None}                             (* opaque result object *)
},
"BayesianMinimization" -> {  {{None, None}, None}                       (* f, vars -> BayesianMinimizationObject *)
},
"BayesianMinimizationObject" -> {  {{None}, None}                             (* opaque result object *)
},
"Because" -> {  {{None, None}, None}                       (* logical connective; typeset form *)
},
"BeckmannDistribution" -> {  {{_Real, _Real, _Real, _Real}, None}   (* μ1, μ2, σ1, σ2; distribution object *)
},
"Beep" -> {  {{}, None}                                 (* side-effectful; returns Null *)
},
"Begin" -> {  {{_String}, _String}                    (* "ctx`" -> prior context string *)
},
"BeginDialogPacket" -> {  {{None}, None}                             (* MathLink packet type; opaque *)
},
"BeginPackage" -> {  {{_String}, _String},
  {{_String, _List}, _String}
},
"BellB" -> {  {{_Integer}, _Integer},
  {{_Integer, None}, _?NumericQ}
},
"BellY" -> {  {{_Integer, _Integer, _List}, _?NumericQ}
},
"BenfordDistribution" -> {  {{None}, None}
},
"BeniniDistribution" -> {  {{None, None, None}, None}
},
"BenktanderGibratDistribution" -> {  {{None, None}, None}
},
"BenktanderWeibullDistribution" -> {  {{None, None}, None}
},
"BernoulliB" -> {  {{_Integer}, _?NumericQ},
  {{_Integer, None}, _?NumericQ}
},
"BernoulliGraphDistribution" -> {  {{_Integer, None}, None}
},
"BernoulliProcess" -> {  {{None}, None}
},
"BernsteinBasis" -> {  {{_Integer, _Integer, None}, _?NumericQ}
},
"BesagL" -> {  {{None}, None},
  {{None, _Integer}, None}
},
"BesselFilterModel" -> {  {{_Integer}, None},
  {{None}, None}
},
"BetaBinomialDistribution" -> {  {{_Integer, None, None}, None}
},
"BetaNegativeBinomialDistribution" -> {  {{_Integer, None, None}, None}
},
"BetaPrimeDistribution" -> {  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"BetaRegularized" -> {  {{None, None, None}, _?NumericQ},
  {{None, None, None, None}, _?NumericQ}
},
"Between" -> {  {{_List}, None},
  {{None, _List}, _?BooleanQ}
},
"BetweennessCentrality" -> {  {{None}, _List}
},
"BeveledPolyhedron" -> {  {{None}, None},
  {{None, None}, None}
},
"BezierCurve" -> {  {{_List}, None},
  {{_List, ___}, None}
},
"BezierFunction" -> {  {{_List}, None}
},
"BezierSurface" -> {  {{_List}, None},
  {{_List, ___}, None}
},
"BilateralFilter" -> {  {{_Image, None, None}, _Image}
},
"BilateralLaplaceTransform" -> {  {{None, None, None}, _?NumericQ},
  {{None, _List, None}, _?NumericQ}
},
"BilateralZTransform" -> {  {{None, None, None}, None}
},
"Binarize" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image}
},
"BinaryDeserialize" -> {  {{None}, None},
  {{None, None}, None}
},
"BinaryDistance" -> {  {{None, None}, _Real}
},
"BinarySerialize" -> {  {{None}, None}
},
"BinCounts" -> {  {{_List}, _List},
  {{_List, None}, _List},
  {{_List, None, None}, _List}
},
"BinLists" -> {  {{_List}, _List},
  {{_List, None}, _List},
  {{_List, None, None}, _List}
},
"BinomialDistribution" -> {  {{_Integer, None}, None}
},
"BinomialPointProcess" -> {  {{_Integer, None}, None}
},
"BinomialProcess" -> {  {{None}, None}
},
"BinormalDistribution" -> {  {{None, None, None}, None},
  {{None}, None}
},
"BioMolecule" -> {  {{_String, _List}, None},
  {{_String, _String}, None}
},
"BioMoleculeAlign" -> {  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"BioMoleculeValue" -> {  {{None, None}, None}
},
"BiorthogonalSplineWavelet" -> {  {{}, None},
  {{_Integer, _Integer}, None}
},
"BioSequence" -> {  {{_String, _String}, None},
  {{_String, None}, None}
},
"BioSequenceComplement" -> {  {{None}, None}
},
"BioSequenceInstances" -> {  {{None}, _List},
  {{None, _Integer}, _List}
},
"BioSequenceModify" -> {  {{None, _String}, None},
  {{None, None}, None}
},
"BioSequenceReverseComplement" -> {  {{None}, None}
},
"BioSequenceTranscribe" -> {  {{None}, None}
},
"BioSequenceTranslate" -> {  {{None}, None}
},
"BiquadraticFilterModel" -> {  {{None}, None},
  {{None, None}, None}
},
"BirnbaumImportance" -> {  {{None, None}, _Real},
  {{None, _List}, _List}
},
"BirnbaumSaundersDistribution" -> {  {{None, None}, None}
},
"BitClear" -> {  {{_Integer, _Integer}, _Integer}   (* BitClear[n, k]: clear bit k of n *)
},
"BitFlip" -> {  {{_Integer, _Integer}, _Integer}   (* BitFlip[n, k]: flip bit k of n *)
},
"BitGet" -> {  {{_Integer, _Integer}, _Integer}   (* BitGet[n, k]: 0 or 1 value of bit k *)
},
"BitLength" -> {  {{_Integer}, _Integer}              (* BitLength[n]: number of significant binary digits *)
},
"BitSet" -> {  {{_Integer, _Integer}, _Integer}   (* BitSet[n, k]: set bit k of n *)
},
"BiweightLocation" -> {  {{_List}, _Real},                   (* BiweightLocation[data] *)
  {{_List, _Real}, _Real}            (* BiweightLocation[data, c]: explicit tuning constant *)
},
"BiweightMidvariance" -> {  {{_List}, _Real},                   (* BiweightMidvariance[data] *)
  {{_List, _Real}, _Real}            (* BiweightMidvariance[data, c] *)
},
"BlackmanHarrisWindow" -> {  {{None}, _?NumericQ}                 (* BlackmanHarrisWindow[x]: window value at x ∈ [0,1] *)
},
"BlackmanNuttallWindow" -> {  {{None}, _?NumericQ}                 (* BlackmanNuttallWindow[x] *)
},
"BlackmanWindow" -> {  {{None}, _?NumericQ}                 (* BlackmanWindow[x] *)
},
"Blank" -> {  {{}, None},                            (* Blank[]: matches any single expression *)
  {{None}, None}                         (* Blank[h]: matches any expr with head h *)
},
"BlankNullSequence" -> {  {{}, None},                            (* BlankNullSequence[]: 0 or more args *)
  {{None}, None}                         (* BlankNullSequence[h]: typed 0+ sequence *)
},
"BlankSequence" -> {  {{}, None},                            (* BlankSequence[]: 1 or more args *)
  {{None}, None}                         (* BlankSequence[h]: typed 1+ sequence *)
},
"Blend" -> {  {{_List}, None},                      (* Blend[{c1,c2,...}]: equal-weight blend; head varies *)
  {{_List, None}, None}                 (* Blend[{c1,c2}, t]: blend at parameter t ∈ [0,1] *)
},
"Block" -> {  {{_List, None}, "_[1]"}              (* Block[{vars}, body]: body return passes through *)
},
"BlockchainAddressData" -> {  {{None}, None},                        (* returns Entity-based association; head varies *)
  {{None, None}, None}
},
"BlockchainBlockData" -> {  {{None}, None},
  {{None, None}, None}
},
"BlockchainContractValue" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"BlockchainData" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"BlockchainGet" -> {  {{None}, None},
  {{None, None}, None}
},
"BlockchainKeyEncode" -> {  {{None}, None},                        (* output encoding varies by format arg *)
  {{None, None}, None}
},
"BlockchainPut" -> {  {{None}, None},
  {{None, None}, None}
},
"BlockchainTokenData" -> {  {{None}, None},
  {{None, None}, None}
},
"BlockchainTransaction" -> {  {{None}, None},
  {{None, None}, None}
},
"BlockchainTransactionData" -> {  {{None}, None},
  {{None, None}, None}
},
"BlockchainTransactionSign" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"BlockchainTransactionSubmit" -> {  {{None}, None},
  {{None, None}, None}
},
"BlockMap" -> {  {{None, _List, _Integer}, _List},            (* BlockMap[f, list, n] *)
  {{None, _List, _Integer, _Integer}, _List}  (* BlockMap[f, list, n, d]: with offset d *)
},
"BlockRandom" -> {  {{None}, "_[1]"}                       (* BlockRandom[expr]: seeded random state; result passes through *)
},
"BlomqvistBeta" -> {  {{_List}, _Real},                   (* BlomqvistBeta[data]: bivariate correlation *)
  {{_List, _List}, _Real}            (* BlomqvistBeta[xdata, ydata] *)
},
"BlomqvistBetaTest" -> {  {{_List}, None},                      (* returns HypothesisTestData object *)
  {{_List, None}, None},
  {{_List, _List}, None}
},
"Blur" -> {  {{_Image}, _Image},                 (* Blur[img]: default Gaussian blur *)
  {{_Image, None}, _Image},           (* Blur[img, r]: blur radius r *)
  {{_Image, None, None}, _Image}      (* Blur[img, r, kernel]: explicit kernel type *)
},
"Blurring" -> {  {{None}, None}                         (* visual effect directive used inside Style[]; not a pure function *)
},
"BohmanWindow" -> {  {{None}, _?NumericQ}                 (* BohmanWindow[x]: window function value *)
},
"Bond" -> {  {{None}, None},                        (* Bond[{i,j}]: bond between atom indices; returns Bond object *)
  {{None, None}, None}                   (* Bond[{i,j}, type]: typed bond (Single, Double, ...) *)
},
"Boole" -> {  {{None}, _Integer}                   (* Boole[expr]: True→1, False→0 *)
},
"BooleanConsecutiveFunction" -> {  {{None, _Integer}, None},             (* BooleanConsecutiveFunction[k, n]: returns BooleanFunction *)
  {{None, _List}, _?BooleanQ}         (* BooleanConsecutiveFunction[k, {b1,...}]: evaluated form *)
},
"BooleanCountingFunction" -> {  {{None, _Integer}, None},             (* BooleanCountingFunction[kspec, n]: returns BooleanFunction *)
  {{None, _List}, _?BooleanQ}         (* BooleanCountingFunction[kspec, {b1,...}]: evaluated *)
},
"BooleanGraph" -> {  {{None, _Graph}, _Graph},           (* BooleanGraph[f, g] *)
  {{None, _List}, _Graph}             (* BooleanGraph[f, {g1, g2, ...}] *)
},
"BooleanMaxterms" -> {  {{None, None}, None},                  (* BooleanMaxterms[n, vars]: symbolic maxterm expression *)
  {{_List, None}, None}                 (* BooleanMaxterms[{n1,...}, vars] *)
},
"BooleanMinterms" -> {  {{None, None}, None},                  (* BooleanMinterms[n, vars]: symbolic minterm expression *)
  {{_List, None}, None}                 (* BooleanMinterms[{n1,...}, vars] *)
},
"BorderDimensions" -> {  {{_Image}, _List}                   (* BorderDimensions[img]: {{top,bot},{left,right}} pixel widths *)
},
"BorelTannerDistribution" -> {  {{None, None}, None}                   (* BorelTannerDistribution[a, n]: distribution object *)
},
"BottomHatTransform" -> {  {{_Image, None}, _Image},           (* BottomHatTransform[img, ker]: closing minus original *)
  {{_Image, None, None}, _Image}      (* with padding method *)
},
"BoundaryDiscretizeGraphics" -> {  {{None}, None},                        (* BoundaryDiscretizeGraphics[g]: MeshRegion; head not in vocab *)
  {{None, None}, None},                  (* with region bounds *)
  {{None, None, None}, None}
},
"BoxData" -> {  {{None}, None}                         (* BoxData[box]: FE internal box representation *)
},
"BoxObject" -> {  {{None}, None}                         (* BoxObject[{...}]: typed reference to a frontend box *)
},
"Bra" -> {  {{None}, None}                         (* Bra[state]: Dirac bra ⟨state|; typeset notation *)
},
"BracketingBar" -> {  {{__}, None}                        (* BracketingBar[e1,...]: typeset absolute-value bars *)
},
"BraKet" -> {  {{None, None}, None}                   (* BraKet[bra, ket]: Dirac inner product ⟨bra|ket⟩ *)
},

(* ═══ Wave 4: General functions A-B ═══ *)


(* ═══ Wave 5: General functions B-C ═══ *)
"BrayCurtisDistance" -> {  {{_List, _List}, _Real},
  {{None, None}, _Real}
},
"BreadthFirstScan" -> {  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, _Rule}, None}
},
"Break" -> {  {{}, None}
},
"BridgeData" -> {  {{___}, None}
},
"BrightnessEqualize" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"BroadcastStationData" -> {  {{___}, None}
},
"BrownForsytheTest" -> {  {{_List}, None},
  {{None}, None},
  {{None, None}, None},
  {{None, None, _String}, None}
},
"BrownianBridgeProcess" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"BSplineBasis" -> {  {{_Integer, None}, _?NumericQ},
  {{_Integer, _Integer, None}, _?NumericQ},
  {{None, _Integer, None}, _?NumericQ}
},
"BSplineCurve" -> {  {{_List}, None},
  {{None}, None}
},
"BSplineFunction" -> {  {{_List}, None},
  {{_List, None}, None}
},
"BSplineSurface" -> {  {{_List}, None}
},
"BubbleHistogram" -> {  {{_List}, _Graphics},
  {{_List, _List}, _Graphics},
  {{_List, _List, _List}, _Graphics}
},
"BuckyballGraph" -> {  {{}, _Graph},
  {{_Integer}, _Graph},
  {{_Integer, _String}, _Graph}
},
"BuildCompiledComponent" -> {  {{None}, None},
  {{None, None}, None}
},
"BuildingData" -> {  {{___}, None}
},
"BulletGauge" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"BunchKaufmanDecomposition" -> {  {{_List}, _List},
  {{None}, _List}
},
"ButterflyGraph" -> {  {{_Integer}, _Graph},
  {{_Integer, None}, _Graph}
},
"ButterworthFilterModel" -> {  {{_Integer}, None},
  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"Button" -> {  {{None, None}, None},
  {{None, None, ___}, None}
},
"ButtonBar" -> {  {{_Association}, None},
  {{_Association, ___}, None},
  {{None}, None}
},
"ButtonBox" -> {  {{None}, None}
},
"ButtonNotebook" -> {  {{}, None},
  {{___}, None}
},
"ByteArrayFormat" -> {  {{___}, _String}
},
"ByteArrayToString" -> {  {{None}, _String},
  {{None, _String}, _String}
},
"C" -> {  {{_Integer}, None}
},
"CalendarConvert" -> {  {{None}, None},
  {{None, None}, None}
},
"CalendarData" -> {  {{___}, None}
},
"CalibratedSystemModel" -> {  {{___}, None}
},
"Callout" -> {  {{_List, None}, None},
  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"CallPacket" -> {  {{None, _List}, None}
},
"CanberraDistance" -> {  {{_List, _List}, _Real},
  {{None, None}, _Real}
},
"CancelButton" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"CanonicalGraph" -> {  {{None}, _Graph}
},
"CanonicalizePolygon" -> {  {{None}, None},
  {{None, _String}, None}
},
"CanonicalizePolyhedron" -> {  {{None}, None}
},
"CanonicalName" -> {  {{_List}, _String},
  {{None}, _String}
},
"CanonicalWarpingCorrespondence" -> {  {{None, None}, _List},
  {{None, None, None}, _List},
  {{None, None, None, None}, _List}
},
"CanonicalWarpingDistance" -> {  {{None, None}, _Real},
  {{None, None, None}, _Real},
  {{None, None, None, None}, _Real}
},
"CantorStaircase" -> {  {{_Real}, _Real},
  {{None}, _?NumericQ}
},
"Canvas" -> {  {{}, None},
  {{None}, None}
},
"Cap" -> {  {{None, None}, None}
},
"CapForm" -> {  {{_String}, None}
},
"CapitalDifferentialD" -> {  {{None}, None}
},
"Capitalize" -> {  {{_String}, _String},
  {{_String, None}, _String}
},
"CapsuleShape" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"CaputoD" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"CarlemanLinearize" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"CarlsonRE" -> {  {{None, None}, _?NumericQ},
  {{_List, None}, _List}
},
"CarlsonRK" -> {  {{None, None, None}, _?NumericQ}
},
"CarlsonRM" -> {  {{None, None, None}, _?NumericQ}
},
"Cases" -> {  {{None, None}, _List},
  {{None, None, None}, _List},
  {{None, None, None, None}, _List}
},
"CaseSensitive" -> {  {{_String}, None}
},
"Cashflow" -> {  {{_List}, None},
  {{_List, None}, None}
},
"Casoratian" -> {  {{None, None}, _?NumericQ}
},
"Cast" -> {  {{None, None}, "_[1]"}
},
"CastColumns" -> {  {{None, None}, None}
},
"CatchExceptions" -> {  {{None, None}, "_[1]"}
},
"CategoricalDistribution" -> {},"CategoricalHistogram" -> {  {{_List}, _Graphics},
  {{_List, None}, _Graphics}
},
"CategoricalValue" -> {},"Catenate" -> {  {{_List}, _List}
},
"CatenateLayer" -> {},"CauchyPointProcess" -> {},"CauchyWindow" -> {  {{None}, _?NumericQ},
  {{None, None}, _?NumericQ}
},
"CayleyGraph" -> {  {{None}, _Graph},
  {{None, None}, _Graph}
},
"CDF" -> {  {{None, None}, _?NumericQ}
},
"CDFDeploy" -> {},"CDFInformation" -> {},"CDFWavelet" -> {},"Cell" -> {},"CellGroup" -> {},"CellGroupData" -> {},"CellObject" -> {},"CellPrint" -> {  {{None}, None}
},
"Cells" -> {  {{}, _List},
  {{None}, _List},
  {{None, None}, _List}
},
"CellularAutomaton" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"CensoredDistribution" -> {},"Censoring" -> {  {{None, None}, None}
},
"CenterArray" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"CenterDot" -> {},"CenteredInterval" -> {  {{None, None}, None}
},
"CentralFeature" -> {  {{_List}, "_[1]"},
  {{_List, None}, "_[1]"}
},
"CentralMoment" -> {  {{_List, None}, _?NumericQ}
},
"CentralMomentGeneratingFunction" -> {  {{None, None}, _?NumericQ}
},
"Cepstrogram" -> {  {{None}, _List},
  {{None, None}, _List}
},
"CepstrogramArray" -> {  {{None}, _List},
  {{None, None}, _List}
},
"CepstrumArray" -> {  {{None}, _List},
  {{None, None}, _List}
},
"CForm" -> {  {{None}, _String}
},
"ChannelListen" -> {},"ChannelListener" -> {},"ChannelListeners" -> {},"ChannelObject" -> {},"ChannelReceiverFunction" -> {},"ChannelSend" -> {  {{None, None}, None}
},
"ChannelSubscribers" -> {},"ChanVeseBinarize" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"CharacterCounts" -> {  {{_String}, _Association}
},
"CharacteristicFunction" -> {  {{None, None}, _?NumericQ}
},
"CharacterName" -> {  {{_String}, _String},
  {{_Integer}, _String},
  {{None, _String}, _String}
},
"CharacterNormalize" -> {  {{_String, None}, _String}
},
"CharacterRange" -> {  {{None, None}, _List},
  {{_Real, _Real}, _List}
},
"Characters" -> {  {{_String}, _List},
  {{_List}, _List}
},
"ChatEvaluate" -> {  {{___}, None}
},
"ChatObject" -> {  {{}, None}
},
"ChatSubmit" -> {  {{___}, None}
},
"Chebyshev1FilterModel" -> {  {{_Integer}, None}
},
"Chebyshev2FilterModel" -> {  {{_Integer}, None}
},
"ChebyshevDistance" -> {  {{_List, _List}, _Real}
},
"ChebyshevT" -> {  {{_Integer, None}, _?NumericQ},
  {{_List, None}, _List}
},
"ChebyshevU" -> {  {{_Integer, None}, _?NumericQ},
  {{_List, None}, _List}
},
"Check" -> {  {{None, None}, "_[1]"},
  {{None, None, _String}, "_[1]"}
},
"CheckArguments" -> {  {{___}, _?BooleanQ}
},
"Checkbox" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, ___}, None}
},
"CheckboxBar" -> {  {{None, None}, None},
  {{None, _Association, ___}, None}
},
"ChemicalConvert" -> {  {{None, _String}, None}
},
"ChemicalData" -> {  {{_String}, None},
  {{_String, _String}, None}
},
"ChemicalFormula" -> {  {{None}, _String}
},
"ChemicalInstance" -> {  {{___}, None}
},
"ChemicalReaction" -> {  {{___}, None}
},
"ChessboardDistance" -> {  {{_List, _List}, _Real}
},
"ChiDistribution" -> {  {{_Integer}, None}
},
"ChoiceButtons" -> {  {{_List, None, None}, None}
},
"ChoiceDialog" -> {  {{None}, None},
  {{None, None}, None},
  {{None, _Association, ___}, None}
},
"CholeskyDecomposition" -> {  {{_List}, _List}
},
"Circle" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"CircleDot" -> {  {{___}, None}
},
"CircleMinus" -> {  {{___}, None}
},
"CirclePlus" -> {  {{___}, None}
},
"CirclePoints" -> {  {{_Integer}, _List},
  {{_Integer, _Integer}, _List},
  {{None, _Integer}, _List},
  {{None, None, _Integer}, _List}
},
"CircleThrough" -> {  {{None}, None},
  {{None, None}, None}
},
"CircleTimes" -> {  {{___}, None}
},
"CirculantGraph" -> {  {{_Integer, _Integer}, _Graph},
  {{_Integer, None}, _Graph}
},
"CircularArcThrough" -> {  {{None}, None},
  {{None, None}, None}
},
"CircumscribedBall" -> {  {{_List}, None}
},
"Circumsphere" -> {  {{_List}, None}
},
"CityData" -> {  {{_String}, None},
  {{_String, _String}, None}
},
"Classify" -> {  {{_List, None}, None},
  {{_List, None, None}, None}
},
"Clear" -> {  {{None}, None},
  {{None, __}, None}
},
"ClearAll" -> {  {{None}, None},
  {{None, __}, None}
},
"ClearAttributes" -> {  {{None, None}, None}
},
"ClearCookies" -> {  {{__}, None}
},
"ClearDistributedDefinitions" -> {  {{None}, None},
  {{}, None}
},
"ClearPermissions" -> {  {{None}, None},
  {{None, _String}, None},
  {{None, _List}, None}
},
"ClearSystemCache" -> {  {{}, None},
  {{_String}, None}
},
"ClebschGordan" -> {  {{None, None, None}, _?NumericQ}
},
"ClickPane" -> {  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image}
},
"ClickToCopy" -> {  {{_String, None}, None},
  {{None, None}, None}
},
"CliffordAlgebra" -> {  {{None, None, None}, None}
},
"Compile" -> {  {{_List, __}, None}
},
"CompiledCodeFunction" -> {  {{}, None}
},
"CompiledComponent" -> {  {{}, None}
},
"CompiledExpressionDeclaration" -> {  {{}, None}
},
"CompiledFunction" -> {  {{}, None}
},
"CompiledLayer" -> {  {{}, None}
},
"CompilerCallback" -> {  {{}, None}
},
"CompilerEnvironmentAppendTo" -> {  {{}, None}
},
"CompilerInformation" -> {  {{}, None}
},
"Complement" -> {  {{_List, _List}, _List},
  {{_List, _List, __}, _List}
},
"ComplementedEntityClass" -> {  {{}, None}
},
"CompleteGraph" -> {  {{_Integer}, _Graph}
},
"CompleteIntegral" -> {  {{__, _List}, None}
},
"CompleteKaryTree" -> {  {{_Integer, _Integer}, _Graph}
},
"ComponentExpand" -> {  {{__}, "_[1]"}
},
"ComponentMeasurements" -> {  {{_Image, __}, _List}
},
"ComposeSeries" -> {  {{__, __}, "_[1]"}
},
"CompoundElement" -> {  {{}, None}
},
"CompoundExpression" -> {  {{__}, "_[1]"}
},
"CompoundPoissonDistribution" -> {  {{}, None}
},
"CompoundPoissonProcess" -> {  {{}, None}
},
"CompoundRenewalProcess" -> {  {{}, None}
},
"Condition" -> {  {{}, None}
},
"ConditionalExpression" -> {  {{}, None}
},
"Conditioned" -> {  {{}, None}
},
"ConformAudio" -> {  {{_List, __}, _List}
},
"ConformDates" -> {  {{__, __}, "_[1]"}
},
"ConformImages" -> {  {{_List, __}, _List}
},
"Congruent" -> {  {{}, None}
},
"ConicGradientFilling" -> {  {{}, None}
},
"ConicOptimization" -> {  {{__, __, _List}, _List}
},
"ConjugateTranspose" -> {  {{_List}, _List}
},
"Conjunction" -> {  {{_List}, _?BooleanQ},
  {{__}, _?BooleanQ}
},
"ConnectedComponents" -> {  {{_Graph}, _List}
},
"ConnectedGraphComponents" -> {  {{_Graph}, _List}
},
"ConnectedMoleculeComponents" -> {  {{__}, _List}
},
"ConnectLibraryCallbackFunction" -> {  {{}, None}
},
"ConnectSystemModelComponents" -> {  {{}, None}
},
"ConnectSystemModelController" -> {  {{}, None}
},
"ConnesWindow" -> {  {{__}, _?NumericQ}
},
"ConoverTest" -> {  {{}, None}
},
"ConservativeConvectionPDETerm" -> {  {{}, None}
},
"ConsoleMessage" -> {  {{_String}, None},
  {{__}, None}
},
"ConstantArray" -> {  {{__, _Integer}, _List},
  {{__, _List}, _List}
},
"ConstantArrayLayer" -> {  {{}, None}
},
"ConstantImage" -> {  {{}, None}
},
"ConstantPlusLayer" -> {  {{}, None}
},
"ConstantTimesLayer" -> {  {{}, None}
},
"Clock" -> {  {{}, _Real},                              (* Dynamic clock, cycles 0..1 *)
  {{None}, _Real},                          (* Clock[{tmin, tmax}] scaled range *)
  {{None, _Integer}, _Real}               (* Clock[{tmin, tmax, dt}] with step *)
},
"ClockGauge" -> {  {{__}, None}                             (* Dynamic gauge display; opaque graphic *)
},
"CloseKernels" -> {  {{}, None},                                 (* close all remote/parallel kernels *)
  {{None}, None}                              (* close specific kernel or list *)
},
"Closing" -> {  {{_Image, None}, _Image},               (* morphological closing of image by ker *)
  {{_Image, _Integer}, _Image}           (* by disk of radius n *)
},
"CloudAccountData" -> {  {{}, None},                                 (* all Wolfram Cloud account properties *)
  {{_String}, None}                          (* specific property; return type varies *)
},
"CloudConnect" -> {  {{}, None},                                 (* connect with saved credentials *)
  {{_String}, None},                         (* connect with username *)
  {{_String, _String}, None}               (* connect with username + password *)
},
"CloudDeploy" -> {  {{None}, None},                             (* deploy expr; returns CloudObject *)
  {{None, None}, None}                        (* deploy to given CloudObject *)
},
"CloudDisconnect" -> {  {{}, None}                                  (* disconnect from Wolfram Cloud *)
},
"CloudEvaluate" -> {  {{None}, "_[1]"},                           (* evaluate expr in cloud; passes result through *)
  {{None, None}, "_[1]"}                      (* evaluate with options *)
},
"CloudExport" -> {  {{None, None, _String}, _String},        (* export data as fmt; returns cloud URL *)
  {{None, None}, _String}                  (* format inferred from extension *)
},
"CloudExpression" -> {  {{_String}, None}                          (* reference a named CloudExpression object *)
},
"CloudExpressions" -> {  {{}, _List},                              (* all CloudExpression objects for account *)
  {{_String}, _List}                       (* filtered by name pattern *)
},
"CloudFunction" -> {  {{None, None}, None}                        (* params, body → deployable cloud function *)
},
"CloudGet" -> {  {{_String}, "_[1]"},                       (* fetch expr from cloud URL and evaluate *)
  {{_URL}, "_[1]"}                           (* with explicit URL object *)
},
"CloudImport" -> {  {{None, None}, None},                       (* import from cloud URL as format; varies by fmt *)
  {{_String, None}, None}                    (* with string URL *)
},
"CloudLoggingData" -> {  {{}, None},                                 (* all cloud logging data *)
  {{None, _List, None}, None}               (* specific deployment logs *)
},
"CloudObject" -> {  {{}, None},                                 (* base CloudObject *)
  {{_String}, None},                         (* cloud object by URL/name string *)
  {{_String, None}, None}                    (* with options *)
},
"CloudObjects" -> {  {{}, _List},                              (* all CloudObjects for account *)
  {{_String}, _List},                      (* in given cloud directory *)
  {{None}, _List}                           (* with filter options *)
},
"CloudPublish" -> {  {{None}, _String},                        (* publish CloudObject; returns public URL *)
  {{None, None}, _String}                   (* publish with permissions/options *)
},
"CloudPut" -> {  {{None}, None},                             (* store expr in cloud; returns CloudObject *)
  {{None, _String}, None},                   (* store at named path *)
  {{None, _URL}, None}                       (* store at explicit URL *)
},
"CloudSave" -> {  {{None}, None},                             (* save CloudSymbol/CloudExpression *)
  {{None, _String}, None},                   (* save to named path *)
  {{None, _URL}, None}                       (* save to explicit URL *)
},
"CloudShare" -> {  {{None}, None},                             (* share CloudObject with everyone *)
  {{None, _String}, None},                   (* share with specific user *)
  {{_URL, None}, None}                       (* share by URL *)
},
"CloudSubmit" -> {  {{None}, None},                             (* submit for async eval; returns EvaluationObject *)
  {{None, _String}, None}                    (* submit to named queue *)
},
"CloudSymbol" -> {  {{None}, None},                             (* cloud-persisted symbol reference *)
  {{None, None}, None}                        (* with initialization value *)
},
"CloudUnshare" -> {  {{None}, None},                             (* revoke public access to CloudObject *)
  {{None, _String}, None},                   (* revoke for specific user *)
  {{None, None}, None}                        (* with further options *)
},
"ClusterClassify" -> {  {{_List}, None},                           (* learn cluster classifier from data *)
  {{_List, _Integer}, None}                (* into k clusters; returns ClassifierFunction *)
},
"ClusteringComponents" -> {  {{_List}, _List},                        (* cluster vectors; returns integer labels *)
  {{_List, _Integer}, _List},             (* into k clusters *)
  {{_Image}, _List},                       (* cluster image pixels *)
  {{_Image, _Integer}, _List}            (* image into k clusters *)
},
"ClusteringMeasurements" -> {  {{None, None}, None},                       (* quality measurements for clustering result *)
  {{None, None, None}, None}                 (* specific measurement by name *)
},
"ClusteringTree" -> {  {{None}, None},                             (* hierarchical clustering Tree (not Graph) *)
  {{None, None}, None}                        (* with distance/linkage method options *)
},
"CoifletWavelet" -> {  {{}, None},                                 (* default Coiflet wavelet spec *)
  {{_Integer}, None}                         (* Coiflet wavelet of order n *)
},
"CollinearPoints" -> {  {{None}, _?BooleanQ}                      (* True if all points in list are collinear *)
},
"Colon" -> {  {{None, None}, None},                       (* a:b range / ratio / pattern notation *)
  {{None, None, None}, None}                 (* n-ary form *)
},
"ColonForm" -> {  {{None, None}, None}                        (* display as a:b formatting *)
},
"Column" -> {  {{_List}, None},                           (* arrange items in a vertical column *)
  {{_List, None}, None}                      (* with alignment/spacing options *)
},
"ColumnForm" -> {  {{None}, None},                             (* legacy column display formatting *)
  {{None, None}, None}                        (* with spacing *)
},
"ColumnKeys" -> {  {{None}, _List},                          (* list column keys of a Dataset *)
  {{None, None}, _List}                     (* specific key level *)
},
"ColumnTypes" -> {  {{None}, _Association}                    (* column name → type map for a Dataset *)
},
"ColumnwiseCombine" -> {  {{None}, None},                             (* combine list of datasets column-wise *)
  {{None, None}, None}                        (* with combiner function *)
},
"ColumnwiseThread" -> {  {{None}, None}                              (* thread a function column-wise over datasets *)
},
"ColumnwiseValue" -> {  {{None}, None}                              (* extract column values from Dataset *)
},
"Comap" -> {  {{None, None}, "_[1]"},                     (* apply f at each level of expr *)
  {{None, None, None}, "_[1]"}               (* with explicit level spec *)
},
"ComapApply" -> {  {{None, None}, "_[1]"},                     (* Apply f at each level of expr *)
  {{None, None, None}, "_[1]"}               (* with explicit level spec *)
},
"CombinedEntityClass" -> {  {{None, None, None, _String}, None}        (* combine entity classes with join/filter spec *)
},
"CometData" -> {  {{None}, None},                             (* all properties of comet entity *)
  {{None, _String}, None}                    (* specific property *)
},
"Commonest" -> {  {{None}, _List},                          (* most frequent element(s) in list *)
  {{None, None}, _List}                     (* n most frequent elements *)
},
"CommonestFilter" -> {  {{_List, _Integer}, _List},             (* replace by most common val in radius-r window *)
  {{_List, None}, _List}                   (* with general radius spec *)
},
"CommonName" -> {  {{None}, _String}                         (* common name string for an entity *)
},
"CommonUnits" -> {  {{None, None}, None}                        (* find common unit expression for two quantities *)
},
"Commutator" -> {  {{None, None}, None},                       (* symbolic commutator A.B - B.A *)
  {{None, None, None}, None}                 (* generalized commutator with operator spec *)
},
"CompanyData" -> {  {{None}, None},                             (* all properties of company entity *)
  {{None, _String}, None}                    (* specific property *)
},
  "CTCLossLayer" -> {  {{}, None}
  },
  "CompiledComponentRawInterface" -> {{{None}, None}},  "ConstantVideo" -> {{{None}, _Video}, {{None, None}, _Video}},  "ConstellationData" -> {{{___}, None}},  "Construct" -> {{{None}, "_[1]"}, {{None, None}, "_[1]"}, {{None, None, None}, "_[1]"}, {{None, __}, "_[1]"}},  "ConstructColumns" -> {{{None}, None}, {{None, None}, None}},  "Containing" -> {{{_String, _String}, None}},  "ContainsExactly" -> {{{None}, None}, {{None, None}, _?BooleanQ}},  "ContainsOnly" -> {{{None}, None}, {{None, None}, _?BooleanQ}},  "ContentDetectorFunction" -> {{{None}, None}},  "ContentObject" -> {{{_String}, None}},  "Context" -> {{{}, _String}, {{None}, _String}},  "Contexts" -> {{{}, _List}, {{_String}, _List}},  "Continue" -> {{{}, None}},  "ContinuedFraction" -> {{{None}, _List}, {{None, _Integer}, _List}, {{_List, _Integer}, _List}},  "ContinuedFractionK" -> {{{None, None}, _?NumericQ}, {{None, None, None}, _?NumericQ}},  "ContinuousMarkovProcess" -> {{{_Integer, _Integer}, None}, {{None, _Integer, None}, None}, {{None, None}, None}},  "ContinuousTask" -> {{{None, _Integer}, None}},  "ContinuousWaveletData" -> {{{None}, None}, {{None, None}, None}},  "ContinuousWaveletTransform" -> {{{_List, None}, None}, {{_List, None, None}, None}, {{_Audio, None}, None}},  "ContourDetect" -> {{{_Image}, _Image}, {{_Image, None}, _Image}, {{_List, None}, None}},  "ContourGraphics" -> {{{None}, _Graphics}, {{None, None}, _Graphics}},  "ContourIntegrate" -> {{{None, None}, _?NumericQ}, {{None, None, None}, _?NumericQ}},  "ContraharmonicMean" -> {{{_List}, _?NumericQ}, {{_List, None}, _?NumericQ}},  "ContrastiveLossLayer" -> {{{}, None}},  "Control" -> {{{None}, None}},  "ControlActive" -> {{{}, _?BooleanQ}, {{None}, _?BooleanQ}},  "ControllabilityGramian" -> {{{None}, _List}},  "ControllableDecomposition" -> {{{None}, None}, {{None, None}, None}},  "ControllerInformation" -> {{{}, _Association}, {{_String}, _Association}},  "ControllerManipulate" -> {{{___}, None}},  "ControllerState" -> {{{}, _Association}, {{_String}, _Association}, {{_Integer, _String}, _Association}, {{_Integer, None}, _Association}, {{None, _String}, _Association}},  "ConvectionPDETerm" -> {{{None, None}, None}, {{None, None, None}, None}},  "Convergents" -> {{{None}, _List}, {{None, _Integer}, _List}, {{_List}, _List}},  "ConvexOptimization" -> {{{None, None, None}, _List}, {{None, None}, _List}, {{None, _String}, _List}},  "ConvolutionLayer" -> {{{_Integer, _String}, None}, {{_Integer, None}, None}, {{_Integer, None, None}, None}},  "Convolve" -> {{{None, None, None, None}, _?NumericQ}},  "ConwayGroupCo1" -> {{{}, None}},  "ConwayGroupCo2" -> {{{}, None}},  "ConwayGroupCo3" -> {{{}, None}},  "CoordinateBoundingBox" -> {{{None}, _List}, {{None, None}, _List}},  "CoordinateBoundingBoxArray" -> {{{None}, _List}, {{None, _Integer}, _List}, {{None, None}, _List}, {{None, None, None}, _List}, {{None, None, None, _Integer}, _List}},  "CoordinateBounds" -> {{{None}, _List}, {{None, None}, _List}, {{_String}, _List}},  "CoordinateBoundsArray" -> {{{None}, _List}, {{None, _Integer}, _List}, {{None, None}, _List}, {{None, None, None}, _List}, {{None, None, None, _Integer}, _List}},  "CoordinateChartData" -> {{{None, None}, None}, {{None, None, None}, None}},  "CoplanarPoints" -> {{{None}, _?BooleanQ}},  "Coproduct" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}, {{None, None, None, None}, None}},  "CopulaDistribution" -> {{{None, None}, None}},  "CopyDatabin" -> {{{None}, None}},  "CopyToClipboard" -> {{{None}, None}},  "CoreNilpotentDecomposition" -> {{{None}, _List}, {{None, _String}, _List}},  "CornerFilter" -> {  {{_Image},           _Image},   (* CornerFilter[img]    *)
  {{_Image, _Integer}, _Image}   (* CornerFilter[img, r] *)
  },

  "Correlation" -> {  {{_List},          _List},  (* Correlation[list]       – autocorrelation matrix *)
  {{_List, _List},  _Real}   (* Correlation[xlist,ylist]– Pearson coefficient    *)
  },

  "CorrelationDistance" -> {  {{None, None}, _Real}   (* CorrelationDistance[u,v] *)
  },

  "CorrelationFunction" -> {  {{None, None},        None},   (* CorrelationFunction[proc, t]       *)
  {{None, None, None},  None}    (* CorrelationFunction[proc, t, opts] *)
  },

  "CorrelationTest" -> {  {{None},              None},   (* CorrelationTest[data]          → HypothesisTestData *)
  {{None, None},        None},   (* CorrelationTest[xlist, ylist]                       *)
  {{None, None, _String}, None} (* CorrelationTest[..., "prop"]                        *)
  },

  "CosDegrees" -> {{{None}, _Real}},    (* CosDegrees[θ°]   *)  "CosIntegral" -> {  {{None},    _?NumericQ},   (* CosIntegral[z]       *)
  {{_List},  _List}         (* CosIntegral[{z,…}]   – Listable *)
  },

  "CoshIntegral" -> {  {{None},    _?NumericQ},   (* CoshIntegral[z]      *)
  {{_List},  _List}         (* CoshIntegral[{z,…}]  – Listable *)
  },

  "CosineDistance" -> {  {{None, None}, _Real}   (* CosineDistance[u,v] *)
  },

  "CosineWindow" -> {  {{None},    _?NumericQ},   (* CosineWindow[x]   – continuous weight at x ∈ [-½,½] *)
  {{_List},  _List}         (* CosineWindow[{…}] – Listable                         *)
  },

  "CotDegrees" -> {{{None}, _Real}},    (* CotDegrees[θ°]   *)  "CoulombF" -> {  {{None, None, None},         _?NumericQ},   (* CoulombF[l, η, r]         *)
  {{_List, None, _Integer},  _List}          (* CoulombF[{l,…}, η, r]     – Listable *)
  },

  "CoulombG" -> {  {{None, None, None},         _?NumericQ},
  {{_List, None, _Integer},  _List}
  },

  "CoulombH1" -> {  {{None, None, None},         _?NumericQ},
  {{_List, None, _Integer},  _List}
  },

  "CoulombH2" -> {  {{None, None, None},         _?NumericQ},
  {{_List, None, _Integer},  _List}
  },

  "CountDistinct" -> {  {{None},        _Integer},   (* CountDistinct[list]       *)
  {{None, None},  _Integer}    (* CountDistinct[list, test] *)
  },

  "CountDistinctBy" -> {  {{None, None}, _Integer}   (* CountDistinctBy[list, f] *)
  },

  "CountRoots" -> {  {{None, None},        _Integer},   (* CountRoots[f, x]       *)
  {{None, _List},      _Integer}    (* CountRoots[f, {x,a,b}] *)
  },

  "CountryData" -> {  {{},                   _List},   (* CountryData[]                 – all countries    *)
  {{_String},           None},      (* CountryData[country]          – entity           *)
  {{_String, _String}, None}       (* CountryData[country, "Prop"]  – value varies     *)
  },

  "Counts" -> {  {{None},          _Association},   (* Counts[list]          – tally map              *)
  {{_List},        _Association},   (* Counts[{a,b,…}]                                *)
  {{_Association}, _Association}    (* Counts[assoc]         – counts of values       *)
  },

  "CountsBy" -> {  {{None, None},          _Association},   (* CountsBy[list, f]      *)
  {{_List, None},        _Association},   (* CountsBy[{a,b,…}, f]   *)
  {{_Association, None}, _Association}    (* CountsBy[assoc, f]     *)
  },

  "CovarianceFunction" -> {  {{None, None},        None},   (* CovarianceFunction[proc, t]        *)
  {{None, None, None},  None}    (* CovarianceFunction[proc, t, opts]  *)
  },

  "CoxIngersollRossProcess" -> {  {{None, None, None, None}, None}   (* CoxIngersollRossProcess[μ,σ,θ,x0] *)
  },

  "CoxModel" -> {  {{},                    None},
  {{None},                None},
  {{None, None},          None},
  {{None, None, None},    None}
  },

  "CoxModelFit" -> {  {{None},              None},
  {{None, None},        None},
  {{None, None, None},  None}
  },

  "CoxianDistribution" -> {  {{_List, _List}, None}   (* CoxianDistribution[{p,…},{λ,…}] *)
  },

  "CramerVonMisesTest" -> {  {{None},                None},   (* CramerVonMisesTest[data]            → HypothesisTestData *)
  {{None, None},          None},   (* CramerVonMisesTest[data, dist]                           *)
  {{None, None, _String}, None}   (* CramerVonMisesTest[data, dist, "prop"]                   *)
  },

  "CreateArchive" -> {  {{None},          _String},   (* CreateArchive[src]       – returns path to archive *)
  {{None, _String}, _String}   (* CreateArchive[src, dest]                           *)
  },

  "CreateChannel" -> {  {{},          None},   (* CreateChannel[]      *)
  {{_String},  None}    (* CreateChannel[name]  *)
  },

  "CreateCloudExpression" -> {  {{None},          None},   (* CreateCloudExpression[expr]       *)
  {{None, _String}, None}   (* CreateCloudExpression[expr, name] *)
  },

  "CreateCompilerEnvironment" -> {  {{},     None},   (* CreateCompilerEnvironment[]       *)
  {{___},  None}    (* CreateCompilerEnvironment[opts…]  *)
  },

  "CreateDataStructure" -> {  {{_String},                None},   (* CreateDataStructure["type"]          *)
  {{_String, None, None, None}, None} (* CreateDataStructure["type",…]        *)
  },

  "CreateDataSystemModel" -> {  {{_List, _String}, None}   (* CreateDataSystemModel[data, name] *)
  },

  "CreateDatabin" -> {  {{},          None},   (* CreateDatabin[]      *)
  {{_String},  None}    (* CreateDatabin[name]  *)
  },

  "CreateDialog" -> {  {{___}, None}   (* CreateDialog[content…, opts…] *)
  },

  "CreateDocument" -> {  {{___}, None}   (* CreateDocument[content…, opts…] *)
  },

  "CreateForeignCallback" -> {  {{None, _String}, None}   (* CreateForeignCallback[f, "type"] *)
  },

  "CreateLicenseEntitlement" -> {  {{},    None},   (* CreateLicenseEntitlement[]      *)
  {{___}, None}    (* CreateLicenseEntitlement[opts…] *)
  },

  "CreateManagedLibraryExpression" -> {  {{None, None}, None}   (* CreateManagedLibraryExpression["name", f] *)
  },

  "CreateManagedObject" -> {  {{None, None}, None}   (* CreateManagedObject[constructor, id] *)
  },

  "CreateNotebook" -> {  {{},    None},   (* CreateNotebook[]      *)
  {{___}, None}    (* CreateNotebook[opts…] *)
  },

  "CreatePacletArchive" -> {  {{_String},          None},   (* CreatePacletArchive[dir]       *)
  {{_String, _String}, None}   (* CreatePacletArchive[dir, dest] *)
  },

  "CreatePalette" -> {  {{___}, None}   (* CreatePalette[content…, opts…] *)
  },

  "CreatePermissionsGroup" -> {  {{_String},       None},   (* CreatePermissionsGroup["name"]          *)
  {{_String, None}, None}    (* CreatePermissionsGroup["name", members] *)
  },

  "CreateScheduledTask" -> {  {{None},              None},   (* CreateScheduledTask[f]             *)
  {{None, None},        None},   (* CreateScheduledTask[f, interval]   *)
  {{None, None, None},  None}    (* CreateScheduledTask[f, t, opts]    *)
  },

  "CreateSearchIndex" -> {  {{},               None},   (* CreateSearchIndex[]           *)
  {{_String},       None},   (* CreateSearchIndex[name]       *)
  {{None, _String}, None}    (* CreateSearchIndex[src, name]  *)
  },

  "CreateSemanticSearchIndex" -> {  {{_List, _String}, None}   (* CreateSemanticSearchIndex[data, name] *)
  },

  "CreateSystemModel" -> {  {{},                    None},
  {{None},                None},
  {{None, None},          None},
  {{None, None, None},    None}
  },

  "CreateTemporary" -> {  {{},          _String},   (* CreateTemporary[]         – returns temp file path *)
  {{_String},  _String}    (* CreateTemporary["tmpl"]   – with name template     *)
  },

  "CreateTypeInstance" -> {  {{___}, None}   (* CreateTypeInstance[type, args…] *)
  },

  "CreateUUID" -> {  {{}, _String},
  {{_String}, _String}
  },

  "CreateVectorDatabase" -> {  {{_List}, None},
  {{_List, _String}, None}
  },

  "CreateWindow" -> {  {{}, None},
  {{None}, None}
  },

  "CriticalSection" -> {  {{None, None}, None}
  },

  "CriticalityFailureImportance" -> {  {{None}, _Real},
  {{None, _Real}, _Real}
  },

  "CriticalitySuccessImportance" -> {  {{None}, _Real},
  {{None, _Real}, _Real}
  },

  "CrossEntropyLossLayer" -> {  {{}, None},
  {{_String}, None}
  },

  "CrossingDetect" -> {  {{_Image}, _Image},
  {{_Image, _Real}, _Image}
  },

  "CrossingPolygon" -> {  {{_List}, None}
  },

  "CscDegrees" -> {  {{None}, _Real}
  },

  "Cube" -> {  {{}, None},
  {{_Real}, None}
  },

  "CubeRoot" -> {  {{None}, _Real}
  },

  "Cuboid" -> {  {{_List}, None},
  {{_List, _List}, None}
  },

  "Cumulant" -> {  {{None, _Integer}, _?NumericQ}
  },

  "CumulantGeneratingFunction" -> {  {{None, None}, None}
  },

  "Cup" -> {  {{None, None}, None}
  },

  "CupCap" -> {  {{None, None}, None}
  },

  "Curl" -> {  {{_List, _List}, _List}
  },

  "CurrencyConvert" -> {  {{None, _String}, None}
  },

  "CurrentCompiledFunctionData" -> {  {{}, None}
  },

  "CurrentDate" -> {  {{}, None},
  {{None}, None}
  },

  "CurrentImage" -> {  {{}, _Image},
  {{None}, _Image}
  },

  "CurrentNotebookImage" -> {  {{}, _Image}
  },

  "CurrentScreenImage" -> {  {{}, _Image},
  {{None}, _Image}
  },

  "CurrentValue" -> {  {{None}, None},
  {{None, None}, None}
  },

  "Curry" -> {  {{None}, None},
  {{None, _Integer}, None}
  },

  "CurryApplied" -> {  {{None}, None},
  {{None, _Integer}, None}
  },

  "CurvatureFlowFilter" -> {  {{_Image, _Real}, _Image},
  {{_Image, _Real, _Integer}, _Image}
  },

  "CycleGraph" -> {  {{_Integer}, _Graph},
  {{_List}, _Graph}
  },

  "Cycles" -> {  {{_List}, None}
  },

  "Cyclic" -> {},  "CyclicGroup" -> {  {{_Integer}, None}
  },

  "Cylinder" -> {  {{}, None},
  {{_List}, None},
  {{_List, _Real}, None}
  },

  "CylindricalDecomposition" -> {  {{None, _List}, None}
  },

  "D" -> {  {{None, _Symbol}, None},
  {{None, _List}, None},
  {{None, _Symbol, _Symbol}, None}
  },

  "DGaussianWavelet" -> {  {{}, None},
  {{_Integer}, None}
  },

  "DHarmonicMean" -> {  {{_List}, _?NumericQ},
  {{_List, None}, _?NumericQ}
  },

  "DSolveChangeVariables" -> {  {{None, None, None, None}, None}
  },

  "DagumDistribution" -> {  {{_Real, _Real, _Real}, None}
  },

  "DamData" -> {  {{}, None},
  {{None}, None},
  {{None, _String}, None}
  },

  "DamerauLevenshteinDistance" -> {  {{_String, _String}, _Integer},
  {{_List, _List}, _Integer}
  },

  "DarkModePane" -> {  {{None}, None}
  },

  "Darker" -> {  {{None}, None},
  {{None, _Real}, None}
  },

  "Dashing" -> {  {{None}, None},
  {{_List}, None}
  },

  "DataConnectionObject" -> {  {{None}, None}                   (* opaque database-connection wrapper *)
  },

  "DataDistribution" -> {  {{_String, None}, None},        (* DataDistribution["Empirical", data] *)
  {{_String, None, None}, None}   (* with additional parameter *)
  },

  "DataStructure" -> {  {{None, _String}, None}         (* DataStructure[object, "Method"] -> varies *)
  },

  "DatabaseConnect" -> {  {{_String}, None}
  },

  "DatabaseDisconnect" -> {  {{None}, None}
  },

  "DatabaseReference" -> {  {{_String}, None},
  {{None}, None}
  },

  "Databin" -> {  {{}, None},
  {{_String}, None}
  },

  "DatabinAdd" -> {  {{None, _Association}, None},
  {{None, _List}, None}
  },

  "DatabinRemove" -> {  {{None, _String}, None}
  },

  "DatabinUpload" -> {  {{None, None}, None},            (* DatabinUpload[bin, data] -> Null (I/O) *)
  {{None, None, None}, None}       (* DatabinUpload[bin, data, opts] *)
  },

  "Databins" -> {  {{None}, None},
  {{None, _Integer}, None}
  },

  "DaubechiesWavelet" -> {  {{}, None},
  {{_Integer}, None}
  },

  "DavisDistribution" -> {  {{None, None, None}, None}       (* DavisDistribution[mu, sigma, b] -> dist obj *)
  },

  "DawsonF" -> {  {{_Real}, _?NumericQ},        (* Dawson integral F(x) -> real number *)
  {{_List}, _List}              (* Listable: maps over input list *)
  },

  "DayCount" -> {  {{___}, _Integer}              (* DayCount[d1,d2] or DayCount[d1,d2,spec] *)
  },

  "DayCountConvention" -> {  {{}, None}                       (* option symbol for financial day-count rules *)
  },

  "DayMatchQ" -> {  {{___}, _?BooleanQ}            (* DayMatchQ[date, daySpec] -> True | False *)
  },

  "DayName" -> {  {{}, None},                      (* current day name -> Symbol (e.g. Monday) *)
  {{None}, None}                   (* DayName[date] -> Symbol — NOT a DateObject *)
  },

  "DayPlus" -> {  {{None, _Integer}, None},       (* DayPlus[date, n] -> DateObject *)
  {{None, _Integer, _String}, None}  (* DayPlus[date, n, "BusinessDay"] *)
  },

  "DayRange" -> {  {{None, None}, _List},         (* DayRange[d1, d2] -> list of DateObjects *)
  {{None, None, _String}, _List}    (* filtered by day-type string *)
  },

  "DayRound" -> {  {{None, _String}, None},        (* DayRound[date, "BusinessDay"] -> DateObject *)
  {{None}, None}                   (* default rounding *)
  },

  "DeBruijnGraph" -> {  {{_Integer, _Integer}, _Graph},         (* DeBruijnGraph[k, n] *)
  {{_Integer, _Integer, _String}, _Graph} (* with alphabet string *)
  },

  "DeBruijnSequence" -> {  {{_List, _Integer}, _List},    (* list alphabet, word length -> list *)
  {{_Integer, _Integer}, _List}, (* k-ary size, word length -> list *)
  {{_String, _Integer}, _String} (* string alphabet -> string — NOT _DateObject *)
  },

  "Decrement" -> {  {{None}, _?NumericQ}           (* x-- : returns pre-decrement value; HoldFirst *)
  },

  "DecryptionObject" -> {  {{None}, None}                   (* wraps an asymmetric private decryption key *)
  },

  "DefaultAxesStyle" -> {  {{}, None}                       (* notebook/plot option symbol *)
  },

  "DefaultButton" -> {  {{None}, None},                  (* DefaultButton[label] *)
  {{None, None}, None}             (* DefaultButton[label, action] *)
  },

  "DefaultColor" -> {  {{}, None}                       (* notebook style option symbol *)
  },

  "DefaultControlPlacement" -> {  {{}, None}                       (* Manipulate/notebook option symbol *)
  },

  "DefaultDuplicateCellStyle" -> {  {{}, None}                       (* notebook option symbol *)
  },

  "DefaultInlineFormatType" -> {  {{}, None}                       (* notebook option symbol *)
  },

  "DefaultInputFormatType" -> {  {{}, None}                       (* notebook option symbol *)
  },

  "DefaultLabelStyle" -> {  {{}, None}                       (* notebook style option symbol *)
  },

  "DefaultMenuStyle" -> {  {{}, None}                       (* notebook style option symbol *)
  },

  "DefaultNaturalLanguage" -> {  {{}, None}                       (* notebook option symbol *)
  },

  "DefaultNewCellStyle" -> {  {{}, None}                       (* notebook option symbol *)
  },

  "DefaultNewInlineCellStyle" -> {  {{}, None}                       (* notebook option symbol *)
  },

  "DefaultNotebookContents" -> {  {{}, None}                       (* notebook option symbol *)
  },

  "DefaultOptions" -> {  {{None}, None},                  (* DefaultOptions[f] -> opaque in plain kernel *)
  {{None, None}, None}             (* DefaultOptions[f, sym] *)
  },

  "DefaultOutputFormatType" -> {  {{}, None}                       (* notebook option symbol *)
  },

  "DefaultStyle" -> {  {{None}, None}                   (* DefaultStyle[nb, styleName] *)
  },

  "DefaultStyleDefinitions" -> {  {{}, None}                       (* notebook option symbol *)
  },

  "DefaultTicksStyle" -> {  {{}, None}                       (* notebook style option symbol *)
  },

  "DefaultTooltipStyle" -> {  {{}, None}                       (* notebook style option symbol *)
  },

  "Defer" -> {  {{None}, None}                   (* Defer[expr] holds expr unevaluated on output *)
  },

  "DefinitionNotebookClient" -> {  {{}, None}                       (* internal FE symbol; no standard call form *)
  },

  "DegreeGraphDistribution" -> {  {{None}, None}                   (* DegreeGraphDistribution[dlist] -> dist obj *)
  },

  "DegreeLexicographic" -> {  {{}, None}                       (* monomial ordering option value symbol *)
  },

  "DegreeReverseLexicographic" -> {  {{}, None}                       (* monomial ordering option value symbol *)
  },

  "DelayedRule" -> {  {{None, None}, None}             (* lhs :> rhs variant; undefined in std kernel *)
  },

  "DelayedRuleDelayed" -> {  {{None, None}, None}             (* internal delayed-of-delayed rule form *)
  },

  "Delete" -> {  {{None, _Integer}, None},       (* Delete[expr, pos] -> expr with pos removed *)
  {{None, _List}, None},          (* Delete[expr, {{p1},{p2},...}] *)
  {{_List, _Integer}, _List},  (* list specialisation -> _List *)
  {{_Association, None}, _Association} (* association specialisation *)
  },

  "DeleteAnomalies" -> {  {{None}, _List},               (* DeleteAnomalies[data] -> cleaned list *)
  {{None, _List}, _List}        (* with method/options list *)
  },

  "DeleteBorderComponents" -> {  {{_Image}, _Image},           (* remove components touching image border *)
  {{_Image, _Integer}, _Image} (* with connectivity n *)
  },

  "DeleteCase" -> {  {{___}, None}                    (* non-standard / FE-internal symbol *)
  },

  "DeleteContent" -> {  {{None}, None}                   (* remove content from object; FE-internal *)
  },

  "DeleteSmallComponents" -> {  {{_Image}, _Image},           (* delete small connected components *)
  {{_Image, _Integer}, _Image} (* with minimum-size threshold *)
  },

  "DeleteStopwords" -> {  {{_String}, _String},         (* remove stopwords from string -> string *)
  {{_List}, _List}              (* remove stopwords from word list -> list *)
  },

  "DelimitedSequence" -> {  {{None, None}, None}             (* DelimitedSequence[patt, delim] -> pattern obj *)
  },

  "Dendrogram" -> {  {{_List}, _Graphics},
  {{_List, None}, _Graphics}
  },

  "DensityHistogram" -> {  {{None}, _Graphics},
  {{None, None}, _Graphics},
  {{None, None, None}, _Graphics}
  },

  "Deploy" -> {  {{None}, None},
  {{None, None}, None}
  },

  "Depth" -> {  {{None}, _Integer}
  },

  "DepthFirstScan" -> {  {{None, None}, None},
  {{None, _String, None}, None}
  },

  "Derivative" -> {  {{__Integer}, None}
  },

  "DerivativeFilter" -> {  {{_List, None}, _List},
  {{_List, None, None}, _List}
  },

  "DerivativePDETerm" -> {  {{None, None}, None},
  {{None, None, None}, None}
  },

  "DerivedKey" -> {  {{_String}, None},
  {{_String, None}, None}
  },

  "DeviceClose" -> {  {{None}, None}
  },

  "DeviceConfigure" -> {  {{None, None}, None}
  },

  "DeviceExecute" -> {  {{None, _String}, None},
  {{None, _String, None}, None}
  },

  "DeviceExecuteAsynchronous" -> {  {{None, _String, None}, None},
  {{None, _String, None, None}, None}
  },

  "DeviceFunction" -> {  {{None, _String}, None}
  },

  "DeviceObject" -> {  {{_String}, None}
  },

  "DeviceOpen" -> {  {{_String}, None},
  {{_String, None}, None}
  },

  "DeviceOpenQ" -> {  {{None}, _?BooleanQ}
  },

  "DeviceRead" -> {  {{None}, None},
  {{None, None}, None}
  },

  "DeviceReadBuffer" -> {  {{None, _Integer}, None}
  },

  "DeviceReadLatest" -> {  {{None}, _List},
  {{None, _Integer}, _List},
  {{None, _Integer, None}, _List}
  },

  "DeviceReadTimeSeries" -> {  {{None, None}, None},
  {{None, None, None}, None}
  },

  "DeviceRemove" -> {  {{None}, None}
  },

  "DeviceStreams" -> {  {{None}, _List},
  {{None, None}, _List}
  },

  "DeviceWrite" -> {  {{None, None}, None},
  {{None, None, None}, None}
  },

  "DeviceWriteBuffer" -> {  {{None, None}, None}
  },

  "Devices" -> {  {{}, _List},
  {{None}, _List}
  },

  "DiagonalBandMatrix" -> {  {{_List}, None},
  {{_List, None}, None}
  },

  "DialingCode" -> {  {{None}, _String}
  },

  "DialogInput" -> {  {{___}, None}
  },

  "DialogNotebook" -> {  {{_List}, None},
  {{___}, None}
  },

  "DialogPacket" -> {  {{___}, None}
  },

  "DialogProlog" -> {  {{}, None}
  },

  "DialogReturn" -> {  {{}, None},
  {{___}, None}
  },

  "DialogSymbols" -> {  {{}, None}
  },

  "DiceDissimilarity" -> {  {{_List, _List}, _Real}
  },

  "DifferenceRootReduce" -> {  {{None, None}, None}
  },

  "DifferentialD" -> {  {{None}, None}
  },

  "DifferentialRoot" -> {  {{None}, None},
  {{None, None}, None}
  },

  "DifferentialRootReduce" -> {  {{None, None}, None}
  },

  "DifferentiatorFilter" -> {  {{_List, None}, _List},
  {{_List, None, _Integer}, _List},
  {{_List, None, _Integer, None}, _List}
  },

  "DigitBlockSeparator" -> {},  "DigitQ" -> {  {{_String}, _?BooleanQ}
  },

  "DihedralGroup" -> {  {{_Integer}, None}
  },

  "Dilation" -> {  {{_Image, None}, _Image},
  {{None, None}, None}
  },

  "DimensionReduce" -> {  {{_List}, _List},
  {{_List, None}, _List},
  {{_List, None, None}, _List}
  },

  "DimensionReducerFunction" -> {},  "DimensionalCombinations" -> {  {{_List, _Integer}, _List},
  {{_List, _Integer, _List}, _List}
  },

  "DirectedGraph" -> {  {{None}, _Graph},
  {{None, None}, _Graph}
  },

  "DirectedGraphQ" -> {  {{None}, _?BooleanQ}
  },

  "DirectedRelations" -> {  {{None}, None}
  },

  "DiscreteChirpZTransform" -> {  {{_List}, _List},
  {{_List, _Integer}, _List},
  {{_List, _Integer, None}, _List},
  {{_List, _Integer, None, None}, _List}
  },

  "DiscreteConvolve" -> {  {{None, None, None, None}, _?NumericQ}
  },

  "DiscreteDelta" -> {  {{_Integer}, _Integer},
  {{__Integer}, _Integer}
  },

  "DiscreteHadamardTransform" -> {  {{_List}, _List},
  {{_List, None}, _List}
  },

  "DiscreteIndicator" -> {},  "DiscreteLQEstimatorGains" -> {  {{None, _List}, _List},
  {{None, _List, None}, _List}
  },

  "DiscreteLQRegulatorGains" -> {  {{None, _List}, _List},
  {{None, _List, None}, _List}
  },

  "DiscreteLyapunovSolve" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
  },

  "DiscreteMarkovProcess" -> {},  "DiscretePlot" -> {  {{None, _List}, _Graphics},
  {{_List, _List}, _Graphics}
  },

  "DiscretePlot3D" -> {  {{None, _List, _List}, _Graphics3D},
  {{_List, _List, _List}, _Graphics3D}
  },

  "DiscreteRiccatiSolve" -> {  {{None, _List}, _List},
  {{None, None, None, None}, _List}
  },

  "DiscreteTimeModelQ" -> {  {{None}, _?BooleanQ}
  },

  "DiscreteUniformDistribution" -> {},  "DiscreteVariables" -> {},  "DiscreteWaveletData" -> {},  "DiscreteWaveletPacketTransform" -> {},  "DiscreteWaveletTransform" -> {},  "DiscriminantAnalysis" -> {},  "DispersionEstimatorFunction" -> {},  "DisplayFunction" -> {},  "DisplayString" -> {  {{None}, _String},
  {{None, None}, _String}
  },

  "DistanceFunction" -> {},  "DistanceTransform" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image}
  },

  "DistributeDefinitions" -> {},  "Distributed" -> {  {{None, None}, None}
  },

  "DivideByZero" -> {},  "DrazinInverse" -> {  {{None}, _List},
  {{None, None}, _List}
  },

  "DropoutLayer" -> {},  "DualLinearProgramming" -> {  {{None, None, None}, _List},
  {{None, None, None, None}, _List}
  },

  "DualPolyhedron" -> {  {{None}, None}
  },

  "DualSystemsModel" -> {  {{None}, None}
  },

  "DummyVariable" -> {},  "DynamicGeoGraphics" -> {},  "DynamicImage" -> {},  "DynamicLocation" -> {},  "DynamicModule" -> {},  "DynamicModuleBox" -> {},  "DynamicModuleParent" -> {},  "DynamicModuleValues" -> {},  "DynamicSetting" -> {},  "DynamicUpdating" -> {},  "DynamicWrapper" -> {},  "EarthImpactData" -> {{{___}, None}},  "EarthquakeData" -> {{{___}, None}},  "EccentricityCentrality" -> {{{None}, _List}, {{None, None}, _List}},  "EditDistance" -> {{{None, None}, _Integer}, {{None, None, None}, _Integer}},  "EffectiveInterest" -> {{{None, None}, _Real}, {{None, None, None}, _Real}},  "Eigensystem" -> {{{None}, _List}, {{None, _Integer}, _List}},  "EigenvalueDecomposition" -> {{{None}, _List}},  "ElectricCurrentDensityValue" -> {{{None, None}, None}, {{None, None, None}, None}},  "ElectricCurrentPDEComponent" -> {{{None, None}, None}, {{None, None, None}, None}},  "ElectricFluxDensityValue" -> {{{None, None}, None}, {{None, None, None}, None}},  "ElectricPotentialCondition" -> {{{None, None}, None}, {{None, None, None}, None}},  "ElectricSymmetryValue" -> {{{None, None}, None}, {{None, None, None}, None}},  "ElectrostaticPDEComponent" -> {{{None, None}, None}, {{None, None, None}, None}},  "Element" -> {{{None, None}, _?BooleanQ}},  "ElementData" -> {{{_String, _String}, None}, {{_Integer, _String}, None}, {{_String}, None}, {{_Integer}, None}, {{}, None}},  "ElementwiseLayer" -> {{{_String}, None}, {{_String, None}, None}},  "Eliminate" -> {{{None, None}, None}},  "Ellipsoid" -> {{{None, None}, None}},  "EllipticExp" -> {{{None, None}, _?NumericQ}, {{_List, None}, _List}},  "EllipticExpPrime" -> {{{None, None}, _?NumericQ}, {{_List, None}, _List}},  "EllipticFilterModel" -> {{{_String, _Integer}, None}, {{_Integer}, None}},  "EllipticReducedHalfPeriods" -> {{{None}, _List}},  "EllipticThetaPrime" -> {{{None, None, _Integer}, _?NumericQ}, {{None, _Integer}, _?NumericQ}, {{_List, None, _Integer}, _List}},  "EmbedCode" -> {{{None}, None}, {{None, _String}, None}, {{None, _String, _String}, None}, {{None, _String, None}, None}},  "EmbeddedHTML" -> {{{_String}, None}},  "EmbeddedSQLEntityClass" -> {{{_String, None}, None}},  "EmbeddedSQLExpression" -> {{{_String}, None}},  "EmbeddedService" -> {{{None}, None}},  "EmbeddingLayer" -> {{{_Integer, _Integer}, None}, {{_Integer}, None}},  "EmitSound" -> {{{None}, None}},  "EmpiricalDistribution" -> {{{None}, None}},  "EmptySpaceF" -> {{{None, _Integer}, None}, {{None, None}, None}},  "Encode" -> {{{_String}, _String}, {{_String, _String}, None}, {{_String, _String, _String}, None}},  "Encrypt" -> {{{None, None}, None}},  "EncryptedObject" -> {{{_Association}, None}},  "End" -> {{{}, _String}},  "EndAdd" -> {{{}, None}},  "EndDialogPacket" -> {{{None}, None}, {{}, None}},  "EndPackage" -> {{{}, None}},  "EnterExpressionPacket" -> {{{None}, None}},  "EnterTextPacket" -> {{{_String}, None}},  "Entity" -> {{{_String, _String}, None}, {{None, _String}, None}},  "EntityAugmentColumns" -> {{{None, None}, None}},  "EntityClass" -> {{{_String, _String}, None}, {{_String, None}, None}},  "EntityCopies" -> {{{None, _Integer}, _List}},  "EntityGroup" -> {{{None}, None}},  "EntityInstance" -> {{{None, None}, None}},  "EntityPrefetch" -> {{{None}, None}, {{None, None}, None}},  "EntityProperties" -> {{{None}, _List}},  "EntityValue" -> {{{___}, None}},  "Environment" -> {  {{_String}, _String}
  },

  "EqualTilde" -> {  {{None, None}, None}
  },

  "EqualTo" -> {  {{None}, None}
  },

  "Equilibrium" -> {  {{None, None}, None}
  },

  "EquirippleFilterKernel" -> {  {{_List, _Integer}, _List},
  {{_List, _Integer, _Symbol}, _List}
  },

  "Equivalent" -> {  {{__, None}, _?BooleanQ}
  },

  "EquivalentStrain" -> {  {{None}, None}
  },

  "Erf" -> {  {{None}, _?NumericQ},
  {{None, None}, _?NumericQ}
  },

  "Erfc" -> {  {{None}, _?NumericQ}
  },

  "Erfi" -> {  {{None}, _?NumericQ}
  },

  "ErlangB" -> {  {{_Real, _Integer}, _Real}
  },

  "ErlangC" -> {  {{_Real, _Integer}, _Real}
  },

  "ErlangDistribution" -> {  {{_Integer, _Real}, None}
  },

  "Erosion" -> {  {{_Image, None}, _Image},
  {{_Image, _Real}, _Image}
  },

  "ErrorBox" -> {  {{None}, None}
  },

  "EstimatedBackground" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image}
  },

  "EstimatedDistribution" -> {  {{_List}, None},
  {{_List, None}, None}
  },

  "EstimatedPointNormals" -> {  {{_List}, _List}
  },

  "EstimatedPointProcess" -> {  {{_List}, None},
  {{_List, None}, None}
  },

  "EstimatedProcess" -> {  {{_List}, None},
  {{_List, None}, None}
  },

  "EstimatedVariogramModel" -> {  {{_List}, None},
  {{_List, None}, None}
  },

  "EstimatorGains" -> {  {{None, _List}, _List}
  },

  "EstimatorRegulator" -> {  {{None, None}, _List}
  },

  "EuclideanDistance" -> {  {{_List, _List}, _Real}
  },

  "EulerAngles" -> {  {{_List}, _List},
  {{_List, _List}, _List}
  },

  "EulerCharacteristic" -> {  {{None}, _Integer}
  },

  "EulerE" -> {  {{_Integer}, _Integer},
  {{_Integer, None}, _?NumericQ}
  },

  "Evaluate" -> {  {{None}, "_[1]"}
  },

  "EvaluatePacket" -> {  {{None}, None}
  },

  "EvaluateScheduledTask" -> {  {{None}, None}
  },

  "EvaluationBox" -> {  {{}, None}
  },

  "EvaluationCell" -> {  {{}, None}
  },

  "EvaluationData" -> {  {{None}, _Association}
  },

  "EvaluationNotebook" -> {  {{}, None}
  },

  "EvaluationObject" -> {  {{_Integer}, None}
  },

  "EventData" -> {  {{None}, None}
  },

  "EventHandler" -> {  {{None, None}, None}
  },

  "EventSeries" -> {  {{_List}, None}
  },

  "EventSeriesAccumulate" -> {  {{None}, None}
  },

  "EventSeriesLookup" -> {  {{None, None}, None}
  },

  "ExactBlackmanWindow" -> {  {{_Real}, _?NumericQ},
  {{_List}, _List}
  },

  "ExampleData" -> {  {{_List}, None},
  {{_List, _String}, None}
  },

  "Except" -> {  {{None}, None},
  {{None, None}, None}
  },

  "Exception" -> {  {{None}, None}
  },

  "ExceptionTypes" -> {  {{}, _List}
  },

  "Exists" -> {  {{None, None}, None},
  {{None, None, None}, None}
  },

  "Exit" -> {  {{}, None},
  {{_Integer}, None}
  },

  "ExoplanetData" -> {  {{}, _List},
  {{_String}, None},
  {{_String, _String}, None}
  },

  "Expand" -> {  {{None}, None},
  {{None, None}, None}
  },

  "Expectation" -> {  {{None, None}, _?NumericQ}
  },

  "DEigensystem" -> {{{None, None}, _List}, {{None, None, None}, _List}},  "DEigenvalues" -> {{{None, None}, _List}, {{None, None, None}, _List}},  "DFixedPoints" -> {{{None, None}, _List}, {{None, None, _Integer}, _List}},  "DMSString" -> {{{None}, _String}},  "DStabilityConditions" -> {{{None}, _List}, {{None, None}, _List}},  "DatabinSubmit" -> {{{None, None}, None}, {{None, None, None}, None}},  "DayHemisphere" -> {{{}, None}, {{None}, None}, {{None, None}, None}},  "DayNightTerminator" -> {{{}, None}, {{None}, None}, {{None, None}, None}},  "Decapitalize" -> {{{_String}, _String}},  "DecisionTreeModel" -> {{{None}, None}, {{None, None}, None}},  "DeclareCompiledComponent" -> {{{None, _List}, None}},  "DeclarePackage" -> {{{_String, _List}, None}},  "Decompose" -> {{{None, None}, _List}},  "DeconvolutionLayer" -> {{{None}, None}, {{None, None}, None}},  "DedekindEta" -> {{{None}, _?NumericQ}},  "DeepSpaceProbeData" -> {{{}, None}, {{None}, None}, {{None, None}, None}},  "Default" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}},  "DefaultValues" -> {{{None}, _List}},  "DefineInputStreamMethod" -> {{{_String, _Association}, None}},  "DefineOutputStreamMethod" -> {{{_String, _Association}, None}},  "DefineResourceFunction" -> {{{None}, None}},  "Definition" -> {{{None}, None}},  "DegreeCentrality" -> {{{_Graph}, _List}, {{_Graph, None}, _List}},  "Del" -> {{{None, None}, None}},  "Delayed" -> {{{None}, None}},  "DeleteAdjacentDuplicates" -> {{{_List}, _List}, {{_List, None}, _List}},  "DeleteChannel" -> {{{None}, None}},  "DeleteCloudExpression" -> {{{None}, None}},  "DeleteColumns" -> {{{None, None}, _List}},  "DeleteElements" -> {{{None, None}, _List}, {{_List, _List}, _List}},  "DeleteObject" -> {{{None}, None}},  "DeletePermissionsKey" -> {{{None}, None}},  "DeleteSearchIndex" -> {{{None}, None}},  "Dialog" -> {{{}, None}, {{None}, None}},  "Diamond" -> {{{None, None}, None}},  "DictionaryLookup" -> {{{_String}, _List}, {{None}, _List}, {{None, _String}, _List}},  "Diff" -> {{{_String, _String}, None}, {{_List, _List}, None}},  "Diff3" -> {{{_String, _String, _String}, None}},  "DiffApply" -> {{{None, None}, None}},  "DiffObject" -> {{{None, None}, None}, {{None, None, None}, None}},  "DifferenceDelta" -> {{{None, None}, None}, {{None, None, None}, None}},  "DifferenceQuotient" -> {{{None, None}, None}},  "DifferenceRoot" -> {{{None}, None}},  "DiffusionPDETerm" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}},  "DiggleGatesPointProcess" -> {{{None, None, None, None}, None}},  "DiggleGrattonPointProcess" -> {{{None, None, None, None, None}, None}},  "DihedralAngle" -> {{{None, None}, _Real}, {{_List, _List}, _Real}},  "DimensionReduction" -> {{{None}, _List}, {{None, None}, None}},  "Dimensions" -> {{{None}, _List}, {{None, _Integer}, _List}},  "DiracComb" -> {{{None}, None}, {{None, None}, None}},  "DiracDelta" -> {{{None}, _?NumericQ}, {{_Real}, _?NumericQ}},  "DirectionalLight" -> {{{None, None}, None}, {{None, None, None}, None}},  "Directive" -> {{{__}, None}},  "DirichletCharacter" -> {{{None, None, None}, _?NumericQ}, {{None, None}, None}},  "DirichletCondition" -> {{{None, None}, None}},  "DirichletConvolve" -> {{{None, None, None, None}, _?NumericQ}},  "DirichletDistribution" -> {{{None}, None}},  "DirichletTransform" -> {{{None, None, None}, None}},  "DirichletWindow" -> {{{None}, _?NumericQ}},  "DisableFormatting" -> {{{None}, None}},  "Discard" -> {{{None}, None}},  "DiscreteAsymptotic" -> {{{None, None}, None}},  "DiscreteHilbertTransform" -> {{{_List}, _List}, {{_List, None}, _List}},  "DiscreteInputOutputModel" -> {{{None}, None}, {{None, None}, None}},  "DiscreteLimit" -> {{{None, None}, None}},  "DiscreteMaxLimit" -> {{{None, None}, None}},  "DiscreteMinLimit" -> {{{None, None}, None}},  "DiscreteRatio" -> {{{None, None}, None}},  "DiscreteShift" -> {{{__}, None}},  "DiscretizeGraphics" -> {{{None}, None}, {{None, None}, None}},  "Disjunction" -> {{{__}, _?BooleanQ}},  "Disk" -> {{{}, None}, {{None}, None}, {{None, None}, None}},  "DiskSegment" -> {{{None, None, None}, None}},  "Display" -> {{{None}, None}, {{None, None}, None}},  "DisplayEndPacket" -> {{{}, None}},  "DisplayForm" -> {{{None}, None}},  "DisplayPacket" -> {{{}, None}},  "Distribute" -> {{{None}, None}, {{None, None}, None}, {{None, __}, None}},  "DistributionFitTest" -> {{{_List}, None}, {{_List, None}, None}, {{_List, None, _String}, None}},  "DistributionParameterAssumptions" -> {{{None}, None}},  "Div" -> {{{None, None}, _?NumericQ}, {{None, None, None}, _?NumericQ}},  "Divide" -> {{{None, None}, _?NumericQ}},  "DivideBy" -> {{{None, None}, _?NumericQ}},  "DivideSides" -> {{{None, None}, None}},  "Divisible" -> {{{_Integer, _Integer}, _?BooleanQ}, {{_List, _Integer}, _List}},  "Do" -> {{{None, None}, None}, {{None, __}, None}},  "DocumentGenerator" -> {{{None, None}, None}},  "DocumentGeneratorInformation" -> {{{None}, _Association}},  "DocumentGenerators" -> {{{}, _List}},  "DocumentNotebook" -> {{{None}, None}, {{None, __}, None}},  "Dodecahedron" -> {{{}, None}, {{None}, None}},  "DominatorTreeGraph" -> {{{None, _Integer}, _Graph}},  "DotEqual" -> {{{}, None}},  "DotLayer" -> {{{}, None}},  "DotPlusLayer" -> {{{}, None}},  "DoubleBracketingBar" -> {{{}, None}},  "DoubleDownArrow" -> {{{None, __}, None}},  "DoubleLeftArrow" -> {{{None, __}, None}},  "DoubleLeftRightArrow" -> {{{None, __}, None}},  "DoubleLeftTee" -> {{{None, __}, None}},  "DoubleLongLeftArrow" -> {{{None, __}, None}},  "DoubleLongLeftRightArrow" -> {{{None, __}, None}},  "DoubleLongRightArrow" -> {{{None, __}, None}},  "DoubleRightArrow" -> {{{None, __}, None}},  "DoubleRightTee" -> {{{None, __}, None}},  "DoubleUpArrow" -> {{{None, __}, None}},  "DoubleUpDownArrow" -> {{{None, __}, None}},  "DoubleVerticalBar" -> {{{None, __}, None}},  "DownArrow" -> {{{None, __}, None}},  "DownArrowBar" -> {{{None, __}, None}},  "DownArrowUpArrow" -> {{{None, __}, None}},  "DownLeftRightVector" -> {{{None, __}, None}},  "DownLeftTeeVector" -> {{{None, __}, None}},  "DownLeftVector" -> {{{None, __}, None}},  "DownLeftVectorBar" -> {{{None, __}, None}},  "DownRightTeeVector" -> {{{None, __}, None}},  "DownRightVector" -> {{{None, __}, None}},  "DownRightVectorBar" -> {{{None, __}, None}},  "DownTee" -> {{{None, __}, None}},  "DownTeeArrow" -> {{{None, __}, None}},  "DownValues" -> {{{None}, _List}},  "DownValuesFunction" -> {{{None}, None}},  "Downsample" -> {{{_List, _Integer}, _List}, {{_List, _Integer, _Integer}, _List}, {{_Image, None}, _Image}},  "DropShadowing" -> {{{None}, _Image}, {{None, None}, _Image}, {{None, None, None}, _Image}},  "Dt" -> {{{None}, None}, {{None, None}, None}, {{None, __}, None}},  "DualPlanarGraph" -> {{{None}, _Graph}},  "DumpGet" -> {{{_String}, None}},  "DumpSave" -> {{{_String, None}, None}, {{_String, _String}, None}},  "Duration" -> {{{None}, None}},  "Dynamic" -> {{{None}, None}, {{None, None}, None}},  "DynamicBox" -> {{{None}, None}},  "EntityProperty" -> {{{_String, None}, None}, {{_String, None, None}, None}},  "EntityPropertyClass" -> {{{_String, None}, None}},  "EntityRegister" -> {{{___}, None}},  "EntityStore" -> {{{_Association}, None}, {{_String}, None}},  "EntityStores" -> {{{}, _List}},  "EntityType" -> {{{_String}, None}},  "EntityTypeName" -> {{{None}, _String}},  "EntityUnregister" -> {{{___}, None}},  "ExpGammaDistribution" -> {{{None, None}, None}},  "ExpToTrig" -> {{{None}, None}},  "ExpectedValue" -> {{{None, None}, _?NumericQ}, {{None, None, None}, _?NumericQ}},  "ExponentialDistribution" -> {{{None}, None}},  "ExponentialGeneratingFunction" -> {{{None, _Integer, None}, None}},  "ExponentialModel" -> {{{None}, None}},  "ExponentialMovingAverage" -> {{{_List, None}, _List}},  "ExponentialPowerDistribution" -> {{{None, None, None}, None}},  "ExpressionCell" -> {{{None, _String}, None}},  "ExpressionGraph" -> {{{None}, _Graph}, {{None, _Integer}, _Graph}},  "ExpressionTree" -> {{{None}, None}},  "ExtendedEntityClass" -> {{{None, _String}, None}},  "ExtendedKey" -> {{{_String, _String, None}, None}},  "ExternalBundle" -> {{{None}, None}},  "ExternalEvaluate" -> {{{_String, _String}, None}, {{None, _String}, None}, {{_Association, _String}, None}},  "ExternalEvaluatorObject" -> {{{___}, None}},  "ExternalEvaluators" -> {{{}, _List}, {{_String}, _List}},  "ExternalFunction" -> {{{None, _String}, None}},  "ExternalIdentifier" -> {{{_String, _Integer}, None}, {{_String, _String}, None}},  "ExternalObject" -> {{{___}, None}},  "ExternalOperation" -> {{{_String, _String}, None}, {{_String, _String, _Association}, None}},  "ExternalSessionObject" -> {{{___}, None}},  "ExternalSessions" -> {{{}, _List}, {{None}, _List}},  "ExternalStorageDownload" -> {{{None}, None}, {{None, None}, None}},  "ExternalStorageGet" -> {{{None}, None}},  "ExternalStorageObject" -> {{{_Association}, None}, {{None, _Association}, None}},  "ExternalStoragePut" -> {{{None, _String}, None}},  "ExternalStorageUpload" -> {{{None}, None}, {{None, None}, None}},  "ExternalValue" -> {{{None, _String}, None}},  "ExtractArchive" -> {{{None, _String}, _List}, {{None, _String, None}, _List}},  "ExtractLayer" -> {{{}, None}},  "ExtractPacletArchive" -> {{{_String}, _String}, {{_String, _String}, _String}},  "ExtremeValueDistribution" -> {{{None, None}, None}},  "FARIMAProcess" -> {{{None, _Integer, None, None}, None}},  "FaceAlign" -> {{{_Image}, _List}, {{_Image, None}, _List}, {{None, None}, _List}},  "FaceForm" -> {{{}, None}, {{None}, None}, {{None, None}, None}},  "FaceRecognize" -> {{{None, _Image}, None}, {{None, _Image, None}, None}},  "FacialFeatures" -> {{{_Image}, _Association}, {{_Image, None}, _Association}},  "FactorInteger" -> {{{_Integer}, _List}, {{_Integer, _Integer}, _List}},  "FactorSquareFree" -> {{{None}, None}},  "FactorialMoment" -> {{{_List, _Integer}, _?NumericQ}, {{None, _Integer}, _?NumericQ}},  "FactorialMomentGeneratingFunction" -> {{{None, None}, None}},  "FactorialPower" -> {{{None, _Integer}, _?NumericQ}, {{None, _Integer, None}, _?NumericQ}, {{_List, _Integer}, _List}},  "Failure" -> {{{None, None}, None}},  "FailureDistribution" -> {{{None, None}, None}},  "FareySequence" -> {{{_Integer}, _List}, {{_Integer, _Integer}, _List}},  "FeatureDistance" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}},  "FeatureExtract" -> {{{None}, _List}, {{None, None}, _List}},  "FeatureExtraction" -> {{{None}, None}, {{None, None}, None}},  "FeatureExtractorFunction" -> {{{___}, None}},  "FeatureNearest" -> {{{_List, None}, _List}, {{_List, None, _Integer}, _List}, {{_List}, _List}},  "FeedbackLinearize" -> {{{None, None, _String}, None}},  "FetalGrowthData" -> {{{None, _Integer}, None}, {{None, None, _Integer}, None}},  "Fibonorial" -> {{{_Integer}, _Integer}},  "FilledCurve" -> {{{None}, None}},  "FilledPolarCurve" -> {{{None, None}, None}},  "FilledTorus" -> {{{_List, None}, None}, {{_List, None, None}, None}},  "FillingTransform" -> {{{_Image}, _Image}, {{_Image, None}, _Image}},  "FilterRules" -> {{{None, None}, _List}},  "FilteredEntityClass" -> {{{None, None}, None}},  "FRatioDistribution" -> {{{_Integer, _Integer}, None}},  "FinancialBond" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}, {{None, None, None, None}, None}},  "FinancialData" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}},  "FinancialDerivative" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},  "FinancialIndicator" -> {{{None, None}, _List}, {{None, None, _Integer}, _List}},  "Find" -> {{{_String, _String}, _Integer}, {{None, _String}, _String}},  "FindAnomalies" -> {{{None}, _List}, {{None, None}, _List}},  "FindArgMax" -> {{{None, None}, _List}, {{None, _List}, _List}, {{_List, _List}, _List}},  "FindArgMin" -> {{{None, None}, _List}, {{None, _List}, _List}, {{_List, _List}, _List}},  "FindAstroEvent" -> {{{None}, _List}, {{None, None}, _List}},  "FindChannels" -> {{{}, _List}, {{None}, _List}},  "FindClique" -> {{{_Graph}, _List}, {{_Graph, _Integer}, _List}, {{_Graph, _List}, _List}},  "FindClusters" -> {{{None}, _List}, {{None, _Integer}, _List}, {{None, None}, _List}},  "FindCookies" -> {{{}, _List}, {{_String}, _List}, {{_String, _String}, _List}},  "FindDevices" -> {{{}, _List}, {{_String}, _List}},  "FindDistribution" -> {{{None}, None}, {{None, None}, None}},  "FindDistributionParameters" -> {{{None, None}, _List}},  "FindDivisions" -> {{{_List, _Integer}, _List}, {{_List, _List}, _List}},  "FindEdgeCover" -> {{{_Graph}, _List}},  "FindEdgeCut" -> {{{_Graph, None, None}, _List}},  "FindEdgeIndependentPaths" -> {{{_Graph, None, None}, _List}, {{_Graph, None, None, _Integer}, _List}},  "FindEquationalProof" -> {{{None, _List}, None}, {{None, _List, None, _List}, None}},  "FindEulerianCycle" -> {{{_Graph}, _List}, {{_Graph, _Integer}, _List}},  "FindExternalEvaluators" -> {{{}, _List}, {{_String}, _List}},  "FindFaces" -> {{{_Image}, _List}, {{_Image, _Integer}, _List}},  "FindFormula" -> {{{None, None}, None}, {{None, _List, None}, None}},  "FindFundamentalCycles" -> {{{_Graph}, _List}},  "FindGeneratingFunction" -> {{{_List, None}, None}, {{_List, None, _Integer}, None}},  "FindGeoLocation" -> {{{}, _Association}, {{_String}, _Association}},  "FindGeometricConjectures" -> {{{None}, _List}, {{None, None}, _List}},  "FindGeometricTransform" -> {{{_List, _List}, _List}, {{_List, _List, None}, _List}},  "FindGraphCommunities" -> {{{_Graph}, _List}, {{_Graph, None}, _List}},  "FindGraphIsomorphism" -> {{{_Graph, _Graph}, _Association}},  "FindGraphPartition" -> {{{_Graph, _Integer}, _List}, {{_Graph, _Integer, None}, _List}},  "FindHamiltonianCycle" -> {{{_Graph}, _List}, {{_Graph, _Integer}, _List}},  "FindHamiltonianPath" -> {{{_Graph}, _List}, {{_Graph, None, None}, _List}},  "FindHiddenMarkovStates" -> {{{_List, None}, _List}},  "FindImageShapes" -> {{{_Image}, _List}, {{_Image, None}, _List}},  "FindImageText" -> {{{_Image}, _List}, {{_Image, None}, _List}},  "FindIndependentEdgeSet" -> {{{_Graph}, _List}, {{_Graph, None}, _List}},  "FindIndependentVertexSet" -> {{{_Graph}, _List}, {{_Graph, None}, _List}},  "FindIntegerNullVector" -> {{{_List}, _List}},  "FindIsomers" -> {{{_String}, _List}, {{_String, _Integer}, _List}},  "FindIsomorphicSubgraph" -> {{{_Graph, _Graph}, _List}},  "FindKClan" -> {{{_Graph, _Integer}, _List}},  "FindKClique" -> {{{_Graph, _Integer}, _List}},  "FindKClub" -> {{{_Graph, _Integer}, _List}},  "FindKPlex" -> {{{_Graph, _Integer}, _List}},  "FindLibrary" -> {{{_String}, _String}},  "FindLinearRecurrence" -> {{{_List}, _List}, {{_List, _Integer}, _List}},  "FindMaxValue" -> {{{None, None}, _Real}},  "FindMaximumCut" -> {{{_Graph}, _List}, {{_Graph, None, None}, _List}},  "FindMaximumFlow" -> {{{_Graph, None, None}, _List}, {{_Graph, None, None, None}, _List}},  "FindMinValue" -> {{{None, None}, _Real}},  "FindMinimumCostFlow" -> {{{None, None, None}, _List}},  "FindMinimumCut" -> {{{None, None, None}, _List}},  "FindMoleculeSubstructure" -> {{{None, None}, _List}},  "FindPeaks" -> {{{None}, _List}, {{None, None}, _List}},  "FindPointProcessParameters" -> {{{None, None}, _List}},  "FindPostmanTour" -> {{{None}, _List}},  "FindProcessParameters" -> {{{None, None}, _List}},  "FindRepeat" -> {{{None}, _List}},  "FindSequenceFunction" -> {{{None}, None}, {{None, None}, None}},  "FindShortestCurve" -> {{{None}, None}},  "FindShortestPath" -> {{{None, None, None}, _List}, {{None, None, None, None}, _List}},  "FindShortestTour" -> {{{None}, _List}},  "FindSolarEclipse" -> {{{}, _Association}, {{None}, _Association}},  "FindSubgraphIsomorphism" -> {{{None, None}, _List}},  "FindSystemModelEquilibrium" -> {{{None}, _Association}},  "FindTextualAnswer" -> {{{None, None}, _String}},  "FindThreshold" -> {{{None}, _Real}},  "FindTransientRepeat" -> {{{None}, _List}},  "FindTreeGameStrategies" -> {{{None, None}, _List}},  "FindVertexCover" -> {{{None}, _List}},  "FindVertexCut" -> {{{None, None, None}, _List}},  "FindVertexIndependentPaths" -> {{{None, None, None}, _List}},  "FinishDynamic" -> {{{}, None}},  "FiniteField" -> {{{None, None}, None}},  "FiniteFieldElement" -> {{{None, None}, None}},  "FiniteFieldElementNorm" -> {{{None}, _Integer}},  "FiniteFieldElementTrace" -> {{{None}, _Integer}},  "FiniteFieldEmbedding" -> {{{None, None}, None}},  "FiniteFieldIndex" -> {{{None, None}, _Integer}},  "FiniteGroupData" -> {{{None, None}, None}},  "First" -> {{{None}, "_[1]"}, {{None, None}, "_[1]"}},  "FirstCase" -> {{{None, None}, "_[1]"}},  "FirstPassageTimeDistribution" -> {{{None, None}, None}},  "FirstPosition" -> {{{None, None}, _List}},  "FischerGroupFi22" -> {{{}, None}},  "FischerGroupFi23" -> {{{}, None}},  "FischerGroupFi24Prime" -> {{{}, None}},  "FisherHypergeometricDistribution" -> {{{None, None, None, None}, None}},  "FisherRatioTest" -> {{{None, None}, None}},  "FisherZDistribution" -> {{{None, None}, None}},  "Fit" -> {{{None, None, None}, None}},  "FittedModel" -> {{{None}, None}},  "FixedOrder" -> {{{None}, None}},  "FlatShading" -> {{{}, None}},  "FlatTopWindow" -> {{{None}, _?NumericQ}},  "FlattenLayer" -> {{{}, None}, {{None}, None}},  "FlightData" -> {{{None}, None}, {{None, None}, None}},  "Fourier" -> {{{None}, _List}, {{None, None}, _List}},  "FourierCoefficient" -> {{{___, _Symbol, _Integer}, _?NumericQ}},  "FourierCosCoefficient" -> {{{___, _Symbol, _Integer}, _?NumericQ}},  "FourierCosSeries" -> {{{___, _List}, None}},  "FourierCosTransform" -> {{{___, _Symbol, _Symbol}, None}},  "FourierDCT" -> {{{_List}, _List}, {{_List, _Integer}, _List}},  "FourierDCTFilter" -> {{{_List}, _List}, {{_List, ___}, _List}},  "FourierDST" -> {{{_List}, _List}, {{_List, _Integer}, _List}},  "FourierSequenceTransform" -> {{{___, _Symbol}, None}},  "FourierSeries" -> {{{___, _List}, None}},  "FourierSinCoefficient" -> {{{___, _Symbol, _Integer}, _?NumericQ}},  "FourierSinSeries" -> {{{___, _List}, None}},  "FourierSinTransform" -> {{{___, _Symbol, _Symbol}, None}},  "FourierTransform" -> {{{___, _Symbol, _Symbol}, None}},  "FourierTrigSeries" -> {{{___, _List}, None}},  "FoxH" -> {{{_List, ___}, _?NumericQ}},  "FoxHReduce" -> {{{___}, None}},  "FractionBox" -> {{{___, ___}, None}},  "FractionalBrownianMotionProcess" -> {{{_Real, _Real}, None}, {{_Real}, None}},  "FractionalD" -> {{{___, _List}, None}},  "FractionalGaussianNoiseProcess" -> {{{_Real, _Real}, None}, {{_Real}, None}},  "FrameBox" -> {{{___}, None}},  "FrameListVideo" -> {{{___, ___}, None}, {{_List}, None}},  "Framed" -> {{{___}, None}},  "FrechetDistribution" -> {{{_Real, _Real}, None}},  "FreeformEvaluate" -> {{{_String}, None}},  "FrequencySamplingFilterKernel" -> {{{_List, _Integer}, _List}},  "FresnelC" -> {{{None}, _?NumericQ}},  "FresnelF" -> {{{None}, _?NumericQ}},  "FresnelG" -> {{{None}, _?NumericQ}},  "FresnelS" -> {{{None}, _?NumericQ}},  "FrobeniusAutomorphism" -> {{{_Integer}, None}},  "FrobeniusDecomposition" -> {{{_List}, _List}},  "FrobeniusReduce" -> {{{___}, None}},  "FrobeniusSolve" -> {{{_List, _Integer}, _List}},  "FromCharacterCode" -> {{{_Integer}, _String}, {{_List}, _String}},  "FromCoefficientRules" -> {{{_List, ___}, None}},  "FromDMS" -> {{{_List}, _Real}},  "FromJSONString" -> {{{_String}, None}},  "FromPolarCoordinates" -> {{{_List}, _List}},  "FromSphericalCoordinates" -> {{{_List}, _List}},  "FrontEnd" -> {{{}, None}},  "FrontEndExecute" -> {{{___}, None}},  "FrontEndObject" -> {{{}, None}},  "FrontEndResource" -> {{{_String, _String}, None}},  "FrontEndToken" -> {{{_String}, None}, {{_String, _String}, None}},  "FunctionCompile" -> {{{___}, None}},  "FlipView" -> {{{None}, None}, {{None, _Integer}, None}},  "FluidFlowPDEComponent" -> {{{None, None}, None}},  "FluidViscosity" -> {{{}, None}},  "FluidViscousStress" -> {{{}, None}},  "FoldPair" -> {{{None, None, _List}, _List}, {{None, None, _List, None}, _List}},  "FoldWhile" -> {{{None, None, None, None}, None}},  "For" -> {{{None, None, None, None}, None}},  "ForAll" -> {{{None, None}, None}, {{None, None, None}, None}},  "ForAllType" -> {{{None, _String}, None}, {{None, _?BooleanQ, _String}, None}},  "ForeignCallback" -> {{{None, None}, None}},  "ForeignFunction" -> {{{None, None, None}, None}},  "ForeignFunctionLoad" -> {{{None, None}, None}, {{None, None, None}, None}},  "ForeignPointerLookup" -> {{{None, None}, None}},  "FormBox" -> {{{None, None}, None}},  "FormControl" -> {{{None}, None}, {{_List, None}, None}},  "FormObject" -> {{{None}, None}, {{_List, None}, None}},  "FormPage" -> {{{None}, None}, {{None, None}, None}},  "Format" -> {{{None}, None}, {{None, None}, None}},  "FormatValues" -> {{{None}, _List}},  "FormulaData" -> {{{_String}, None}, {{_String, None}, None}},  "FormulaLookup" -> {{{_String}, _List}, {{_String, _Integer}, _List}},  "FormulaModel" -> {{{None}, None}},  "FortranForm" -> {{{None}, _String}},  "FromAbsoluteTime" -> {{{None}, None}, {{None, None}, None}},  "FromDate" -> {{{None}, _Real}},  "FromDateString" -> {{{_String}, None}, {{None}, None}},  "FromEntity" -> {{{None}, None}},  "FromFiniteField" -> {{{None}, _Integer}, {{None, None}, _Integer}},  "FromFiniteFieldIndex" -> {{{None, None}, None}},  "FromJulianDate" -> {{{__}, None}},  "FromRawPointer" -> {{{___}, None}},  "FromTabular" -> {{{None}, None}, {{None, _String}, None}},  "FromUnixTime" -> {{{None}, None}},  "FrontEndTokenExecute" -> {{{__}, None}},  "FullAxes" -> {{{None}, _List}},  "FullDefinition" -> {{{None}, None}, {{_String}, None}},  "FullForm" -> {{{None}, None}},  "FullGraphics" -> {{{None}, _Graphics}},  "FullInformationOutputRegulator" -> {{{None}, _List}, {{None, None}, _List}},  "FullMoon" -> {{{}, None}, {{None}, None}},  "FullSimplify" -> {{{None}, None}, {{None, None}, None}},  "Function" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}},  "FunctionAnalytic" -> {{{None, None}, _?BooleanQ}, {{None, None, None}, _?BooleanQ}},  "FunctionBijective" -> {{{None, None}, _?BooleanQ}, {{None, None, None}, _?BooleanQ}},  "FunctionCompileExport" -> {{{None, _String}, _String}, {{None, _String, None}, _String}},  "FunctionCompileExportByteArray" -> {{{None, _String}, None}},  "FunctionCompileExportLibrary" -> {{{None, _String}, _String}, {{None, _String, None}, _String}},  "FunctionCompileExportString" -> {{{None}, _String}, {{None, None}, _String}},  "FunctionContinuous" -> {{{None, None}, _?BooleanQ}, {{None, None, None}, _?BooleanQ}},  "FunctionConvexity" -> {{{None, None}, None}, {{None, None, None}, None}},  "FunctionDeclaration" -> {{{_Symbol, _List, None}, None}},  "FunctionDiscontinuities" -> {{{None, _Symbol}, _List}, {{None, _List}, _List}},  "FunctionDomain" -> {{{None, _Symbol}, None}},  "FunctionExpand" -> {{{None}, None}},  "FunctionInjective" -> {{{None, _Symbol}, _?BooleanQ}, {{None, _List}, _?BooleanQ}},  "FunctionLayer" -> {{{None}, None}, {{None, None}, None}},  "FunctionMeromorphic" -> {{{None, _Symbol}, _?BooleanQ}},  "FunctionMonotonicity" -> {{{None, _Symbol}, None}, {{None, _List}, None}},  "FunctionPeriod" -> {{{None, _Symbol}, None}},  "FunctionPoles" -> {{{None, _Symbol}, _List}},  "FunctionRange" -> {{{None, _List, _Symbol}, None}},  "FunctionSign" -> {{{None, _Symbol}, None}, {{None, _List}, None}},  "FunctionSingularities" -> {{{None, _Symbol}, _List}},  "FunctionSurjective" -> {{{None, _List, _List}, _?BooleanQ}},  "FussellVeselyImportance" -> {{{None}, _Real}, {{None, None}, _Real}},  "GARCHProcess" -> {{{_List, _List}, None}},  "GPUArray" -> {{{_List}, None}},  "GaborFilter" -> {{{_Image, None, _Integer}, _Image}, {{_Image, None, _Integer, _Real}, _Image}},  "GaborWavelet" -> {{{}, None}, {{_Real}, None}, {{_Real, _Integer}, None}},  "GainMargins" -> {{{None}, _List}},  "GainPhaseMargins" -> {{{None}, _List}},  "GalaxyData" -> {{{}, _List}, {{None}, None}, {{None, _String}, None}},  "GalleryView" -> {{{_List}, None}},  "GameTheoryData" -> {{{None}, None}, {{None, _String}, None}},  "Gamma" -> {{{None}, _?NumericQ}, {{None, None}, _?NumericQ}, {{None, None, None}, _?NumericQ}},  "GatedRecurrentLayer" -> {{{_Integer}, None}},  "Gather" -> {{{_List}, _List}, {{_List, None}, _List}},  "GatherBy" -> {{{_List, None}, _List}},  "GaussianFilter" -> {{{_Image, None}, _Image}, {{_List, None}, _List}},  "GaussianWindow" -> {{{None}, _?NumericQ}, {{None, _Real}, _?NumericQ}},  "GegenbauerC" -> {{{_Integer, None}, _?NumericQ}, {{_Integer, _Integer, None}, _?NumericQ}},  "GeneralizedLinearModelFit" -> {{{_List, _List, _Symbol}, None}, {{_List, _List, _List}, None}},  "GeneralizedPolyLog" -> {{{None, None}, _?NumericQ}},  "GeneralizedPower" -> {{{None, None}, _?NumericQ}},  "GenerateAsymmetricKeyPair" -> {{{}, _Association}, {{_String}, _Association}},  "GenerateDerivedKey" -> {{{_String}, None}, {{_String, _String}, None}},  "GenerateDocument" -> {{{None, _Association}, None}, {{None, _List}, None}},  "GenerateHTTPResponse" -> {{{None}, None}, {{None, None}, None}},  "GenerateLLMToolResponse" -> {{{None}, None}, {{None, None}, None}},  "GenerateSecuredAuthenticationKey" -> {{{}, None}, {{_String}, None}},  "GenerateSymmetricKey" -> {{{}, None}, {{_String}, None}},  "GenericCylindricalDecomposition" -> {{{None, _List}, None}},  "GenomeData" -> {{{_String}, None}, {{_String, _String}, None}},  "GenomeLookup" -> {{{_String}, _List}},  "GeoAntipode" -> {{{None}, None}},  "GeoArea" -> {{{None}, None}},  "GeoBoundingBox" -> {{{None}, None}},  "GeoBounds" -> {{{None}, _List}},  "GeoCircle" -> {{{None, None}, None}, {{None, _List}, None}},  "GeoDestination" -> {{{None, None}, None}},  "GeoDirection" -> {{{None, None}, _Real}},  "GeoDisk" -> {{{None, _Integer}, None}, {{None, _Integer, None}, None}},  "GeoDisplacement" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}},  "GeoElevationData" -> {{{}, None}, {{None, None, _String}, None}},  "GeoEntities" -> {{{None, None}, _List}, {{None}, _List}},  "GeoGraphics" -> {{{_List}, _Graphics}, {{None}, _Graphics}, {{}, _Graphics}},  "GeoGridDirectionDifference" -> {{{None, None, None}, _Real}},  "GeoGridPosition" -> {{{_List, None, None}, None}, {{None, __}, None}},  "GeoGridUnitArea" -> {{{None, None}, None}},  "GeoGridUnitDistance" -> {{{None, None, None}, None}},  "GeoGridVector" -> {{{_List, None}, None}},  "GeoGroup" -> {{{None}, None}},  "GeoHemisphere" -> {{{_String}, None}, {{None}, None}},  "GeoHemisphereBoundary" -> {{{}, None}, {{None}, None}},  "GeoHistogram" -> {{{None}, _Graphics}, {{None, None}, _Graphics}, {{None, None, None}, _Graphics}},  "GeoIdentify" -> {{{None, None}, _List}, {{None}, _List}, {{}, _List}},  "GeoImage" -> {{{None}, _Image}, {{None, None}, _Image}},  "GeoLength" -> {{{None}, None}},  "GeoMarker" -> {{{}, None}, {{None}, None}},  "GeoNearest" -> {{{None, None}, _List}, {{None, None, _Integer}, _List}},  "GeoOrientationData" -> {{{None, None}, None}, {{None, None, _String}, None}},  "GeoPolygon" -> {{{None}, None}, {{None, None}, None}},  "GeoPosition" -> {{{_List}, "_[1]"}, {{_List, None}, "_[1]"}},  "GeoPositionENU" -> {{{_List, None}, None}},  "GeoPositionXYZ" -> {{{_List, None}, None}},  "GeoProjectionData" -> {{{_String, _String}, None}, {{_String}, None}},  "GeoReposition" -> {{{None, _List}, None}},  "GeoSmoothHistogram" -> {{{None}, _Graphics}, {{None, None}, _Graphics}},  "GeoStyling" -> {{{}, None}, {{None}, None}},  "GeoVariant" -> {{{None, None}, None}},  "GeoVector" -> {{{_List, None}, None}, {{_List}, None}},  "GeoVectorENU" -> {{{_List, None}, None}},  "GeoVectorXYZ" -> {{{_List, None}, None}},  "GeodesicClosing" -> {{{_Image, None}, _Image}},  "GeodesicDilation" -> {{{_Image, None}, _Image}, {{_Image, _Integer}, _Image}},  "GeodesicErosion" -> {{{_Image, None}, _Image}, {{_Image, _Integer}, _Image}},  "GeodesicOpening" -> {{{_Image, None}, _Image}, {{_Image, _Integer}, _Image}},  "GeodesicPolyhedron" -> {{{_Integer}, None}, {{_List}, None}, {{_String, _Integer}, None}},  "GeodesyData" -> {{{_String, _String}, None}, {{None, _String}, None}},  "GeogravityModelData" -> {{{}, None}, {{None, None}, None}},  "GeologicalPeriodData" -> {{{___}, None}},  "GeomagneticModelData" -> {{{}, None}, {{None}, None}, {{None, None}, None}},  "GeometricAssertion" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}},  "GeometricBrownianMotionProcess" -> {{{None, None, None}, None}},  "GeometricMeanFilter" -> {{{_Image, _Integer}, _Image}, {{_Image, None}, _Image}, {{_List, _Integer}, _List}, {{_List, None}, _List}},  "GeometricOptimization" -> {{{None, None}, _List}, {{None, _String}, _List}},  "GeometricScene" -> {{{None}, None}, {{None, None}, None}},  "GeometricSolveValues" -> {{{None, None}, _List}},  "GeometricStep" -> {{{_List, _Integer}, None}, {{None, _String}, None}},  "GeometricTest" -> {{{None, None}, _?BooleanQ}, {{None, None, None, None}, _?BooleanQ}},  "GeometricTransformation" -> {{{None, None}, None}},  "GestureHandler" -> {{{None, _Association, ___}, None}},  "GetContext" -> {{{_String}, _String}, {{}, _String}},  "GetEnvironment" -> {{{_String}, _List}, {{}, _Association}},  "GibbsPointProcess" -> {{{None, _Integer}, None}},  "GlobalClusteringCoefficient" -> {{{None}, _Real}},  "Glow" -> {{{_Integer}, None}, {{}, None}},  "GompertzMakehamDistribution" -> {{{None, None}, None}, {{None, None, None}, None}, {{None, None, None, None}, None}},  "GoochShading" -> {{{}, None}},  "GoodmanKruskalGamma" -> {{{_List}, _Real}, {{_List, _List}, _Real}},  "GoodmanKruskalGammaTest" -> {{{None, None}, None}, {{None, _String}, None}},  "Goto" -> {{{_String}, None}},  "GouraudShading" -> {{{}, None}},  "Grad" -> {{{None, None}, _List}, {{None, None, None}, _List}},  "GradientFilter" -> {{{_Image, None}, _Image}, {{_Image, _Integer}, _Image}},  "GradientOrientationFilter" -> {{{_Image, None}, _Image}, {{_Image, _Integer}, _Image}},  "GrammarApply" -> {{{None, _String}, _List}},  "GrammarRules" -> {{{None}, _List}, {{None, None}, _List}},  "GrammarToken" -> {{{_String}, None}},  "GrassmannAlgebra" -> {{{None}, None}, {{None, None}, None}},  "GrayLevel" -> {{{_Real}, None}, {{_Real, _Real}, None}},  "GreaterEqualLess" -> {{{None, None}, None}},  "GreaterEqualThan" -> {{{None}, None}},  "GreaterFullEqual" -> {{{None, None}, None}},  "GreaterGreater" -> {{{None, None}, None}},  "GreaterLess" -> {{{None, None}, None}},  "GreaterSlantEqual" -> {{{None, None}, None}},  "GreaterThan" -> {{{None}, None}},  "GreaterTilde" -> {{{None, None}, None}},  "GreenFunction" -> {{{None, None, None, None}, None}, {{None, None, None, None, None}, None}},  "Grid" -> {{{_List}, None}},  "GridBox" -> {{{_List}, None}},  "GridGraph" -> {{{None}, _Graph}},  "GridVideo" -> {{{_List, _Integer}, None}},  "GroupBy" -> {{{None, None}, _Association}, {{None, None, None}, _Association}, {{_List, None}, _Association}},  "GroupCentralizer" -> {{{None, None}, None}},  "GroupElementFromWord" -> {{{None, None}, None}},  "GroupElementPosition" -> {{{None, None}, _Integer}},  "GroupElementToWord" -> {{{None, None}, _List}},  "GroupElements" -> {{{None}, _List}, {{None, None}, _List}},  "GroupGenerators" -> {{{None}, _List}},  "GroupMultiplicationTable" -> {{{None}, _List}},  "GroupOrbits" -> {{{None, None}, _List}, {{None, None, None}, _List}},  "GroupOrder" -> {{{None}, _Integer}},  "GroupSetwiseStabilizer" -> {{{None, None}, None}, {{None, None, None}, None}},  "GroupStabilizer" -> {{{None, None}, None}},  "GroupStabilizerChain" -> {{{None}, _List}},  "Groupings" -> {{{None, _Integer}, _List}, {{None, None}, _List}},  "GrowCutComponents" -> {{{_Image, None}, _List}},  "Gudermannian" -> {{{None}, _?NumericQ}, {{_List}, _List}},"HeatTemperatureCondition" -> {{{_?BooleanQ, None, None}, None}, {{_?BooleanQ, None, None, None}, None}},"HeatTransferPDEComponent" -> {{{None, None}, None}},"HistogramTransformInterpolation" -> {{{__}, None}},"HyperexponentialDistribution" -> {{{_List, _List}, None}},"Hypergeometric0F1Regularized" -> {{{None, None}, _?NumericQ}},"Hypergeometric1F1Regularized" -> {{{None, None, None}, _?NumericQ}},"Hypergeometric2F1Regularized" -> {{{None, None, None, None}, _?NumericQ}},"ImprovementImportance" -> {{{None, None}, None}},"In" -> {{{}, None}, {{_Integer}, None}},"Inactivate" -> {{{None}, None}},"Inactive" -> {{{None}, None}},"IncidenceGraph" -> {{{None, None}, _Graph}, {{_List, _List}, _Graph}},"Increment" -> {{{_Symbol}, _?NumericQ}},"IncrementalFunction" -> {{{None}, None}},"IncrementalObject" -> {{{__}, None}},"IncrementalReceive" -> {{{None, None}, None}},"IncrementalYield" -> {{{None, None}, None}},"IndependenceTest" -> {{{_List, _List}, None}},"IndependentPhysicalQuantity" -> {{{__}, None}},"IndependentUnit" -> {{{_String}, None}},"IndependentUnitDimension" -> {{{_String}, None}},"Indexed" -> {{{None, None}, None}},"IndexEdgeTaggedGraph" -> {{{_Graph}, _Graph}},"IndexGraph" -> {{{_Graph}, _Graph}, {{_Graph, _Integer}, _Graph}},"InertEvaluate" -> {{{None}, None}},"InertExpression" -> {{{None}, None}},"InfiniteLine" -> {{{_List}, None}},"InfiniteLineThrough" -> {{{_List}, None}},"InfinitePlane" -> {{{_List}, None}},"Infix" -> {{{_List}, None}, {{_List, None}, None}},"InflationAdjust" -> {{{None, None}, None}},"Information" -> {{{_Symbol}, None}, {{_Symbol, _String}, _String}},"InhomogeneousPoissonPointProcess" -> {{{None, _Integer}, None}},"InhomogeneousPoissonProcess" -> {{{None, None}, None}},"InitializationObject" -> {{{_String}, None}},"InitializationObjects" -> {{{}, _List}},"InitializationValue" -> {{{_Symbol}, None}},"Initialize" -> {{{None}, None}},"InnerPolygon" -> {{{None}, None}},"InnerPolyhedron" -> {{{None}, None}},"Inpaint" -> {{{_Image, _Image}, _Image}},"Input" -> {{{}, None}, {{_String}, None}},"InputField" -> {{{None, None}, None}, {{None}, None}},"InputForm" -> {{{None}, None}},"InputNamePacket" -> {{{_String}, None}},"InputNotebook" -> {{{}, None}},"InputOutputResponse" -> {{{__}, None}},"InputOutputResponseData" -> {{{__}, None}},"InputPacket" -> {{{}, None}},"InputStream" -> {{{_String, _Integer}, None}},"InputString" -> {{{}, _String}, {{_String}, _String}},"InputStringPacket" -> {{{_String}, None}},"InscribedBall" -> {{{None}, None}},"Insert" -> {{{_List, None, _Integer}, _List}, {{_List, None, _List}, _List}},"InsertColumns" -> {{{None, None, None}, None}},"InsertLinebreaks" -> {{{_String}, _String}, {{_String, _Integer}, _String}},"Inset" -> {{{None, None}, None}, {{None, None, None}, None}},"GuidedFilter"             -> {{{_Image, None, _Integer, None}, _Image}, {{_Image, _Integer, None}, _Image}},"GumbelDistribution"       -> {{{}, None}, {{None, None}, None}},"HaarWavelet"              -> {{{}, None}},"HalfLine"                 -> {{{_List}, None}, {{_List, _List}, None}},"HalfNormalDistribution"   -> {{{None}, None}},"HalfPlane"                -> {{{_List}, None}, {{_List, None}, None}},"HalfSpace"                -> {{{_List, None}, None}, {{None, None}, None}},"HalftoneShading"          -> {{{}, None}, {{_Integer}, None}, {{_String}, None}, {{_Integer, _Integer, _String}, None}},"Haloing"                  -> {{{}, None}, {{_Integer}, None}, {{_Integer, None}, None}, {{_Integer, None, _Integer}, None}},"HammingDistance"          -> {{{None, None}, _Integer}},"HammingWindow"            -> {{{None}, _?NumericQ}, {{_List}, _List}},"HankelH1"                 -> {{{_Integer, None}, _?NumericQ}, {{_Real, _Real}, _?NumericQ}, {{_List, None}, _List}},"HankelH2"                 -> {{{_Integer, None}, _?NumericQ}, {{_Real, _Real}, _?NumericQ}, {{_List, None}, _List}},"HankelTransform"          -> {{{None, None, None}, _?NumericQ}, {{None, None, None, None}, _?NumericQ}},"HannPoissonWindow"        -> {{{None}, _?NumericQ}, {{_List}, _List}},"HannWindow"               -> {{{None}, _?NumericQ}, {{_List}, _List}},"HaradaNortonGroupHN"      -> {{{}, None}},"HararyGraph"              -> {{{_Integer, _Integer}, _Graph}},"HardcorePointProcess"     -> {{{None, None, _Integer}, None}},"HarmonicMeanFilter"       -> {{{_Image, None}, _Image}, {{_List, None}, _List}},"HarmonicPolyLog"          -> {{{None, None}, _?NumericQ}},"Hash"                     -> {{{None}, _Integer}, {{None, _String}, _Integer}, {{None, _String, _String}, _String}},"HatchFilling"             -> {{{}, None}, {{_String}, None}, {{None, _Integer}, None}, {{None, _Integer, _Integer}, None}},"HatchShading"             -> {{{}, None}, {{_Integer}, None}, {{_Integer, _Integer}, None}},"Haversine"                -> {{{None}, _?NumericQ}, {{_List}, _List}},"HazardFunction"           -> {{{None, None}, _?NumericQ}},"HeadCompose"              -> {{{None, None, None, None}, None}},"HeatFluxValue"            -> {{{_?BooleanQ, None, None}, None}, {{_?BooleanQ, None, None, None}, None}},"HeatInsulationValue"      -> {{{_?BooleanQ, None, None}, None}, {{_?BooleanQ, None, None, None}, None}},"HeatOutflowValue"         -> {{{_?BooleanQ, None, None}, None}, {{_?BooleanQ, None, None, None}, None}},"HeatRadiationValue"       -> {{{_?BooleanQ, None, None}, None}, {{_?BooleanQ, None, None, None}, None}},"HeatSymmetryValue"        -> {{{_?BooleanQ, None, None}, None}, {{_?BooleanQ, None, None, None}, None}},"HeatTransferValue"        -> {{{_?BooleanQ, None, None}, None}, {{_?BooleanQ, None, None, None}, None}},"HeavisideLambda"          -> {{{None, __}, _?NumericQ}, {{_List, __}, _List}},"HeavisidePi"              -> {{{None}, _?NumericQ}, {{_List}, _List}},"HeavisideTheta"           -> {{{None}, _?NumericQ}, {{None, __}, _?NumericQ}},"HeldGroupHe"              -> {{{}, None}},"HelmholtzPDEComponent"    -> {{{None, None}, None}},"HermiteDecomposition"     -> {{{_List}, _List}},"HermiteH"                 -> {{{_Integer, None}, _?NumericQ}, {{_List, None}, _List}},"HermiteReduce"            -> {{{None, _Symbol}, _List}},"Hermitian"                -> {{{_List}, _?BooleanQ}},"HessenbergDecomposition"  -> {{{_List}, _List}},"HeunB"                    -> {{{_Integer, None, None, None, None, None}, _?NumericQ}, {{_List, None, None, None, None, None}, _List}},"HeunBPrime"               -> {{{_Integer, None, None, None, None, None}, _?NumericQ}, {{_List, None, None, None, None, None}, _List}},"HeunC"                    -> {{{_Integer, None, None, None, None, None}, _?NumericQ}, {{_List, None, None, None, None, None}, _List}},"HeunCPrime"               -> {{{_Integer, None, None, None, None, None}, _?NumericQ}, {{_List, None, None, None, None, None}, _List}},"HeunD"                    -> {{{_Integer, None, None, None, None, None}, _?NumericQ}, {{_List, None, None, None, None, None}, _List}},"HeunDPrime"                      -> {{{__}, _?NumericQ}},"HeunG"                           -> {{{__}, _?NumericQ}},"HeunGPrime"                      -> {{{__}, _?NumericQ}},"HeunT"                           -> {{{__}, _?NumericQ}},"HeunTPrime"                      -> {{{__}, _?NumericQ}},"Hexahedron"                      -> {{{_List}, None}},"HiddenMarkovProcess"             -> {{{_List, _List, _List}, None}},"Highlighted"                     -> {{{None, None}, None}},"HighlightGraph"                  -> {{{_Graph, __}, _Graph}},"HighlightImage"                  -> {{{_Image, __}, _Image}},"HighlightVideo"                  -> {{{None, None}, None}},"HighpassFilter"                  -> {{{_List, __}, _List}, {{_Image, __}, _Image}},"HigmanSimsGroupHS"               -> {{{}, None}},"HilbertCurve"                    -> {{{_Integer, __}, _Graph}},"HilbertFilter"                   -> {{{_List, __}, _List}},"HilbertTransform"                -> {{{None, None, None}, None}},"Histogram"                       -> {{{__}, _Graphics}},"Histogram3D"                     -> {{{__}, _Graphics3D}},"HistogramDistribution"           -> {{{__}, None}},"HistogramPointDensity"           -> {{{__}, None}},"HistogramTransform"              -> {{{_Image, __}, _Image}},"HistoricalPeriodData"            -> {{{None, None}, None}},"HitMissTransform"                -> {{{_Image, __}, _Image}},"HITSCentrality"                  -> {{{_Graph}, _List}},"HjorthDistribution"              -> {{{None, None, None}, None}},"HodgeDual"                       -> {{{None}, None}},"HoeffdingD"                      -> {{{_List, _List}, _Real}, {{_List}, _Real}},"HoeffdingDTest"                  -> {{{__}, None}},"Hold"                            -> {{{__}, None}},"HoldComplete"                    -> {{{__}, None}},"HoldCompleteForm"                -> {{{__}, None}},"HolderModel"                     -> {{{__}, None}},"HoldForm"                        -> {{{__}, None}},"HoldPattern"                     -> {{{None}, None}},"HolidayCalendarData"             -> {{{__}, None}},"HorizontalGauge"                 -> {{{__}, None}},"HostLookup"                      -> {{{_String}, _String}, {{_String, _String}, _String}},"HotellingTSquareDistribution"    -> {{{None, None}, None}},"HoytDistribution"             -> {{{None, None}, None}},"HTMLSave"                     -> {{{None, None}, None}},"HTTPErrorResponse"            -> {{{None}, None}},"HTTPRedirect"                 -> {{{None}, None}},"HTTPRequest"                  -> {{{_String}, None}, {{_String, _Association}, None}},"HTTPRequestData"              -> {{{None}, _String}},"HTTPResponse"                 -> {{{None}, None}},"HumanGrowthData"              -> {{{__}, None}},"HumpDownHump"                 -> {},"HumpEqual"                    -> {},"HurwitzLerchPhi"              -> {{{None, None, None}, _?NumericQ}},"HurwitzZeta"                  -> {{{None, None}, _?NumericQ}},"HyperbolicDistribution"       -> {{{None, None, None, None}, None}},"HypercubeGraph"               -> {{{_Integer}, _Graph}},"Hyperfactorial"               -> {{{_Integer}, _?NumericQ}},"HypergeometricDistribution"   -> {{{None, None, None}, None}},"Hyperlink"                    -> {{{_String, _String}, None}},"Hyperplane"                   -> {{{None, None}, None}},"HypoexponentialDistribution"  -> {{{_List}, None}},"HypothesisTestData"           -> {{{__}, None}},"IconData"                     -> {{{None, None}, None}},"Iconize"                      -> {{{None}, None}},"Icosahedron"                  -> {{{}, None}},"IfCompiled"                   -> {{{None, None}, None}},"IgnoringInactive"             -> {{{None}, None}},"ImplicitD"                    -> {{{None, None, None, None}, None}},"ImageBestColorSpace"          -> {{{_Image}, _String}},"ImageCaptureDialog"           -> {{{}, _Image}},"ImageCovariance"              -> {{{_Image}, _List}},"Insphere" -> {{{None}, None}},"Install" -> {{{_String}, None}},"InstallService" -> {{{None}, None}},"InstanceNormalizationLayer" -> {{{}, None}},"InString" -> {{{}, None}},"InterfaceSwitched" -> {{{None, None}, None}},"IntermediateTest" -> {{{None, None}, None}},"InternallyBalancedDecomposition" -> {{{None}, _List}},"InterpolatingFunction" -> {{{None}, None}},"Interpretation" -> {{{None, None}, None}},"InterpretationBox" -> {{{None, None}, None}},"Interpreter" -> {{{_String}, None}},"InterquartileRange" -> {{{_List}, _Real}},"Interrupt" -> {{{}, None}},"IntersectedEntityClass" -> {{{None, None}, None}},"Interval" -> {{{_List, __}, None}},"IntervalIntersection" -> {{{None, __}, None}},"IntervalSlider" -> {{{None}, None}},"IntervalUnion" -> {{{None, __}, None}},"InverseBetaRegularized" -> {{{_Real, _Real, _Real}, _?NumericQ}},"InverseBilateralLaplaceTransform" -> {{{None, _Symbol, _Symbol}, None}},"InverseBilateralZTransform" -> {{{None, _Symbol, _Symbol}, None}},"InverseCDF" -> {{{None, _Real}, _?NumericQ}},"InverseChiSquareDistribution" -> {{{None}, None}},"InverseContinuousWaveletTransform" -> {{{None}, _List}},"InverseDistanceTransform" -> {{{None}, _Image}},"InverseErf" -> {{{_Real}, _?NumericQ}},"InverseErfc" -> {{{_Real}, _?NumericQ}},"InverseFourierSequenceTransform" -> {{{None, _Symbol, _Symbol}, None}},"InverseGammaDistribution" -> {{{None, None}, None}},"InverseGammaRegularized" -> {{{_Real, _Real}, _?NumericQ}},"InverseGaussianDistribution" -> {{{None, None}, None}},"InverseGudermannian" -> {{{_Real}, _?NumericQ}},"InverseHankelTransform" -> {{{None, _Symbol, _Symbol}, None}},"InverseHaversine" -> {{{_Real}, _?NumericQ}},"InverseHilbertTransform" -> {{{None, _Symbol, _Symbol}, None}},"InverseImagePyramid" -> {{{None}, _Image}},"InverseJacobiCD" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiCN" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiCS" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiDC" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiDN" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiDS" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiNC" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiND" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiNS" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiSC" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiSD" -> {{{_Real, _Real}, _?NumericQ}},"InverseJacobiSN" -> {{{_Real, _Real}, _?NumericQ}},"InverseMellinTransform" -> {{{None, None, None}, None}},"InversePermutation" -> {{{None}, _List}},"InverseRadon" -> {{{_Image}, _Image}, {{_Image, None}, _Image}},"InverseRadonTransform" -> {{{None, None, None, None}, None}},"InverseSeries" -> {{{None}, None}, {{None, None}, None}},"InverseShortTimeFourier" -> {{{None}, _List}, {{None, _Integer}, _List}, {{None, _Integer, _Integer}, _List}},"InverseSpectrogram" -> {{{None}, _List}, {{_Image}, _List}, {{None, _Integer}, _List}, {{None, _Integer, _Integer}, _List}},"InverseSurvivalFunction" -> {{{None, None}, _?NumericQ}, {{None, _List}, _List}},"InverseWaveletTransform" -> {{{None}, _List}, {{None, _List}, _List}},"InverseWeierstrassP" -> {{{None, None}, _?NumericQ}},"Invisible" -> {{{None}, None}},"IPAddress" -> {{{_String}, None}},"IslandData" -> {{{___}, None}},"IsolatingInterval" -> {{{None}, _List}, {{None, None}, _List}},"IsotopeData" -> {{{None, _String}, None}, {{_String}, None}, {{_String, _String}, None}},"Item" -> {{{None}, None}, {{None, None}, None}},"ItoProcess" -> {{{None, None, None}, None}, {{None, None, None, None}, None}, {{None, None, None, None, None}, None}},"JaccardDissimilarity" -> {{{None, None}, _Real}},"JacobiEpsilon" -> {{{None, None}, _?NumericQ}, {{_List, None}, _List}},"JacobiZN" -> {{{None, None}, _?NumericQ}, {{_List, None}, _List}},"JankoGroupJ1" -> {{{}, None}},"JankoGroupJ2" -> {{{}, None}},"JankoGroupJ3" -> {{{}, None}},"JankoGroupJ4" -> {{{}, None}},"JarqueBeraALMTest" -> {{{_List}, None}, {{_List, _String}, None}},"JohnsonDistribution" -> {{{_String, None, None, None, None}, None}},"JoinedCurve" -> {{{None}, None}},"JoinForm" -> {{{_String}, None}},"JordanDecomposition" -> {{{None}, _List}},"JordanModelDecomposition" -> {{{None}, None}},"JordanReduce" -> {{{None}, _List}},"JulianDate" -> {{{}, _Real}, {{___}, _Real}},"JuliaSetBoettcher" -> {{{None, None}, _?NumericQ}},"JuliaSetPoints" -> {{{None}, _List}, {{None, None}, _List}},"KaiserBesselWindow" -> {{{None}, _?NumericQ}},"KaiserWindow" -> {{{None}, _?NumericQ}, {{None, None}, _?NumericQ}},"KalmanEstimator" -> {{{None, None}, None}, {{None, None, None}, None}},"KalmanFilter" -> {{{None, _List}, _List}},"KarhunenLoeveDecomposition" -> {{{None}, _List}, {{None, _Integer}, _List}},"KaryTree" -> {{{_Integer}, _Graph}, {{_Integer, _Integer}, _Graph}},"KatzCentrality" -> {{{None}, _List}, {{None, None}, _List}, {{None, None, None}, _List}},"KCoreComponents" -> {{{None, _Integer}, _List}, {{None}, _List}},"KDistribution" -> {{{_Integer, None, None}, None}},"KEdgeConnectedComponents" -> {{{None, _Integer}, _List}, {{None}, _List}},"KelvinBei" -> {{{_Integer, None}, _?NumericQ}, {{_List, None}, _List}},"KelvinBer" -> {{{_Integer, None}, _?NumericQ}, {{_List, None}, _List}},"KelvinKei" -> {{{_Integer, None}, _?NumericQ}, {{_List, None}, _List}},"KelvinKer" -> {{{_Integer, None}, _?NumericQ}, {{_List, None}, _List}},"KendallTau" -> {{{None, None}, _Real}, {{_List}, _Real}},"KendallTauTest" -> {{{None, None}, None}, {{None, _String}, None}},"KernelConfiguration" -> {{{}, _Association}},"KernelConfigurationEdit" -> {{{}, None}},"KernelEvaluate" -> {{{None}, None}, {{None, None}, None}},"KernelFunction" -> {{{None}, None}},"KernelMixtureDistribution" -> {{{None}, None}},"KernelModelFit" -> {{{None, None, None}, None}},"KernelObject" -> {{{None}, None}},"Kernels" -> {{{}, _List}},"Ket" -> {{{None}, None}, {{_String}, None}},"KillProcess" -> {{{None}, None}},"KirchhoffGraph" -> {{{_List}, _Graph}},"KleinInvariantJ" -> {{{_Real}, _?NumericQ}, {{None}, _?NumericQ}},"KnapsackSolve" -> {{{_List, _Real}, _List}, {{_List, _Integer}, _List}},"KnightTourGraph" -> {{{_Integer, _Integer}, _Graph}},"KnotData" -> {{{_Symbol}, None}, {{_Symbol, _String}, None}},"KochCurve" -> {{{_Integer}, None}},"KolmogorovSmirnovTest" -> {{{_List}, None}, {{_List, None}, None}},"KroneckerDelta" -> {{{_Integer, _Integer}, _Integer}, {{_Integer, __}, _Integer}},"KroneckerModelDecomposition" -> {{{None}, None}},"KroneckerProduct" -> {{{_List, _List}, _List}, {{_List, __}, _List}},"KuiperTest" -> {{{_List}, None}, {{_List, None}, None}},"KumaraswamyDistribution" -> {{{_Real, _Real}, None}},"Kurtosis" -> {{{_List}, _Real}},"KuwaharaFilter" -> {{{_Image}, _Image}, {{_Image, _Integer}, _Image}},"KVertexConnectedComponents" -> {{{_Graph, _Integer}, _List}},"Label" -> {{{None}, None}},"Labeled" -> {{{None, None}, None}, {{None, None, None}, None}},"LabeledGraphicsBox" -> {{{None}, None}},"LaguerreL" -> {{{_Integer, _Real}, _?NumericQ}, {{_Integer, _Integer, _Real}, _?NumericQ}, {{None, None}, _?NumericQ}},"LakeData" -> {{{_Symbol}, None}, {{_Symbol, _String}, None}},"LambdaComponents" -> {{{_Graph}, _List}},"LameC" -> {{{_Integer, _Integer, _Real, _Real}, _?NumericQ}},"LameCPrime" -> {{{_Integer, _Integer, _Real, _Real}, _?NumericQ}},"LameEigenvalueA" -> {{{_Integer, _Integer, _Real}, _?NumericQ}},"LameEigenvalueB" -> {{{_Integer, _Integer, _Real}, _?NumericQ}},"LameS" -> {{{_Integer, _Integer, _Real, _Real}, _?NumericQ}},"LameSPrime" -> {{{_Integer, _Integer, _Real, _Real}, _?NumericQ}},"LaminaData" -> {{{_Symbol}, None}, {{_Symbol, _String}, None}},"LanczosWindow" -> {{{_Real}, _?NumericQ}},"LandauDistribution" -> {{{_Real, _Real}, None}},"LanguageData" -> {{{_Symbol}, None}, {{_Symbol, _String}, None}},"LanguageIdentify" -> {{{_String}, None}},"LaplaceDistribution" -> {{{_Real, _Real}, None}},"Laplacian" -> {{{None, _List}, None}, {{None, _List, _List}, None}},"LaplacianFilter" -> {{{_Image}, _Image}, {{_Image, _Integer}, _Image}},"LaplacianGaussianFilter" -> {{{_Image}, _Image}, {{_Image, _Real}, _Image}},"LaplacianPDETerm" -> {{{_List, _List}, None}},"Last" -> {{{_List}, "_[1]"}, {{_Association}, "_[1]"}},"Latitude" -> {{{None}, _Real}},"LatitudeLongitude" -> {{{None}, _List}},"LexicographicOrder" -> {{{_List, _List}, _Integer}},"LexicographicSort" -> {{{_List}, _List}, {{_List, None}, _List}},"LibraryDataType" -> {{{None, __}, None}},"LibraryFunctionError" -> {{{_String, _Integer}, None}},"LibraryFunctionLoad" -> {{{_String, _String, _List, None}, None}},"LibraryFunctionUnload" -> {{{None}, None}},"LibraryLoad" -> {{{_String}, None}},"LibraryUnload" -> {{{_String}, None}},"LiftingWaveletTransform" -> {{{None}, None}, {{None, None}, None}},"Line" -> {{{_List}, None}},"LinearFractionalOptimization" -> {{{None, None, None}, _Association}, {{None, None, None, None}, _Association}},"LinearFractionalTransform" -> {{{None}, None}, {{None, None}, None}},"LinearLayer" -> {{{}, None}, {{_Integer}, None}},"LinearModelFit" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"LinearOptimization" -> {{{None, None, _List}, _Association}, {{None, None, _List, None}, _Association}},"LinearRecurrence" -> {{{_List, _List, _Integer}, _List}, {{None, None, None, None}, _List}},"LinearSolveFunction" -> {{{_List}, None}},"LineBreak" -> {},"LinebreakAdjustments" -> {},"LinebreakSemicolon" -> {},"LinebreakWithin" -> {},"LineGraph" -> {{{_Graph}, _Graph}, {{_List}, _Graph}},"LineIntegralConvolutionPlot" -> {{{_List, None, None}, _Graphics}, {{_List, None, None, None}, _Graphics}},"LinkActivate" -> {{{None}, None}},"LinkClose" -> {{{None}, None}},"LinkConnect" -> {{{}, None}, {{_String}, None}},"LinkCreate" -> {{{}, None}, {{_String}, None}},"LinkFlush" -> {{{None}, None}},"LinkFunction" -> {{{None}, None}, {{None, None}, None}},"LinkInterrupt" -> {{{None}, None}},"LinkLaunch" -> {{{_String}, None}},"LinkObject" -> {{{None, __}, None}},"LinkRead" -> {{{None}, None}, {{None, None}, None}},"LinkWrite" -> {{{None, None}, None}},"LiouvilleLambda" -> {{{_Integer}, _Integer}},"ListAnimate" -> {{{_List}, None}, {{_List, None}, None}},"ListContourPlot" -> {{{_List}, _Graphics}, {{_List, None}, _Graphics}},"ListContourPlot3D" -> {{{_List}, None}, {{_List, None}, None}},"ListCurvePathPlot" -> {{{_List}, _Graphics}, {{_List, None}, _Graphics}},"ListDeconvolve" -> {{{_List, _List}, _List}, {{_List, _List, None}, _List}},"ListDensityPlot3D" -> {{{_List}, None}, {{_List, None}, None}},"ListLogPlot" -> {{{_List}, _Graphics}, {{_List, None}, _Graphics}},"ListStepPlot" -> {{{_List}, _Graphics}, {{_List, None}, _Graphics}},"LatticeData" -> {{{}, None}, {{_String}, None}, {{_String, _String}, None}},"LatticeReduce" -> {{{_List}, _List}},"LaunchKernels" -> {{{}, _List}, {{_Integer}, _List}, {{_String}, _List}},"LDLDecomposition" -> {{{_List}, _List}},"LeapVariant" -> {{{None, None}, None}},"LearnDistribution" -> {{{None}, None}},"LearnedDistribution" -> {{{None}, None}},"LeastSquares" -> {{{_List, _List}, _List}},"LeastSquaresFilterKernel" -> {{{None, _Integer}, _List}},"LeftArrow" -> {},"LeftArrowBar" -> {},"LeftArrowRightArrow" -> {},"LeftDownTeeVector" -> {},"LeftDownVector" -> {},"LeftDownVectorBar" -> {},"LeftJoinAcross" -> {{{None, None}, None}, {{None, None, None}, None}},"LeftRightArrow" -> {},"LeftRightVector" -> {},"LeftTee" -> {},"LeftTeeArrow" -> {},"LeftTeeVector" -> {},"LeftTriangle" -> {},"LeftTriangleBar" -> {},"LeftTriangleEqual" -> {},"LeftUpDownVector" -> {},"LeftUpTeeVector" -> {},"LeftUpVector" -> {},"LeftUpVectorBar" -> {},"LeftVector" -> {},"LeftVectorBar" -> {},"Legended" -> {{{None, None}, None}},"LegendreP" -> {{{_Integer, None}, _?NumericQ}, {{_Integer, _Integer, None}, _?NumericQ}},"LengthWhile" -> {{{_List, None}, _Integer}},"LessEqualGreater" -> {},"LessEqualThan" -> {},"LessFullEqual" -> {},"LessGreater" -> {},"LessLess" -> {},"LessSlantEqual" -> {},"LessThan" -> {},"LessTilde" -> {},"LetterCounts" -> {{{_String}, _Association}, {{_List}, _Association}},"Level" -> {{{None, None}, _List}},"LeveneTest" -> {{{None}, None}, {{_List, None}, None}},"LeviCivitaTensor" -> {{{_Integer}, _List}, {{_Integer, None}, _List}},"LevyDistribution" -> {{{None, None}, None}},"LibraryFunction" -> {{{None}, None}},"LibraryFunctionDeclaration" -> {{{_String, _List, None}, None}},"LibraryFunctionInformation" -> {{{None}, _Association}},"LicenseEntitlementObject" -> {{{None}, None}},"LicenseEntitlements" -> {{{}, _List}},"LiftingFilterData" -> {{{}, _List}, {{None}, _List}, {{None, None}, _List}, {{None, None, None}, _List}},"LightDarkSwitched" -> {{{None}, None}, {{None, None}, None}},"Lighter" -> {{{None}, None}, {{None, None}, None}},"LightModePane" -> {{{None}, None}, {{None, None}, None}},"Likelihood" -> {{{None, None}, _Real}},"LindleyDistribution" -> {{{None}, None}},"LinearGradientFilling" -> {{{None}, None}},"LinearGradientImage" -> {{{None, None}, _Image}},"LinearizingTransformationData" -> {{{}, None}, {{None}, None}, {{None, None}, None}, {{None, None, None}, None}},"LinearModel" -> {{{None, None}, None}, {{None, None, None}, None}},"LineIntegrate" -> {{{None, None}, None}, {{None, None, None}, None}, {{None, None, None, None}, None}},"LineLegend" -> {{{None}, None}, {{None, None}, None}},"LinkError" -> {{{None}, None}},"LinkPatterns" -> {{{None}, _List}},"LinkRankCentrality" -> {{{None}, _List}, {{None, None}, _List}},"LinkReadHeld" -> {{{None}, None}},"Links" -> {{{}, _List}, {{None}, _List}},"LinkWriteHeld" -> {{{None, None}, None}, {{None, None, None}, None}},"ListFourierSequenceTransform" -> {{{None, None}, _List}, {{None, None, None}, _List}},"ListInterpolation" -> {{{None}, None}, {{None, None}, None}},"ListPicker" -> {{{None}, None}, {{None, None}, None}},"ListPickerBox" -> {{{None}, None}, {{None, None}, None}},"ListPlay" -> {{{None}, None}, {{None, None}, None}},"ListZTransform" -> {{{None, None}, None}, {{None, None, None}, None}},"LiteralType" -> {{{None}, None}},"LLMConfiguration" -> {{{}, None}, {{None}, None}, {{None, None}, None}, {{None, None, None}, None}},"LLMExampleFunction" -> {{{None}, None}, {{None, None}, None}},"LLMFunction" -> {{{None}, None}, {{None, None}, None}},"LLMGraph" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"LLMGraphSubmit" -> {{{}, None}, {{None}, None}, {{None, None}, None}, {{None, None, None}, None}},"LLMPrompt" -> {{{None}, None}},"LLMPromptGenerator" -> {{{None}, None}, {{None, None}, None}},"LLMResourceFunction" -> {{{None}, None}},"LLMResourceTool" -> {{{None}, None}},"LLMSynthesize" -> {{{None}, _String}, {{None, None}, _String}},"LLMSynthesizeSubmit" -> {{{None}, None}, {{None, None}, None}},"LLMTool" -> {{{None}, None}, {{None, None}, None}},"LLMToolRequest" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"LLMToolResponse" -> {{{}, None}, {{None}, None}, {{None, None}, None}, {{None, None, None}, None}},"LoadCompiledComponent" -> {{{None}, None}, {{None, None}, None}},"LocalAdaptiveBinarize" -> {{{None}, _Image}, {{None, None}, _Image}},"LocalCache" -> {{{None}, None}, {{None, None}, None}},"LocalClusteringCoefficient" -> {{{None}, _List}, {{None, None}, _List}},"LocalEvaluate" -> {{{None}, None}, {{None, None}, None}, {{None, None, None}, None}},"LocalModelFit" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"LocalObject" -> {{{None}, None}},"LocalObjects" -> {{{}, _List}, {{None}, _List}},"LocalResponseNormalizationLayer" -> {{{}, None}, {{None}, None}},"LocalSubmit" -> {{{None}, None}, {{None, None}, None}},"LocalSymbol" -> {{{None}, None}},"LocalTime" -> {{{__}, None}},"LocalTimeZone" -> {{{__}, _String}},"LocationEquivalenceTest" -> {{{None, None}, None}, {{None, None, _String}, None}},"LocationTest" -> {{{None}, None}, {{None, None}, None}, {{None, None, _String}, None}},"Locator" -> {{{_List}, None}, {{}, None}},"LocatorPane" -> {{{_List, None}, None}, {{_List, None, None}, None}},"LogBarnesG" -> {{{None}, _?NumericQ}, {{_List}, _List}},"LogGammaDistribution" -> {{{_Real, _Real, _Real}, None}},"LogicalExpand" -> {{{None}, None}},"LogisticDistribution" -> {{{_Real, _Real}, None}},"LogisticSigmoid" -> {{{None}, _Real}, {{_List}, _List}},"LogitModelFit" -> {{{_List, None, None}, None}, {{_List, None, None, None}, None}},"LogLikelihood" -> {{{None, _List}, _Real}},"LogLogisticDistribution" -> {{{_Real, _Real}, None}},"LogModel" -> {{{__}, None}},"LogMultinormalDistribution" -> {{{_List, _List}, None}},"LogNormalDistribution" -> {{{_Real, _Real}, None}},"LogRankTest" -> {{{None}, None}, {{None, None}, None}, {{None, None, _String}, None}},"LogSeriesDistribution" -> {{{_Real}, None}},"LommelS1" -> {{{None, None, None}, _?NumericQ}, {{_List, None, None}, _List}},"LommelS2" -> {{{None, None, None}, _?NumericQ}, {{_List, None, None}, _List}},"LommelT1" -> {{{None, None, None}, _?NumericQ}, {{_List, None, None}, _List}},"LommelT2" -> {{{None, None, None}, _?NumericQ}, {{_List, None, None}, _List}},"Longest" -> {{{None}, None}},"LongestCommonSequence" -> {{{_List, _List}, _List}, {{_String, _String}, _List}},"LongestCommonSequencePositions" -> {{{_List, _List}, _List}, {{_String, _String}, _List}},"LongestCommonSubsequence" -> {{{_String, _String}, _String}, {{_List, _List}, _List}},"LongestCommonSubsequencePositions" -> {{{_String, _String}, _List}, {{_List, _List}, _List}},"LongestOrderedSequence" -> {{{_List}, _List}},"Longitude" -> {{{None}, _Real}},"LongLeftArrow" -> {},"LongLeftRightArrow" -> {},"LongRightArrow" -> {},"LongShortTermMemoryLayer" -> {{{_Integer}, None}, {{_Integer, None}, None}, {{}, None}},"LowerLeftArrow" -> {},"LowerRightArrow" -> {},"LowpassFilter" -> {{{_List, None}, _List}, {{_Image, None}, _Image}, {{_List, None, _Integer}, _List}, {{_Image, None, _Integer}, _Image}},"LQEstimatorGains" -> {{{None, _List}, _List}, {{None, _List, None}, _List}},"LQGRegulator" -> {{{None, _List, _List}, None}, {{None, _List, _List, None}, None}},"LQOutputRegulatorGains" -> {{{None, _List}, _List}, {{None, _List, None}, _List}},"LQRegulatorGains" -> {{{None, _List}, _List}, {{None, _List, None}, _List}},"LQRegulatorTrain" -> {{{__}, None}},"LuccioSamiComponents" -> {{{None}, _List}, {{None, None}, _List}},"LUDecomposition" -> {{{_List}, _List}},"LunarEclipse" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"LyapunovSolve" -> {{{_List, _List}, _List}, {{_List, _List, _List}, _List}},"LyonsGroupLy" -> {{{}, None}},"MagneticFieldIntensity" -> {{{__}, None}},"MagneticFluxDensity" -> {{{__}, None}},"MagneticFluxDensityValue" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"MagneticPDEComponent" -> {{{None, None}, None}},"MagneticPotentialCondition" -> {{{_?BooleanQ, None, None}, None}, {{_?BooleanQ, None, None, None}, None}},"MagneticSymmetryValue" -> {{{_?BooleanQ, None, None}, None}, {{_?BooleanQ, None, None, None}, None}},"MagnetostaticPDEComponent" -> {{{None, None}, None}},"Magnify" -> {{{None}, None}, {{None, None}, None}},"MailExecute" -> {{{None, None}, None}},"MailFolder" -> {{{}, None}, {{_String}, None}, {{_String, None}, None}},"MailItem" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"MailReceiverFunction" -> {{{None}, None}},"MailSearch" -> {{{None, _Association}, _List}, {{_Association}, _List}, {{}, _List}},"MailServerConnect" -> {{{}, None}, {{_String}, None}, {{_String, None}, None}, {{_String, None, None}, None}},"MailServerConnection" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"MainSolve" -> {{{None}, None}, {{None, None}, None}},"Majority" -> {{{None, None, None}, _?BooleanQ}},"MakeBoxes" -> {{{None}, None}, {{None, None}, None}},"MakeExpression" -> {{{None, None}, None}},"ManagedLibraryExpressionID" -> {{{None}, _Integer}, {{None, None}, _Integer}},"ManagedObject" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"MandelbrotSetBoettcher" -> {{{None}, _?NumericQ}},"MandelbrotSetDistance" -> {{{None}, _Real}, {{None, _String}, _Real}},"ManhattanDistance" -> {{{None, None}, _Real}},"Manipulate" -> {{{None, __}, None}},"ManipulateVideo" -> {{{None, __}, None}},"Manipulator" -> {{{None, __}, None}},"MannedSpaceMissionData" -> {{{__}, None}},"MannWhitneyTest" -> {{{None}, None}, {{None, None}, None}, {{None, None, _String}, None}},"MapAll" -> {{{None, None}, None}},"MAProcess" -> {{{_List, None}, None}, {{_List, None, None}, None}},"MarchenkoPasturDistribution" -> {{{None, None}, None}},"MardiaCombinedTest" -> {{{_List}, None}, {{_List, _String}, None}},"MardiaKurtosisTest" -> {{{_List}, None}, {{_List, _String}, None}},"MardiaSkewnessTest" -> {{{_List}, None}, {{_List, _String}, None}},"MarginalDistribution" -> {{{None, _Integer}, None}},"MarkovProcessProperties" -> {{{None}, _Association}, {{None, _String}, None}},"MassConcentrationCondition" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"MassFluxValue" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"MassImpermeableBoundaryValue" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"MassOutflowValue" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"MassSymmetryValue" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"MassTransferValue" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"MassTransportPDEComponent" -> {{{None, None}, None}},"MatchingDissimilarity" -> {{{None, None}, _Real}},"MaterialShading" -> {{{_String}, None}},"MaternPointProcess" -> {{{None, None, None, _Integer}, None}},"MathematicalFunctionData" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"MathieuCharacteristicExponent" -> {{{None, None}, _?NumericQ}, {{_List, None}, _List}},"MathieuGroupM11" -> {{{}, None}},"MathieuGroupM12" -> {{{}, None}},"MathieuGroupM22" -> {{{}, None}},"MathieuGroupM23" -> {{{}, None}},"MathieuGroupM24" -> {{{}, None}},"Matrices" -> {{{_List}, None}},"MaxDate" -> {{{_List}, None}},"MaxDetect" -> {{{_Image, __}, _Image}},"MaxFilter" -> {{{_Image, __}, _Image}, {{_List, __}, _List}},"MaxLimit" -> {{{__, _Rule}, _?NumericQ}},"MaxMemoryUsed" -> {{{}, _Integer}},"MaxStableDistribution" -> {{{None, None, None, None}, None}},"MaxValue" -> {{{None, __}, _?NumericQ}},"MaxwellDistribution" -> {{{None}, None}},"McLaughlinGroupMcL" -> {{{}, None}},"MeanAbsoluteLossLayer" -> {{{}, None}},"MeanAround" -> {{{_List}, None}},"MeanClusteringCoefficient" -> {{{_Graph}, _Real}},"MeanDegreeConnectivity" -> {{{_Graph}, _Real}},"MeanDeviation" -> {{{_List}, _Real}},"MeanFilter" -> {{{_Image, __}, _Image}, {{_List, __}, _List}},"MeanGraphDistance" -> {{{_Graph}, _Real}},"MeanNeighborDegree" -> {{{_Graph}, _List}},"MeanPointDensity" -> {{{__}, _Real}},"MeanShift" -> {{{_List, __}, _List}},"MeanShiftFilter" -> {{{_Image, __}, _Image}},"MeanSquaredLossLayer" -> {{{}, None}},"MedianDeviation" -> {{{_List}, _Real}},"MedianFilter" -> {{{_Image, __}, _Image}, {{_List, __}, _List}},"MedicalTestData" -> {{{__}, None}},"MeijerGReduce" -> {{{None, None}, None}},"MeixnerDistribution" -> {{{None, None, None, None}, None}},"MellinConvolve" -> {{{None, None, None, None}, None}},"MellinTransform" -> {{{None, None, None}, None}},"MemoryAvailable" -> {{{}, _Integer}},"MemoryConstrained" -> {{{None, _Integer}, None}},"MemoryInUse" -> {{{}, _Integer}},"MenuPacket" -> {{{__}, None}},"MenuView" -> {{{_List}, None}},"Message" -> {{{__}, None}},"MessageDialog" -> {{{__}, None}},"MessageName" -> {{{_Symbol, _String}, None}},"MessagePacket" -> {{{__}, None}},"Messages" -> {{{_Symbol}, _Association}},"MeteorShowerData" -> {{{__}, None}},"MexicanHatWavelet" -> {{{}, None}},"MeyerWavelet" -> {{{}, None}},"MidDate" -> {{{_List}, None}},"Midpoint" -> {{{_List}, _List}},"MinDate" -> {{{_List}, None}},"MinDetect" -> {{{_Image, __}, _Image}},"MineralData" -> {{{__}, None}},"MinFilter" -> {{{_Image, __}, _Image}, {{_List, __}, _List}},"MinimalStateSpaceModel" -> {{{None}, None}, {{None, None}, None}},"MinimumTimeIncrement" -> {{{None}, None}},"MinkowskiQuestionMark" -> {{{None}, _?NumericQ}},"MinLimit" -> {{{None, None}, _?NumericQ}},"MinorPlanetData" -> {{{None}, None}, {{None, None}, None}, {{}, None}},"Minors" -> {{{None}, _List}, {{None, None}, _List}},"MinStableDistribution" -> {{{None, None, None}, None}},"Minus" -> {{{None}, _?NumericQ}},"MinusPlus" -> {},"MinValue" -> {{{None, None, None}, _?NumericQ}, {{None, None}, _?NumericQ}},"Missing" -> {{{}, None}, {{_String}, None}, {{_String, None}, None}},"MissingFallback" -> {{{None, None}, "_[1]"}},"MittagLefflerE" -> {{{None, None}, _?NumericQ}, {{None, None, None}, _?NumericQ}},"MixedFractionParts" -> {{{None}, _List}},"MixedMagnitude" -> {{{None, None}, None}},"MixedRadix" -> {{{None}, None}},"MixedRadixQuantity" -> {{{None, None}, None}},"MixedUnit" -> {{{None}, None}},"MixtureDistribution" -> {{{None, None}, None}},"ModelFit" -> {{{None, None, None}, None}, {{None, None, None, None}, None}},"ModelFitReport" -> {{{None}, None}},"ModelPredictiveController" -> {{{None, None}, None}},"ModularInverse" -> {{{_Integer, _Integer}, _Integer}},"ModularLambda" -> {{{None}, _?NumericQ}},"Module" -> {{{None, None}, "_[1]"}},"Molecule" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"MoleculeAlign" -> {{{None, None}, None}},"MoleculeDraw" -> {{{None}, None}},"MoleculeFeatureDistance" -> {{{None, None}, _Real}, {{None, None, None}, _Real}},"MoleculeFingerprint" -> {{{None}, _String}, {{None, None}, _String}},"MoleculeGraph" -> {{{None}, _Graph}},"MoleculeMaximumCommonSubstructure" -> {{{None, None}, None}},"MoleculeModify" -> {{{None, None}, None}},"MoleculeName" -> {{{None}, _String}},"MoleculePattern" -> {{{None}, None}},"MoleculeProperty" -> {{{None, None}, None}},"MoleculeRecognize" -> {{{_Image}, None}},"MoleculeSubstructure" -> {{{None, None}, _?BooleanQ}},"MoleculeSubstructureCases" -> {{{None, None}, _List}},"MoleculeValue" -> {{{None, None}, None}},"Moment" -> {{{None, None}, _?NumericQ}, {{None, None, None}, _?NumericQ}},"MomentConvert" -> {{{None, None}, None}},"MomentEvaluate" -> {{{None, None}, None}},"MomentGeneratingFunction" -> {{{None, None}, None}, {{None, None, None}, None}},"MomentOfInertia" -> {{{None, None}, _Real}, {{None, None, None}, _Real}},"Monitor" -> {{{None, None}, "_[1]"}},"MonsterGroupM" -> {{{}, None}},"MoonPhase" -> {{{}, _Real}, {{None}, _Real}},"MoonPhaseDate" -> {{{None}, None}, {{None, None}, None}},"MoonPosition" -> {{{}, None}, {{None}, None}},"MorletWavelet" -> {{{}, None}},"MorphologicalBinarize" -> {{{_Image}, _Image}, {{_Image, _Real}, _Image}, {{_Image, _List}, _Image}},"MorphologicalBranchPoints" -> {{{_Image}, _Image}},"MorphologicalComponents" -> {{{_Image}, _Image}, {{_Image, _Image}, _Image}},"MorphologicalGraph" -> {{{_Image}, _Graph}},"MorphologicalPerimeter" -> {{{_Image}, _Image}, {{_Image, _Real}, _Image}},"MorphologicalTransform" -> {{{_Image, None}, _Image}},"MortalityData" -> {{{__}, None}},"Most" -> {{{_List}, _List}},"MountainData" -> {{{__}, None}},"MouseAnnotation" -> {{{}, None}},"MouseAppearance" -> {{{None, _String}, None}},"MouseButtons" -> {{{}, _List}},"Mouseover" -> {{{None, None}, None}},"MousePosition" -> {{{}, _List}},"MovieData" -> {{{__}, None}},"MovingAverage" -> {{{_List, _Integer}, _List}},"MovingMap" -> {{{None, _List, _Integer}, _List}},"MovingMedian" -> {{{_List, _Integer}, _List}},"MoyalDistribution" -> {{{_Real, _Real}, None}},"Multicolumn" -> {{{_List}, None}},"MultinomialDistribution" -> {{{_Integer, _List}, None}},"MultinormalDistribution" -> {{{_List, _List}, None}},"MultiplePolyLog" -> {{{_List, _List}, _?NumericQ}},"MultipleZeta" -> {{{_List}, _?NumericQ}},"MultiplySides" -> {{{None, None}, None}},"MultivariateHypergeometricDistribution" -> {{{_Integer, _List}, None}},"MultivariatePoissonDistribution" -> {{{_Real, _List}, None}},"MultivariateTDistribution" -> {{{_List, _List, _Real}, None}},"MusicChord" -> {{{_List}, None}},"MusicDuration" -> {{{__}, None}},"MusicInterval" -> {{{__}, None}},"MusicKeySignature" -> {{{__}, None}},"MusicMeasure" -> {{{_List}, None}},"MusicMeasurements" -> {{{__}, None}},"MusicNote" -> {{{None, None}, None}},"MusicPitch" -> {{{_String}, None}},"MusicRest" -> {{{None}, None}},"MusicScale" -> {{{_String}, None}},"MusicScore" -> {{{_List}, None}},"MusicTimeSignature" -> {{{_Integer, _Integer}, None}},"MusicTransform" -> {{{None, None}, None}},"MusicVoice" -> {{{_List}, None}},"NakagamiDistribution" -> {{{_Real, _Real}, None}},"Names" -> {{{_String}, _List}},"NArgMax" -> {{{None, _List}, _List}, {{None, None}, _List}},"NArgMin" -> {{{None, _List}, _List}, {{None, None}, _List}},"NBernoulliB" -> {{{_Integer}, _Real}},"NBodySimulation" -> {{{_List, _List, _Real}, None}},"NCache" -> {{{None, None}, None}},"NCaputoD" -> {{{None, _List}, _?NumericQ}},"NContourIntegrate" -> {{{None, _List}, _?NumericQ}},"NDEigensystem" -> {{{_List, _Symbol, _List, _Integer}, _List}},"NDEigenvalues" -> {{{_List, _Symbol, _List, _Integer}, _List}},"Nearest" -> {{{_List, None}, _List}, {{_List, None, _Integer}, _List}},"NearestFunction" -> {{{_List}, None}},"NearestModel" -> {{{_List}, None}},"NearestNeighborG" -> {{{None}, None}},"NearestNeighborGraph" -> {{{_List}, _Graph}},"NearestTo" -> {{{None}, None}},"NebulaData" -> {{{__}, None}},"NeedlemanWunschSimilarity" -> {{{_String, _String}, _Real}},"Needs" -> {{{_String}, None}},"Negative" -> {{{None}, _?BooleanQ}},"NegativeBinomialDistribution" -> {{{_Integer, _Real}, None}},"NegativelyOrientedPoints" -> {{{_List}, _?BooleanQ}},"NegativeMultinomialDistribution" -> {{{_Integer, _List}, None}},"NeighborhoodData" -> {{{__}, None}},"NeighborhoodGraph" -> {{{_Graph, None}, _Graph}},"Nest" -> {{{None, None, _Integer}, "_[1]"}},"NestedGreaterGreater" -> {},"NestedLessLess" -> {},"NestGraph" -> {{{None, None, _Integer}, _Graph}},"NestTree" -> {{{None, None, _Integer}, None}},"NeumannBoundaryUnitNormal" -> {{{_List}, None}},"NeumannValue" -> {{{None, None}, None}},"NevilleThetaC" -> {{{_Real, _Real}, _?NumericQ}},"NevilleThetaD" -> {{{_Real, _Real}, _?NumericQ}},"NevilleThetaN" -> {{{_Real, _Real}, _?NumericQ}},"NevilleThetaS" -> {{{_Real, _Real}, _?NumericQ}},"NewMoon" -> {{{None}, None}},"NExpectation" -> {{{None, None}, _Real}},"NextCell" -> {{{}, None}},"NextDate" -> {{{None, None}, None}},"NextScheduledTaskTime" -> {{{None}, None}},"NextValue" -> {{{None}, None}},"NeymanScottPointProcess" -> {{{None, None, _Integer}, None}},"NFractionalD" -> {{{None, _List}, _?NumericQ}},"NightHemisphere" -> {{{}, None}},"NLineIntegrate" -> {{{None, _List, None}, _Real}},"NMaxValue" -> {{{None, _List}, _Real}, {{None, None}, _Real}},"NMinValue" -> {{{None, _List}, _Real}, {{None, None}, _Real}},"Nominal" -> {{{_List}, None}},"NominalScale" -> {{{_List}, None}},"NoncentralBetaDistribution" -> {{{_Real, _Real, _Real}, None}},"NoncentralChiSquareDistribution" -> {{{_Real, _Real}, None}},"NoncentralFRatioDistribution" -> {{{_Real, _Real, _Real}, None}},"NoncentralStudentTDistribution" -> {{{_Real, _Real}, None}},"NonCommutativeAlgebra" -> {{{_List}, None}},"NonCommutativeCollect" -> {{{None}, None}},"NonCommutativeExpand" -> {{{None}, None}},"NonCommutativeGroebnerBasis" -> {{{_List, _List}, _List}},"NonCommutativeMultiply" -> {},"NonCommutativeVariables" -> {{{None}, _List}},"NondimensionalizationTransform" -> {{{None, _List}, None}},"NoneMatch" -> {{{_List, None}, _?BooleanQ}},"NonlinearModelFit" -> {{{_List, None, _List, None}, None}},"NonlinearStateSpaceModel" -> {{{_List, _List, None, None}, None}},"NonlocalMeansFilter" -> {{{_Image, _Integer, _Real}, _Image}},"NonNegative" -> {{{None}, _?BooleanQ}},"NonPositive" -> {{{None}, _?BooleanQ}},"NorlundB" -> {{{_Integer, None}, _?NumericQ}},"NormalDistribution" -> {{{_Real, _Real}, None}},"NormalizationLayer" -> {{{}, None}},"Normalize" -> {{{_List}, _List}},"NormalizedSquaredEuclideanDistance" -> {{{_List, _List}, _Real}},"NotCongruent" -> {},"NotCupCap" -> {},"NotDoubleVerticalBar" -> {},"Notebook" -> {{{_List}, None}},"NotebookApply" -> {{{None, None}, None}},"NotebookCellData" -> {{{None}, None}},"NotebookClose" -> {{{None}, None}},"NotebookCreate" -> {{{__}, None}},"NotebookDelete" -> {{{None}, None}},"NotebookEvaluate" -> {{{None}, None}},"NotebookFind" -> {{{None, _String}, None}},"NotebookGet" -> {{{None}, None}},"NotebookImport" -> {{{_String}, None}},"NotebookInformation" -> {{{None}, _Association}},"NotebookLocate" -> {{{None}, None}},"NotebookObject" -> {{{None}, None}},"NotebookOpen" -> {{{_String}, None}},"NotebookPrint" -> {{{None}, None}},"NotebookPut" -> {{{None, None}, None}},"NotebookRead" -> {{{None}, None}},"Notebooks" -> {{{}, _List}},"NotebookSave" -> {{{None}, None}},"NotebookSelection" -> {{{None}, None}},"NotebookTemplate" -> {{{None}, None}},"NotebookWrite" -> {{{None, None}, None}},"NotElement" -> {},"NotEqualTilde" -> {},"NotExists" -> {},"NotGreater" -> {},"NotGreaterEqual" -> {},"NotGreaterFullEqual" -> {},"NotGreaterGreater" -> {},"NotGreaterLess" -> {},"NotGreaterSlantEqual" -> {},"NotGreaterTilde" -> {},"Nothing" -> {},"NotHumpDownHump" -> {},"NotHumpEqual" -> {},"NotLeftTriangle" -> {},"NotLeftTriangleBar" -> {},"NotLeftTriangleEqual" -> {},"NotLess" -> {},"NotLessEqual" -> {},"NotLessFullEqual" -> {},"NotLessGreater" -> {},"NotLessLess" -> {},"NotLessSlantEqual" -> {},"NotLessTilde" -> {},"NotNestedGreaterGreater" -> {},"NotNestedLessLess" -> {},"NotPrecedes" -> {},"NotPrecedesEqual" -> {},"NotPrecedesSlantEqual" -> {},"NotPrecedesTilde" -> {},"NotReverseElement" -> {},"NotRightTriangle" -> {},"NotRightTriangleBar" -> {},"NotRightTriangleEqual" -> {},"NotSquareSubset" -> {},"NotSquareSubsetEqual" -> {},"NotSquareSuperset" -> {},"NotSquareSupersetEqual" -> {},"NotSubset" -> {},"NotSubsetEqual" -> {},"NotSucceeds" -> {},"NotSucceedsEqual" -> {},"NotSucceedsSlantEqual" -> {},"NotSucceedsTilde" -> {},"NotSuperset" -> {},"NotSupersetEqual" -> {},"NotTilde" -> {},"NotTildeEqual" -> {},"NotTildeFullEqual" -> {},"NotTildeTilde" -> {},"NotVerticalBar" -> {},"NProbability" -> {{{None, None}, _Real}, {{None, None, __}, _Real}},"NProduct" -> {{{None, _List}, _Real}, {{None, _List, _List, __}, _Real}},"NRoots" -> {{{None, _Symbol}, _List}},"NSolveValues" -> {{{None, None}, _List}, {{None, None, None}, _List}},"NSum" -> {{{None, _List}, _Real}, {{None, _List, _List, __}, _Real}},"NSurfaceIntegrate" -> {{{None, _List, None}, _Real}},"NuclearExplosionData" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"NuclearReactorData" -> {{{}, None}, {{None}, None}, {{None, None}, None}},"NumeratorDenominator" -> {{{None}, _List}},"NumericalSort" -> {{{_List}, _List}},"NumericArray" -> {{{_List, _String}, None}},"NumericArrayType" -> {{{None}, _String}},"NuttallWindow" -> {{{_Real}, _?NumericQ}},"NValues" -> {{{_Symbol}, _List}},"O" -> {{{_Integer}, None}},"ObjectTrackingData" -> {{{__}, None}},"ObservabilityGramian" -> {{{None}, _List}},"ObservableDecomposition" -> {{{None}, None}},"OceanData" -> {{{__}, None}},"Octahedron" -> {{{}, None}},"Off" -> {{{__}, None}},"Offset" -> {{{_List, __}, None}},"On" -> {{{__}, None}},"ONanGroupON" -> {{{}, None}},"Once" -> {{{None}, "_[1]"}},"Opacity" -> {{{_Real}, None}, {{_Real, None}, None}},"OpaqueRawPointer" -> {{{__}, None}},"Opener" -> {{{None}, None}},"OpenerView" -> {{{_List}, None}},"Opening" -> {{{_Image, __}, _Image}},"OpenTemporary" -> {{{}, None}},"Operate" -> {{{None, None}, None}},"OperationDeclaration" -> {{{__}, None}},"OperatorApplied" -> {{{None, _Integer}, None}},"OptimumFlowData" -> {{{_Graph, None, None}, None}},"Optional" -> {{{None}, None}},"OptionalElement" -> {{{None}, None}},"Options" -> {{{_Symbol}, _List}},"OptionsPattern" -> {{{}, None}},"OptionValue" -> {{{None}, "_[1]"}},"OrbitalElements" -> {{{None, None}, _Association}},"Order" -> {{{None, None}, _Integer}},"OrderDistribution" -> {{{_List, _Integer}, None}},"OrderedSchurDecomposition" -> {{{_List}, _List}},"OrderingBy" -> {{{_List, None}, _List}},"OrderingLayer" -> {{{}, None}},"OrderlessPatternSequence" -> {{{__}, None}},"Ordinal" -> {{{None}, None}},"OrdinalScale" -> {{{_List}, None}},"OrnsteinUhlenbeckProcess" -> {{{_Real, _Real, _Real}, None}},"Orthogonalize" -> {{{_List}, _List}},"Out" -> {{{_Integer}, None}},"OuterPolygon" -> {{{None}, None}},"OuterPolyhedron" -> {{{None}, None}},"OutputNamePacket" -> {{{_String}, None}},"OutputResponse" -> {{{None, None}, None}},"OutputStream" -> {{{_String, _Integer}, None}},"OverBar" -> {{{None}, None}},"OverDot" -> {{{None}, None}},"OverHat" -> {{{None}, None}},"Overlay" -> {{{_List}, None}},"OverlayVideo" -> {{{None, None}, None}},"Overscript" -> {{{None, None}, None}},"OverscriptBox" -> {{{None, None}, None}},"OverTilde" -> {{{None}, None}},"OverVector" -> {{{None}, None}},"OwenT" -> {{{None, None}, _?NumericQ}},"PackageExported" -> {{{_Symbol}, None}},"PackageImport" -> {{{_String}, None}},"PackageInitialize" -> {{{None}, None}},"PackageScoped" -> {{{_Symbol}, None}},"PacletDataRebuild" -> {{{}, None}},"PacletDisable" -> {{{None}, None}},"PacletEnable" -> {{{None}, None}},"PacletFind" -> {{{_String}, _List}, {{None}, _List}},"PacletFindRemote" -> {{{_String}, _List}, {{None}, _List}},"PacletInstall" -> {{{_String}, None}, {{None}, None}},"PacletInstallSubmit" -> {{{_String}, None}, {{None}, None}},"PacletObject" -> {{{None}, None}},"PacletSiteObject" -> {{{None}, None}},"PacletSiteRegister" -> {{{_String}, None}},"PacletSites" -> {{{}, _List}},"PacletSiteUnregister" -> {{{None}, None}},"PacletSiteUpdate" -> {{{None}, None}},"PacletSymbol" -> {{{_String, _String}, None}},"PacletUninstall" -> {{{None}, None}},"PaddingLayer" -> {{{_List}, None}, {{_List, None}, None}},"PadeApproximant" -> {{{None, None}, None}},"PadLeft" -> {{{_List, _Integer}, _List}, {{_List, _Integer, None}, _List}, {{_List}, _List}},"PadRight" -> {{{_List, _Integer}, _List}, {{_List, _Integer, None}, _List}, {{_List}, _List}},"PageRankCentrality" -> {{{_Graph}, _List}, {{_Graph, None}, _List}},"PairCorrelationG" -> {{{None}, None}},"PairedHistogram" -> {{{_List, _List}, _Graphics}, {{None}, _Graphics}},"PairedSmoothHistogram" -> {{{_List, _List}, _Graphics}, {{None}, _Graphics}},"PairedTTest" -> {{{None, None}, None}, {{None}, None}},"PairedZTest" -> {{{None, None}, None}, {{None}, None}},"PairwiseDensityHistogram" -> {{{None}, _Graphics}},"PairwiseSmoothDensityHistogram" -> {{{None}, _Graphics}},"PaletteNotebook" -> {{{None}, None}},"Pane" -> {{{None}, None}, {{None, None}, None}},"Panel" -> {{{None}, None}, {{None, None}, None}},"PaneSelector" -> {{{_List, None}, None}, {{_List, None, None}, None}},"ParabolicCylinderD" -> {{{None, None}, _?NumericQ}},"ParallelArray" -> {{{None, _Integer}, _List}, {{None, _List}, _List}},"ParallelCases" -> {{{None, None}, _List}, {{None, None, None}, _List}},"ParallelCombine" -> {{{None, _List}, _List}, {{None, _List, None}, _List}},"ParallelDo" -> {{{None, _List}, None}},"Parallelepiped" -> {{{_List, _List}, None}},"ParallelEvaluate" -> {{{None}, _List}},
"Parallelize" -> {  {{None}, None}
},
"ParallelKernels" -> {  {{}, _List},
  {{_String}, _List}
},
"ParallelMap" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"ParallelNeeds" -> {  {{_String}, None}
},
"Parallelogram" -> {  {{None, _List}, None}
},
"ParallelProduct" -> {  {{None, _List, __}, _?NumericQ}
},
"ParallelSelect" -> {  {{None, None}, _List}
},
"ParallelSubmit" -> {  {{None}, None},
  {{_List, None}, None}
},
"ParallelSum" -> {  {{None, _List, __}, _?NumericQ}
},
"ParallelTable" -> {  {{None, _List, __}, _List}
},
"ParallelTry" -> {  {{None, _List}, None},
  {{None, _List, _Integer}, _List}
},
"ParameterMixtureDistribution" -> {  {{None, None}, None}
},
"ParametricConvexOptimization" -> {  {{None, None, None, None}, None},
  {{None, None, None, None, _String}, None}
},
"ParametricFunction" -> {  {{None, __}, None}
},
"ParametricNDSolve" -> {  {{None, None, _List, None}, None}
},
"ParametricNDSolveValue" -> {  {{None, None, _List, None}, None}
},
"ParametricRampLayer" -> {  {{}, None},
  {{None}, None}
},
"ParentBox" -> {  {{None}, None}
},
"ParentCell" -> {  {{None}, None}
},
"Parenthesize" -> {  {{None, None, None}, None},
  {{None, None, None, None}, None},
  {{None, None, None, None, None}, None}
},
"ParentNotebook" -> {  {{None}, None}
},
"ParetoDistribution" -> {  {{_Real, _Real}, None},
  {{_Real, _Real, _Real}, None},
  {{_Real, _Real, _Real, _Real}, None}
},
"ParetoPickandsDistribution" -> {  {{_Real, _Real, _Real}, None},
  {{_Real}, None}
},
"ParkData" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"PartialCorrelationFunction" -> {  {{None, None}, None}
},
"PartialFractionElements" -> {  {{None, None}, _List},
  {{None, None, _String}, None}
},
"PartialFractions" -> {  {{None, None}, None}
},
"ParticleAcceleratorData" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"ParticleData" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"PartLayer" -> {  {{None}, None},
  {{_List}, None}
},
"PartOfSpeech" -> {  {{_String}, _List}
},
"ParzenWindow" -> {  {{None}, _?NumericQ}
},
"PascalBinomial" -> {  {{_Integer, _Integer}, _Integer}
},
"PascalDistribution" -> {  {{_Integer, _Real}, None}
},
"Paste" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"PasteButton" -> {  {{None}, None},
  {{None, None}, None}
},
"PathGraph" -> {  {{_List}, _Graph},
  {{_List, _List}, _Graph}
},
"Pattern" -> {  {{None, None}, _Pattern}
},
"PatternFilling" -> {  {{None}, _PatternFilling},
  {{_String}, _PatternFilling},
  {{None, None}, _PatternFilling},
  {{None, None, _List}, _PatternFilling}
},
"PatternReaction" -> {  {{_Rule}, None},
  {{_Rule, None}, None},
  {{_String}, None}
},
"PatternSequence" -> {  {{__}, None}
},
"PatternTest" -> {  {{None, None}, None}
},
"PaulWavelet" -> {  {{}, None},
  {{_Integer}, None}
},
"Pause" -> {  {{_Real}, None}
},
"PDF" -> {  {{None, None}, _?NumericQ},
  {{None, _List}, _?NumericQ},
  {{None}, None}
},
"PeakDetect" -> {  {{_List}, _List},
  {{_List, None}, _List},
  {{_List, None, None}, _List},
  {{_List, None, None, None}, _List}
},
"PeanoCurve" -> {  {{_Integer}, None}
},
"PearsonChiSquareTest" -> {  {{_List}, None},
  {{_List, None}, None},
  {{_List, None, _String}, None}
},
"PearsonCorrelationTest" -> {  {{_List, _List}, None},
  {{_List, _List, _String}, None}
},
"PearsonDistribution" -> {  {{_Real, _Real, _Real, _Real, _Real}, None},
  {{_Integer, _Real, _Real, _Real, _Real, _Real}, None}
},
"PenttinenPointProcess" -> {  {{None, None, None, None}, None}
},
"PercentForm" -> {  {{None}, None},
  {{None, _Integer}, None}
},
"PerceptronModel" -> {  {{__}, None}
},
"Perimeter" -> {  {{None}, _?NumericQ},
  {{None, None, None}, _?NumericQ},
  {{None, None, None, None}, _?NumericQ}
},
"PeriodicBoundaryCondition" -> {  {{None, None, None}, None}
},
"PeriodicModel" -> {  {{__}, None}
},
"Periodogram" -> {  {{_List, __}, _Graphics}
},
"PeriodogramArray" -> {  {{_List, __}, _List}
},
"Permanent" -> {  {{_List}, _?NumericQ}
},
"PermissionsGroup" -> {  {{_String}, None},
  {{None, _String}, None}
},
"PermissionsGroups" -> {  {{}, _List}
},
"PermissionsKey" -> {  {{_String}, None}
},
"PermissionsKeys" -> {  {{}, _List}
},
"PermutationCycles" -> {  {{None}, None}
},
"PermutationGroup" -> {  {{__}, None}
},
"PermutationLength" -> {  {{None}, _Integer}
},
"PermutationMax" -> {  {{None}, _Integer}
},
"PermutationMin" -> {  {{None}, _Integer}
},
"PermutationOrder" -> {  {{None}, _Integer}
},
"PermutationPower" -> {  {{None, _Integer}, None}
},
"PermutationProduct" -> {  {{__}, None}
},
"PermutationReplace" -> {  {{None, None}, None}
},
"PermutationSupport" -> {  {{None}, _List}
},
"Permute" -> {  {{None, None}, None}
},
"PeronaMalikFilter" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image},
  {{_Image, None, None}, _Image},
  {{_Image, None, None, None}, _Image}
},
"PerpendicularBisector" -> {  {{None}, None},
  {{None, None}, None}
},
"PersistenceLocation" -> {  {{_String}, None},
  {{_String, None}, None}
},
"PersistentObject" -> {  {{_String, None}, None}
},
"PersistentObjects" -> {  {{}, _List},
  {{_String}, _List},
  {{_String, None}, _List}
},
"PersistentSymbol" -> {  {{_String}, None},
  {{_String, None}, None}
},
"PersistentValue" -> {  {{None}, None},
  {{None, None}, None}
},
"PersonData" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"PERTDistribution" -> {  {{_List, None}, None},
  {{_List, None, None}, None}
},
"PetersenGraph" -> {  {{}, _Graph},
  {{None, None}, _Graph}
},
"PfaffianDet" -> {  {{None}, _?NumericQ}
},
"PhaseMargins" -> {  {{None}, _List}
},
"PhongShading" -> {  {{__}, None}
},
"PhysicalSystemData" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"PIDData" -> {  {{__}, None}
},
"PIDTune" -> {  {{__}, None}
},
"Piecewise" -> {  {{_List}, None},
  {{_List, None}, None}
},
"PiecewiseExpand" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"PillaiTrace" -> {  {{_List, _List}, _?NumericQ}
},
"PillaiTraceTest" -> {  {{_List, _List}, None},
  {{_List, _List, _String}, None}
},
"PingTime" -> {  {{None}, None},
  {{None, _Integer}, _List}
},
"PitchRecognize" -> {  {{None}, None},
  {{None, None}, None}
},
"PivotFromColumns" -> {  {{None, None}, None}
},
"PivotTable" -> {  {{None, None, None, None}, None}
},
"PivotToColumns" -> {  {{None, None}, None}
},
"PixelValue" -> {  {{_Image, None}, None},
  {{_Image, None, _String}, None}
},
"PixelValuePositions" -> {  {{_Image, None}, _List},
  {{_Image, None, None}, _List}
},
"Placed" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"Placeholder" -> {  {{__}, None}
},
"PlaceholderLayer" -> {  {{__}, None}
},
"PlanarAngle" -> {  {{None, __}, _?NumericQ}
},
"PlanarGraph" -> {  {{__}, _Graph}
},
"PlanckRadiationLaw" -> {  {{None, None}, _?NumericQ}
},
"PlaneCurveData" -> {  {{None, __}, None}
},
"PlanetaryMoonData" -> {  {{None, __}, None}
},
"PlanetData" -> {  {{None, __}, None}
},
"PlantData" -> {  {{None, __}, None}
},
"Play" -> {  {{None, _List}, None}
},
"PlotGrid" -> {  {{_List}, _Graphics},
  {{_List, None}, _Graphics}
},
"Pluralize" -> {  {{_String}, _String},
  {{None, None}, _String}
},
"PlusMinus" -> {},"Pochhammer" -> {  {{None, None}, _?NumericQ}
},
"Point" -> {  {{None}, None}
},
"PointCountDistribution" -> {  {{None, None}, None}
},
"PointDensity" -> {  {{None, __}, None}
},
"PointDensityFunction" -> {  {{__}, None}
},
"PointLegend" -> {  {{_List}, None},
  {{_List, None}, None}
},
"PointLight" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"PointProcessFitTest" -> {  {{None, __}, None}
},
"PointProcessParameterAssumptions" -> {  {{None}, None}
},
"PointSize" -> {  {{None}, None}
},
"PointStatisticFunction" -> {  {{__}, None}
},
"PoissonConsulDistribution" -> {  {{None, None}, None}
},
"PoissonDistribution" -> {  {{None}, None}
},
"PoissonPDEComponent" -> {  {{None, None}, None}
},
"PoissonPointProcess" -> {  {{None, _Integer}, None}
},
"PoissonProcess" -> {  {{None}, None}
},
"PoissonWindow" -> {  {{None}, _?NumericQ},
  {{None, None}, _?NumericQ}
},
"PolarCurve" -> {  {{None, _List}, None}
},
"PolarDecomposition" -> {  {{None}, _List}
},
"PolyaAeppliDistribution" -> {  {{None, None}, None}
},
"PolyGamma" -> {  {{None}, _?NumericQ},
  {{None, None}, _?NumericQ}
},
"Polygon" -> {  {{_List}, None},
  {{_List, None}, None}
},
"PolygonAngle" -> {  {{None}, _List},
  {{None, None}, _?NumericQ}
},
"PolygonCoordinates" -> {  {{None}, _List}
},
"PolygonDecomposition" -> {  {{None, __}, _List}
},
"Polyhedron" -> {  {{_List}, None},
  {{_List, None}, None}
},
"PolyhedronAngle" -> {  {{None, None}, _?NumericQ}
},
"PolyhedronCoordinates" -> {  {{None}, _List}
},
"PolyhedronData" -> {  {{None, __}, None}
},
"PolyhedronDecomposition" -> {  {{None}, _List}
},
"PolyhedronGenus" -> {  {{None}, _Integer}
},
"PolyLog" -> {  {{None, None}, _?NumericQ},
  {{None, None, None}, _?NumericQ}
},
"PoolingLayer" -> {  {{None, __}, None}
},
"PopovDecomposition" -> {  {{None}, _List},
  {{None, None}, _List}
},
"PopupMenu" -> {  {{None, _List, __}, None}
},
"PopupView" -> {  {{_List}, None}
},
"PopupWindow" -> {  {{None, None}, None}
},
"PositionIndex" -> {  {{_List}, _Association}
},
"PositionLargest" -> {  {{_List}, _List}
},
"PositionSmallest" -> {  {{_List}, _List}
},
"Positive" -> {  {{None}, _?BooleanQ}
},
"PositivelyOrientedPoints" -> {  {{_List}, _?BooleanQ}
},
"Postfix" -> {  {{None}, None}
},
"PowerDistribution" -> {  {{None, None}, None}
},
"PowerModel" -> {  {{None, None}, None}
},
"PowerRange" -> {  {{None, None, None}, _List}
},
"PowerSpectralDensity" -> {  {{None, None}, None}
},
"PowersRepresentations" -> {  {{_Integer, _Integer, _Integer}, _List}
},
"Precedence" -> {  {{None}, _Real}
},
"PrecedenceForm" -> {  {{None, _Integer}, None}
},
"Precedes" -> {},"PrecedesEqual" -> {},"PrecedesSlantEqual" -> {},"PrecedesTilde" -> {},"Precision" -> {  {{None}, _?NumericQ}
},
"PreDecrement" -> {  {{None}, _?NumericQ}
},
"Predict" -> {  {{None}, None}
},
"PreemptProtect" -> {  {{None}, None}
},
"Prefix" -> {  {{None}, None}
},
"PreIncrement" -> {  {{None}, _?NumericQ}
},
"PrependLayer" -> {  {{}, None}
},
"PreviousCell" -> {  {{None}, None}
},
"PreviousDate" -> {  {{None, None}, None}
},
"PriceGraphDistribution" -> {  {{None, None, None}, None}
},
"PrimeZetaP" -> {  {{None}, _?NumericQ}
},
"PrimitiveRoot" -> {  {{_Integer}, _Integer}
},
"PrincipalComponents" -> {  {{_List}, _List}
},
"Printout3D" -> {  {{None}, None}
},
"Prism" -> {  {{_List}, None}
},
"PrivateKey" -> {  {{_Association}, None}
},
"Probability" -> {  {{None, __}, _?NumericQ}
},
"ProbabilityDistribution" -> {  {{None, __}, None}
},
"ProbitModelFit" -> {  {{None, None, None}, None}
},
"ProcessConnection" -> {  {{None, _String}, None}
},
"Processes" -> {  {{}, _List}
},
"ProcessInformation" -> {  {{None, None}, None}
},
"ProcessObject" -> {  {{__}, None}
},
"ProcessParameterAssumptions" -> {  {{None}, _?BooleanQ}
},
"ProcessStatus" -> {  {{None, None}, None}
},
"Product" -> {  {{None, __}, _?NumericQ}
},
"ProductDistribution" -> {  {{__}, None}
},
"ProgressIndicator" -> {  {{None, None}, None}
},
"Projection" -> {  {{_List, _List}, _List}
},
"PromptForm" -> {  {{None, None}, None}
},
"ProofObject" -> {  {{__}, None}
},

"Property" -> {  {{None, None}, None}
},
"PropertyValue" -> {  {{None, None}, None}
},
"Proportion" -> {},"Proportional" -> {},"Protect" -> {  {{None, __}, _List}
},
"ProteinData" -> {  {{None}, None},
  {{None, None}, None}
},
"Pruning" -> {  {{_Image}, _Image},
  {{_Image, _Integer}, _Image}
},
"PsychrometricPropertyData" -> {  {{None}, None}
},
"PublicKey" -> {  {{None}, None}
},
"PulsarData" -> {  {{None}, None},
  {{None, None}, None}
},
"Put" -> {  {{None, _String}, None}
},
"PutAppend" -> {  {{None, _String}, None}
},
"Pyramid" -> {  {{_List}, None}
},
"QBinomial" -> {  {{_Integer, _Integer, None}, _?NumericQ}
},
"QFactorial" -> {  {{_Integer, None}, _?NumericQ}
},
"QGamma" -> {  {{None, None}, _?NumericQ}
},
"QnDispersion" -> {  {{_List}, _?NumericQ}
},
"QPochhammer" -> {  {{None, None}, _?NumericQ},
  {{None, None, None}, _?NumericQ}
},
"QPolyGamma" -> {  {{None, None}, _?NumericQ},
  {{None, None, None}, _?NumericQ}
},
"QRDecomposition" -> {  {{_List}, _List}
},
"QuadraticOptimization" -> {  {{None, None, None}, None}
},
"Quantile" -> {  {{None, None}, None},
  {{None, None, _List}, None}
},
"Quantity" -> {  {{None}, None},
  {{None, None}, None}
},
"QuantityArray" -> {  {{_List, None}, None}
},
"QuantityDistribution" -> {  {{None, None}, None}
},
"QuantityForm" -> {  {{None}, None},
  {{None, None}, None}
},
"QuantityMagnitude" -> {  {{None}, _?NumericQ},
  {{None, None}, _?NumericQ}
},
"QuantityUnit" -> {  {{None}, None}
},
"QuantityVariable" -> {  {{_String, None}, None},
  {{_String, None, None}, None}
},
"QuantityVariableCanonicalUnit" -> {  {{None}, _String}
},
"QuantityVariableDimensions" -> {  {{None}, _List}
},
"QuantityVariableIdentifier" -> {  {{None}, _String}
},
"QuantityVariablePhysicalQuantity" -> {  {{None}, _String}
},
"QuartileDeviation" -> {  {{None}, _?NumericQ}
},
"Quartiles" -> {  {{None}, _List}
},
"QuartileSkewness" -> {  {{None}, _?NumericQ}
},
"Query" -> {  {{None, __}, None}
},
"QuestionGenerator" -> {  {{None, __}, None}
},
"QuestionInterface" -> {  {{None, __}, None}
},
"QuestionObject" -> {  {{None, __}, None}
},
"QuestionSelector" -> {  {{None, __}, None}
},
"QueueingNetworkProcess" -> {  {{None, __}, None}
},
"QueueingProcess" -> {  {{None, __}, None}
},
"QueueProperties" -> {  {{None, None}, None}
},
"QuietEcho" -> {  {{None}, None}
},
"Quit" -> {  {{}, None},
  {{_Integer}, None}
},
"QuotientRemainder" -> {  {{None, None}, _List}
},
"RadialGradientFilling" -> {  {{_List, __}, None}
},
"RadialGradientImage" -> {  {{_List}, _Image},
  {{_List, None}, _Image}
},
"RadialityCentrality" -> {  {{_Graph}, _List},
  {{_Graph, None}, _?NumericQ}
},
"RadicalBox" -> {  {{None, None}, None}
},
"RadioButton" -> {  {{None, None}, None}
},
"RadioButtonBar" -> {  {{None, _List}, None}
},
"Radon" -> {  {{_Image}, _Image}
},
"RadonTransform" -> {  {{None, _List, _List}, None}
},
"RamanujanTau" -> {  {{_Integer}, _Integer}
},
"RamanujanTauL" -> {  {{None}, _?NumericQ}
},
"RamanujanTauTheta" -> {  {{None}, _?NumericQ}
},
"RamanujanTauZ" -> {  {{None}, _?NumericQ}
},
"Ramp" -> {  {{None}, _?NumericQ}
},
"RangeFilter" -> {  {{None, None}, None}
},
"RangeSpace" -> {  {{_List}, _List}
},
"RankDecomposition" -> {  {{_List}, _List}
},
"RankedMax" -> {  {{_List, _Integer}, _?NumericQ}
},
"RankedMin" -> {  {{_List, _Integer}, _?NumericQ}
},
"RarerProbability" -> {  {{None, None}, _?NumericQ}
},
"Raster" -> {  {{_List, __}, None}
},
"Raster3D" -> {  {{_List, __}, None}
},
"RasterArray" -> {  {{_List, __}, None}
},
"Rasterize" -> {  {{None}, _Image},
  {{None, _String}, None}
},
"RawBoxes" -> {  {{None}, None}
},
"RawData" -> {  {{None}, None}
},
"RawMemoryAllocate" -> {  {{None, _Integer}, None}
},
"RawMemoryExport" -> {  {{None, _String}, None}
},
"RawMemoryFree" -> {  {{None}, None}
},
"RawMemoryImport" -> {  {{_String, None}, None}
},
"RawMemoryRead" -> {  {{None}, None}
},
"RawMemoryWrite" -> {  {{None, None}, None}
},
"RawPointer" -> {},"RayleighDistribution" -> {  {{None}, None}
},
"ReactionBalance" -> {  {{None}, None}
},
"ReactionPDETerm" -> {  {{None, None}, None}
},
"Read" -> {  {{None}, None},
  {{None, None}, None}
},
"ReadByteArray" -> {  {{None}, None},
  {{None, _Integer}, None}
},
"ReapVideo" -> {  {{None, __}, None}
},
"Rectangle" -> {  {{_List}, None},
  {{_List, _List}, None}
},
"RectangularRepeatingElement" -> {  {{None, __}, None}
},
"RecurrenceFilter" -> {  {{None, None}, None}
},
"RecurrenceTable" -> {  {{_List, _Symbol, _List}, _List}
},
"Refine" -> {  {{None, None}, None}
},
"ReflectionTransform" -> {  {{_List}, None},
  {{_List, _List}, None}
},
"Refresh" -> {  {{None, __}, None}
},
"RegisterExceptionType" -> {  {{None, __}, None}
},
"RegisterExternalEvaluator" -> {  {{_String, _String}, None}
},
"RegularExpression" -> {  {{_String}, None}
},
"RegularPolygon" -> {  {{_Integer}, None},
  {{None, _Integer}, None}
},
"ReIm" -> {  {{None}, _List}
},
"Reinstall" -> {  {{None}, None}
},
"RelationalDatabase" -> {  {{None, __}, None}
},
"RelationGraph" -> {  {{None, _List}, _Graph},
  {{None, _List, _List}, _Graph}
},
"ReleaseHold" -> {  {{None}, None}
},
"ReliabilityDistribution" -> {  {{None, _List}, None}
},
"ReliefImage" -> {  {{_List}, _Image}
},
"Remesh" -> {  {{None}, None},
  {{None, _Integer}, None}
},
"RemoteBatchJobAbort" -> {  {{None}, None}
},
"RemoteBatchJobObject" -> {  {{None}, None}
},
"RemoteBatchJobs" -> {  {{}, _List},
  {{None}, _List}
},
"RemoteBatchMapSubmit" -> {  {{None, None, _List}, None}
},
"RemoteBatchSubmissionEnvironment" -> {  {{_String, __}, None}
},
"RemoteBatchSubmit" -> {  {{None, None}, None}
},
"RemoteConnect" -> {  {{None}, None}
},
"RemoteConnectionObject" -> {  {{None}, None}
},
"RemoteEvaluate" -> {  {{None, None}, None}
},
"RemoteKernelObject" -> {  {{None}, None}
},
"RemoteRun" -> {  {{None, _String}, None}
},
"RemoteRunProcess" -> {  {{None, None}, None}
},
"Remove" -> {  {{None, __}, None}
},
"RemoveAlphaChannel" -> {  {{_Image}, _Image}
},
"RemoveAsynchronousTask" -> {  {{None}, None}
},
"RemoveAudioStream" -> {  {{None}, None}
},
"RemoveBackground" -> {  {{_Image}, _Image}
},
"RemoveChannelListener" -> {  {{None}, None}
},
"RemoveChannelSubscribers" -> {  {{None, None}, None}
},
"Removed" -> {  {{_String}, None}
},
"RemoveDiacritics" -> {  {{_String}, _String}
},
"RemoveInputStreamMethod" -> {  {{_String}, None}
},
"RemoveOutputStreamMethod" -> {  {{_String}, None}
},
"RemoveProperty" -> {  {{None, None}, None}
},
"RemoveScheduledTask" -> {  {{None}, None}
},
"RemoveUsers" -> {  {{None, None}, None}
},
"RemoveVideoStream" -> {  {{None}, None}
},
"RenameColumns" -> {  {{None, None}, None}
},
"RenewalProcess" -> {  {{None}, None}
},
"Repeated" -> {  {{None}, None},
  {{None, None}, None}
},
"RepeatedNull" -> {  {{None}, None},
  {{None, None}, None}
},
"RepeatedTiming" -> {  {{None}, _List},
  {{None, None}, _List}
},
"RepeatingElement" -> {  {{None}, None},
  {{None, None}, None}
},
"ReplaceAt" -> {  {{None, None, None}, None}
},
"ReplaceImageValue" -> {  {{_Image, _Rule}, _Image}
},
"ReplacePixelValue" -> {  {{_Image, _Rule}, _Image}
},
"ReplicateLayer" -> {  {{None}, None}
},
"ResamplingAlgorithmData" -> {  {{None}, None},
  {{None, _String}, None}
},
"Rescale" -> {  {{None}, None},
  {{None, _List}, None},
  {{None, _List, _List}, None}
},
"RescalingTransform" -> {  {{_List}, None}
},
"ResetScheduledTask" -> {  {{None}, None}
},
"ReshapeLayer" -> {  {{None}, None}
},
"Residue" -> {  {{None, _List}, _?NumericQ}
},
"ResidueSum" -> {  {{None, _List}, _?NumericQ}
},
"ResizeLayer" -> {  {{None}, None}
},
"Resolve" -> {  {{None}, _?BooleanQ},
  {{None, None}, _?BooleanQ}
},
"ResourceData" -> {  {{None}, None},
  {{None, None}, None}
},
"ResourceFunction" -> {  {{None}, None}
},
"ResourceObject" -> {  {{None}, None}
},
"ResourceRegister" -> {  {{None}, None}
},
"ResourceRemove" -> {  {{None}, None}
},
"ResourceSearch" -> {  {{_String}, None},
  {{_String, _String}, None}
},
"ResourceSubmit" -> {  {{None}, None}
},
"ResourceUpdate" -> {  {{None}, None}
},
"ResponseForm" -> {  {{None}, None},
  {{None, None}, None}
},
"Restricted" -> {  {{None, None}, None}
},
"Resultant" -> {  {{None, None, _Symbol}, None}
},
"ResumePacket" -> {  {{None}, None}
},
"Return" -> {  {{}, None},
  {{None}, None}
},
"ReturnExpressionPacket" -> {  {{None}, None}
},
"ReturnPacket" -> {  {{None}, None}
},
"ReturnTextPacket" -> {  {{_String}, None}
},
"Reverse" -> {  {{None}, _List},
  {{None, _Integer}, _List}
},
"ReverseApplied" -> {  {{None}, None}
},
"ReverseBiorthogonalSplineWavelet" -> {  {{}, None},
  {{_Integer, _Integer}, None}
},
"ReverseElement" -> {},"ReverseEquilibrium" -> {},"ReverseGraph" -> {  {{_Graph}, _Graph},
  {{_Graph, None}, _Graph}
},
"ReverseSort" -> {  {{None}, _List},
  {{None, None}, _List}
},
"ReverseSortBy" -> {  {{None, None}, _List},
  {{None}, None}
},
"ReverseUpEquilibrium" -> {},"RFixedPoints" -> {  {{None, None}, _List}
},
"RiccatiSolve" -> {  {{_List, _List}, _List}
},
"RiceDistribution" -> {  {{None, None}, None}
},
"RidgeFilter" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image}
},
"RiemannR" -> {  {{None}, _?NumericQ}
},
"RiemannSiegelTheta" -> {  {{None}, _?NumericQ}
},
"RiemannSiegelZ" -> {  {{None}, _?NumericQ}
},
"RiemannXi" -> {  {{None}, _?NumericQ}
},
"RightArrow" -> {},"RightArrowBar" -> {},"RightArrowLeftArrow" -> {},"RightComposition" -> {  {{__}, None}
},
"RightCosetRepresentative" -> {  {{None, None}, None}
},
"RightDownTeeVector" -> {},"RightDownVector" -> {},"RightDownVectorBar" -> {},"RightTee" -> {},"RightTeeArrow" -> {},"RightTeeVector" -> {},"RightTriangle" -> {},"RightTriangleBar" -> {},"RightTriangleEqual" -> {},"RightUpDownVector" -> {},"RightUpTeeVector" -> {},"RightUpVector" -> {},
"RightUpVectorBar" -> {},"RightVector" -> {},"RightVectorBar" -> {},"RipleyK" -> {  {{None, None}, _?NumericQ}
},
"RiskAchievementImportance" -> {  {{None, None}, _?NumericQ}
},
"RiskReductionImportance" -> {  {{None, None}, _?NumericQ}
},
"RobustConvexOptimization" -> {  {{None, None, None}, None}
},
"RogersTanimotoDissimilarity" -> {  {{None, None}, _?NumericQ}
},
"RollPitchYawAngles" -> {  {{None}, _List}
},
"RomanNumeral" -> {  {{None}, _String}
},
"Root" -> {  {{None, _Integer}, None},
  {{None, _Integer, _Integer}, None}
},
"RootApproximant" -> {  {{None}, None},
  {{None, _Integer}, None}
},
"RootIntervals" -> {  {{None}, _List}
},
"RootMeanSquare" -> {  {{None}, _?NumericQ}
},
"Roots" -> {  {{None, None}, None}
},
"RootSum" -> {  {{None, None}, None}
},
"RootTree" -> {  {{None}, None},
  {{None, None}, None}
},
"RotationTransform" -> {  {{None}, None},
  {{None, None}, None}
},
"Row" -> {  {{None}, None},
  {{None, None}, None}
},
"RowBox" -> {  {{None}, None}
},
"RowKey" -> {  {{None}, None}
},
"RSolve" -> {  {{None, None, None}, None}
},
"RSolveValue" -> {  {{None, None, None}, None}
},
"RStabilityConditions" -> {  {{None}, None}
},
"RudinShapiro" -> {  {{_Integer}, _Integer}
},
"RudvalisGroupRu" -> {},"Rule" -> {},"RulesTree" -> {  {{None}, None}
},
"Run" -> {  {{_String}, _Integer}
},
"RunProcess" -> {  {{None}, _Association},
  {{None, None}, _Association},
  {{None, None, None}, _Association}
},
"RunScheduledTask" -> {  {{None}, None}
},
"RunThrough" -> {  {{None, None}, None}
},
"RussellRaoDissimilarity" -> {  {{None, None}, _?NumericQ}
},
"SameAs" -> {  {{None}, None}
},
"SampledEntityClass" -> {  {{None, None}, None}
},
"SampledSoundFunction" -> {  {{None, None, None}, None}
},
"SamplerModel" -> {  {{None}, None}
},
"SARIMAProcess" -> {  {{None, None, None}, None}
},
"SARMAProcess" -> {  {{None, None, None}, None}
},
"SASTriangle" -> {  {{None, None, None}, None}
},
"SatelliteData" -> {  {{None}, None},
  {{None, None}, None}
},
"Save" -> {  {{None, None}, None}
},
"SawtoothWave" -> {  {{None}, _?NumericQ}
},
"Scale" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"Scaled" -> {  {{None}, None}
},
"ScalingTransform" -> {  {{None}, None}
},
"ScheduledTask" -> {  {{None, None}, None}
},
"ScheduledTaskInformation" -> {  {{None}, None},
  {{None, None}, None}
},
"ScheduledTaskObject" -> {  {{None, __}, None}
},
"ScheduledTasks" -> {  {{}, _List}
},
"SchrodingerPDEComponent" -> {  {{None, None}, None}
},
"SchurDecomposition" -> {  {{_List}, _List}
},
"ScorerGi" -> {  {{None}, _?NumericQ}
},
"ScorerGiPrime" -> {  {{None}, _?NumericQ}
},
"ScorerHi" -> {  {{None}, _?NumericQ}
},
"ScorerHiPrime" -> {  {{None}, _?NumericQ}
},
"ScrollVideo" -> {  {{None}, None},
  {{None, None}, None}
},
"SearchAdjustment" -> {  {{None, None}, None}
},
"SearchIndexObject" -> {},"SearchIndices" -> {  {{}, _List}
},
"SearchQueryString" -> {  {{_String}, None}
},
"SearchResultObject" -> {},"SecDegrees" -> {  {{None}, _?NumericQ}
},
"SechDistribution" -> {  {{None, None}, None}
},
"SecondOrderConeOptimization" -> {  {{None, None, None}, None}
},
"SecuredAuthenticationKey" -> {  {{None}, None}
},
"SecuredAuthenticationKeys" -> {  {{}, _List}
},
"SecurityCertificate" -> {  {{None}, None}
},
"SelectComponents" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"SelectedCells" -> {  {{}, _List},
  {{None}, _List}
},
"SelectedNotebook" -> {  {{}, None}
},
"SelectFirst" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"SelectionAnimate" -> {  {{None, None}, None}
},
"SelectionCreateCell" -> {  {{None}, None}
},
"SelectionEvaluate" -> {  {{None}, None}
},
"SelectionEvaluateCreateCell" -> {  {{None}, None}
},
"SelectionMove" -> {  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"SemanticImport" -> {  {{None}, None},
  {{None, None}, None}
},
"SemanticImportString" -> {  {{_String}, None},
  {{_String, None}, None}
},
"SemanticInterpretation" -> {  {{_String}, None},
  {{_String, None}, None}
},
"SemanticRanking" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"SemanticSearch" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"SemanticSearchIndex" -> {  {{None}, None},
  {{None, None}, None}
},
"SemanticSearchIndices" -> {  {{}, _List}
},
"SemialgebraicComponentInstances" -> {  {{_List, _List}, _List}
},
"SemidefiniteOptimization" -> {  {{None, None, None}, None}
},
"SendMail" -> {  {{None}, None}
},
"SendMessage" -> {  {{None, None}, None}
},
"SequenceAlignment" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"SequenceAttentionLayer" -> {  {{}, None},
  {{__}, None}
},
"SequenceCases" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"SequenceFold" -> {  {{None, None, None}, _List}
},
"SequenceForm" -> {  {{__}, None}
},
"SequenceIndicesLayer" -> {  {{}, None}
},
"SequenceLastLayer" -> {  {{}, None}
},
"SequenceMostLayer" -> {  {{}, None}
},
"SequencePosition" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"SequencePredict" -> {  {{None}, None},
  {{None, None}, None}
},
"SequenceReplace" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"SequenceRestLayer" -> {  {{}, None}
},
"SequenceReverseLayer" -> {  {{}, None}
},
"SequenceSplit" -> {  {{None, None}, _List}
},
"SequenceType" -> {  {{None}, None},
  {{None, None}, None}
},
"Series" -> {  {{None, _List}, None},
  {{None, _List, __}, None}
},
"ServiceConnect" -> {  {{None}, None},
  {{None, None}, None}
},
"ServiceDeploy" -> {  {{None}, None}
},
"ServiceDeployment" -> {  {{None}, None}
},
"ServiceDisconnect" -> {  {{None}, None}
},
"ServiceExecute" -> {  {{None, _String}, None},
  {{None, _String, _Association}, None}
},
"ServiceObject" -> {},"ServiceObjects" -> {  {{}, _List},
  {{_String}, _List}
},
"ServiceRequest" -> {  {{None, _String}, None},
  {{None, _String, None}, None}
},
"ServiceSubmit" -> {  {{None}, None}
},
"SessionSubmit" -> {  {{None}, None},
  {{None, None}, None}
},
"SessionTime" -> {  {{}, _?NumericQ}
},
"Set" -> {},"SetAccuracy" -> {  {{None, _Integer}, _?NumericQ}
},
"SetAlphaChannel" -> {  {{_Image}, _Image},
  {{_Image, None}, _Image}
},
"SetAttributes" -> {  {{None, None}, None}
},
"SetCookies" -> {  {{_List}, None}
},
"SetDelayed" -> {},"SetEnvironment" -> {  {{_Rule, __}, None}
},
"SetOptions" -> {  {{None, __}, None}
},
"SetPermissions" -> {  {{None, None}, None}
},
"SetPrecision" -> {  {{None, _Integer}, _?NumericQ}
},
"SetProperty" -> {  {{None, _Rule}, None}
},
"SetSelectedNotebook" -> {  {{None}, None}
},
"SetSharedFunction" -> {  {{None, __}, None}
},
"SetSharedVariable" -> {  {{None, __}, None}
},
"SetStreamPosition" -> {  {{None, _Integer}, _Integer}
},
"SetSystemModel" -> {  {{None}, None}
},
"SetSystemOptions" -> {  {{None, __}, None}
},
"Setter" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"SetterBar" -> {  {{None, _List}, None}
},
"Setting" -> {  {{None}, None}
},
"SetUsers" -> {  {{None, None}, None}
},
"Shallow" -> {  {{None}, None},
  {{None, None}, None}
},
"ShannonWavelet" -> {  {{}, None}
},
"ShapiroWilkTest" -> {  {{_List}, _?NumericQ},
  {{_List, None}, _?NumericQ}
},
"Share" -> {  {{}, _Integer},
  {{None}, _Integer}
},
"Sharpen" -> {  {{_Image}, _Image},
  {{_Image, _Real}, _Image}
},
"ShearingTransform" -> {  {{None, None, None}, None}
},
"ShiftedGompertzDistribution" -> {  {{_Real, _Real}, None}
},
"ShiftRegisterSequence" -> {  {{None, _Integer}, _List}
},
"Short" -> {  {{None}, None},
  {{None, _Integer}, None}
},
"ShortDownArrow" -> {},"Shortest" -> {  {{None}, None}
},
"ShortestCurveDistance" -> {  {{_List, _List}, _?NumericQ}
},
"ShortestMatch" -> {  {{None}, None}
},
"ShortestPathFunction" -> {  {{None}, _List}
},
"ShortLeftArrow" -> {},"ShortRightArrow" -> {},"ShortTimeFourier" -> {  {{None}, None},
  {{None, None}, None}
},
"ShortTimeFourierData" -> {  {{None}, None}
},
"ShortUpArrow" -> {},"Show" -> {  {{None}, _Graphics},
  {{None, __}, _Graphics}
},
"SiderealTime" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"SiegelTheta" -> {  {{None, None}, _?NumericQ}
},
"SiegelTukeyTest" -> {  {{None}, None},
  {{None, None}, None}
},
"SierpinskiCurve" -> {  {{None}, _Graphics},
  {{None, None}, _Graphics}
},
"Signature" -> {  {{None}, _Integer}
},
"SignedRankTest" -> {  {{None}, None},
  {{None, None}, None}
},
"SignTest" -> {  {{None}, None},
  {{None, None}, None}
},
"SimpleGraph" -> {  {{None}, _Graph},
  {{None, None}, _Graph}
},
"Simplex" -> {  {{None}, None}
},
"Simplify" -> {  {{None}, None},
  {{None, None}, None}
},
"Sinc" -> {  {{None}, _?NumericQ}
},
"SinDegrees" -> {  {{None}, _?NumericQ}
},
"SinghMaddalaDistribution" -> {  {{None, None, None}, None}
},
"SingularValues" -> {  {{None}, _List},
  {{None, None}, _List}
},
"SinhIntegral" -> {  {{None}, _?NumericQ}
},
"SinIntegral" -> {  {{None}, _?NumericQ}
},
"SixJSymbol" -> {  {{None, None}, _?NumericQ}
},
"Skeleton" -> {  {{None}, _Image},
  {{None, None}, _Image}
},
"SkeletonTransform" -> {  {{None}, _Image}
},
"SkellamDistribution" -> {  {{None, None}, None}
},
"Skewness" -> {  {{None}, _?NumericQ}
},
"SkewNormalDistribution" -> {  {{None}, None},
  {{None, None, None}, None}
},
"Skip" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"SliceDistribution" -> {  {{None, None}, None}
},
"Slider" -> {  {{None}, None},
  {{None, None}, None}
},
"Slider2D" -> {  {{None}, None},
  {{None, None}, None}
},
"SliderBox" -> {  {{None}, None},
  {{None, None}, None}
},
"SlideShowVideo" -> {  {{None}, None},
  {{None, None}, None}
},
"SlideView" -> {  {{None}, None},
  {{None, None}, None}
},
"Slot" -> {},"SlotSequence" -> {},"SmallCircle" -> {},"SmithDecomposition" -> {  {{None}, _List}
},
"SmithDelayCompensator" -> {  {{None, None}, None}
},
"SmithReduce" -> {  {{None, None}, None}
},
"SmithWatermanSimilarity" -> {  {{None, None}, _?NumericQ},
  {{None, None, None}, _?NumericQ}
},
"SmoothDateHistogram" -> {  {{None}, _Graphics},
  {{None, None}, _Graphics}
},
"SmoothDensityHistogram" -> {  {{None}, _Graphics},
  {{None, None}, _Graphics}
},
"SmoothHistogram" -> {  {{None}, _Graphics},
  {{None, None}, _Graphics}
},
"SmoothHistogram3D" -> {  {{None}, _Graphics3D},
  {{None, None}, _Graphics3D}
},
"SmoothKernelDistribution" -> {  {{None}, None},
  {{None, None}, None}
},
"SmoothPointDensity" -> {  {{None}, _Graphics},
  {{None, None}, _Graphics}
},
"SnDispersion" -> {  {{None}, _?NumericQ}
},
"Snippet" -> {  {{None}, None},
  {{None, None}, None}
},

"SnippetsVideo" -> {  {{None, __}, None}
},
"SnubPolyhedron" -> {  {{None}, None}
},
"SocialMediaData" -> {  {{None, __}, None}
},
"SocketConnect" -> {  {{None, __}, None}
},
"SocketListen" -> {  {{None, None}, None}
},
"SocketListener" -> {},"SocketObject" -> {},"SocketOpen" -> {  {{None, __}, None}
},
"SocketReadMessage" -> {  {{None}, None}
},
"Sockets" -> {  {{}, _List}
},
"SocketWaitAll" -> {  {{None, __}, _List}
},
"SocketWaitNext" -> {  {{None, __}, None}
},
"SocketWriteMessage" -> {  {{None, None}, None}
},
"SoftmaxLayer" -> {  {{}, None},
  {{None}, None}
},
"SokalSneathDissimilarity" -> {  {{None, None}, _Real}
},
"SolarEclipse" -> {  {{None, __}, None}
},
"SolarSystemFeatureData" -> {  {{None, __}, None}
},
"SolarTime" -> {  {{None, __}, None}
},
"SolidAngle" -> {  {{None, None}, _?NumericQ}
},
"SolidBoundaryLoadValue" -> {  {{None, __}, None}
},
"SolidData" -> {  {{None, __}, None}
},
"SolidDisplacementCondition" -> {  {{None, __}, None}
},
"SolidFixedCondition" -> {  {{None, __}, None}
},
"SolidMechanicsPDEComponent" -> {  {{None, None}, None}
},
"SolidMechanicsStrain" -> {  {{None, None}, None}
},
"SolidMechanicsStress" -> {  {{None, None}, None}
},
"SolveAlways" -> {  {{None, None}, _List}
},
"SolveValues" -> {  {{None, None}, _List}
},
"SortedEntityClass" -> {  {{None, None}, None}
},
"Sound" -> {  {{None}, None},
  {{None, None}, None}
},
"SoundNote" -> {  {{None, __}, None}
},
"SourcePDETerm" -> {  {{None, None}, None}
},
"Sow" -> {  {{None}, None},
  {{None, None}, None}
},
"SowVideo" -> {  {{None, __}, None}
},
"SpaceCurveData" -> {  {{None, __}, None}
},
"SpaceForm" -> {  {{None}, None}
},
"Spacer" -> {  {{None}, None}
},
"SpatialBinnedPointData" -> {  {{None, __}, None}
},
"SpatialEstimate" -> {  {{None, __}, None}
},
"SpatialEstimatorFunction" -> {},"SpatialGraphDistribution" -> {  {{None, None}, None}
},
"SpatialJ" -> {  {{None}, None}
},
"SpatialMedian" -> {  {{None}, _List}
},
"SpatialPointData" -> {  {{None}, None},
  {{None, None}, None}
},
"SpatialPointSelect" -> {  {{None, None}, None}
},
"SpatialRandomnessTest" -> {  {{None, __}, None}
},
"SpatialTransformationLayer" -> {  {{}, None},
  {{None}, None}
},
"Speak" -> {  {{None}, None}
},
"SpearmanRankTest" -> {  {{None, __}, None}
},
"SpearmanRho" -> {  {{None, None}, _Real}
},
"SpeciesData" -> {  {{}, _List},
  {{None}, _Association},
  {{None, _String}, None}
},
"SpectralLineData" -> {  {{}, _List},
  {{None}, _Association},
  {{None, _String}, None}
},
"Spectrogram" -> {  {{None}, _Graphics}
},
"SpectrogramArray" -> {  {{None}, _List}
},
"Specularity" -> {  {{None}, None},
  {{None, None}, None}
},
"SpeechCases" -> {  {{None, None}, _List}
},
"SpeechInterpreter" -> {  {{None}, None}
},
"SpeechRecognize" -> {  {{None}, _String}
},
"SpeechSynthesize" -> {  {{None}, None}
},
"Sphere" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None}
},
"SpherePoints" -> {  {{_Integer}, _List},
  {{_Integer, None}, _List}
},
"SphericalAngle" -> {  {{None, None, None}, _?NumericQ}
},
"SphericalBesselJ" -> {  {{None, None}, _?NumericQ}
},
"SphericalBesselY" -> {  {{None, None}, _?NumericQ}
},
"SphericalDistance" -> {  {{None, None}, _?NumericQ}
},
"SphericalHankelH1" -> {  {{None, None}, _?NumericQ}
},
"SphericalHankelH2" -> {  {{None, None}, _?NumericQ}
},
"SphericalHarmonicY" -> {  {{None, None, None, None}, _?NumericQ}
},
"SphericalShell" -> {  {{None, None}, None}
},
"SpheroidalEigenvalue" -> {  {{None, None, None}, _?NumericQ}
},
"SpheroidalJoiningFactor" -> {  {{None, None, None}, _?NumericQ}
},
"SpheroidalPS" -> {  {{None, None, None, None}, _?NumericQ}
},
"SpheroidalPSPrime" -> {  {{None, None, None, None}, _?NumericQ}
},
"SpheroidalQS" -> {  {{None, None, None, None}, _?NumericQ}
},
"SpheroidalQSPrime" -> {  {{None, None, None, None}, _?NumericQ}
},
"SpheroidalRadialFactor" -> {  {{None, None, None}, _?NumericQ}
},
"SpheroidalS1" -> {  {{None, None, None, None}, _?NumericQ}
},
"SpheroidalS1Prime" -> {  {{None, None, None, None}, _?NumericQ}
},
"SpheroidalS2" -> {  {{None, None, None, None}, _?NumericQ}
},
"SpheroidalS2Prime" -> {  {{None, None, None, None}, _?NumericQ}
},
"Splice" -> {  {{_List}, None}
},
"SplicedDistribution" -> {  {{_List, _List, _List}, None}
},
"Split" -> {  {{_List}, _List},
  {{_List, None}, _List}
},
"SpokenString" -> {  {{None}, _String}
},
"SpotLight" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"SqrtBox" -> {  {{None}, None}
},
"Square" -> {},"SquaredEuclideanDistance" -> {  {{_List, _List}, _?NumericQ}
},
"SquareIntersection" -> {},"SquareRepeatingElement" -> {  {{None}, None}
},
"SquaresR" -> {  {{_Integer, _Integer}, _Integer}
},
"SquareSubset" -> {},"SquareSubsetEqual" -> {},"SquareSuperset" -> {},"SquareSupersetEqual" -> {},"SquareUnion" -> {},"SquareWave" -> {  {{None}, _?NumericQ},
  {{_List, None}, _?NumericQ}
},
"Squiggled" -> {  {{None}, None}
},
"SSSTriangle" -> {  {{None, None, None}, None}
},
"StableDistribution" -> {  {{None, None, None, None}, None}
},
"Stack" -> {  {{}, _List},
  {{None}, _List}
},
"StackBegin" -> {  {{None}, None}
},
"StackComplete" -> {  {{None}, None}
},
"StackInhibit" -> {  {{None}, None}
},
"StadiumShape" -> {  {{_List, _Real}, None}
},
"StandardAtmosphereData" -> {  {{None}, None},
  {{None, _String}, None}
},
"StandardDeviationFilter" -> {  {{_List, _Integer}, _List},
  {{_Image, _Integer}, _Image}
},
"StandardForm" -> {  {{None}, None}
},
"Standardize" -> {  {{_List}, _List},
  {{_List, None, None}, _List}
},
"StandardOceanData" -> {  {{None}, None},
  {{None, _String}, None}
},
"StandbyDistribution" -> {  {{None, None}, None}
},
"Star" -> {},"StarClusterData" -> {  {{None, _String}, None}
},
"StarData" -> {  {{None, _String}, None}
},
"StarGraph" -> {  {{_Integer}, _Graph}
},
"StartAsynchronousTask" -> {  {{None}, None}
},
"StartExternalSession" -> {  {{_String}, None},
  {{None}, None}
},
"StartProcess" -> {  {{_String}, None},
  {{_List}, None}
},
"StartScheduledTask" -> {  {{None}, None}
},
"StartWebSession" -> {  {{}, None},
  {{None}, None}
},
"StateFeedbackGains" -> {  {{None, _List}, _List}
},
"StateOutputEstimator" -> {  {{None, None}, None}
},
"StateResponse" -> {  {{None, None}, None}
},
"StateSpaceModel" -> {  {{_List}, None},
  {{None}, None}
},
"StateSpaceTransform" -> {  {{None, None}, None}
},
"StateTransformationLinearize" -> {  {{None, __}, None}
},
"StationaryDistribution" -> {  {{None}, None}
},
"StationaryWaveletPacketTransform" -> {  {{_List}, None},
  {{_List, None}, None}
},
"StationaryWaveletTransform" -> {  {{_List}, None},
  {{_List, None}, None}
},
"StatusArea" -> {  {{None, None}, None}
},
"StatusCentrality" -> {  {{_Graph}, _List}
},
"StieltjesGamma" -> {  {{_Integer}, _?NumericQ},
  {{_Integer, None}, _?NumericQ}
},
"StippleShading" -> {  {{}, None},
  {{None}, None}
},
"StopAsynchronousTask" -> {  {{None}, None}
},
"StoppingPowerData" -> {  {{None, None, None}, None}
},
"StopScheduledTask" -> {  {{None}, None}
},
"StratonovichProcess" -> {  {{__}, None}
},
"StraussHardcorePointProcess" -> {  {{None, None, None, None, _Integer}, None}
},
"StraussPointProcess" -> {  {{None, None, None, _Integer}, None}
},
"StreamPosition" -> {  {{None}, _Integer}
},
"StripBoxes" -> {  {{None}, None}
},
"StructuralImportance" -> {  {{None}, None}
},
"StructuredArray" -> {  {{None, _List, None}, None}
},
"StruveH" -> {  {{None, None}, _?NumericQ}
},
"StruveL" -> {  {{None, None}, _?NumericQ}
},
"StudentTDistribution" -> {  {{None}, None},
  {{None, None, None}, None}
},
"Style" -> {  {{None, __}, None}
},
"StyleBox" -> {  {{None, __}, None}
},
"StyleData" -> {  {{_String}, None},
  {{_String, _String}, None}
},
"StyleForm" -> {  {{None, __}, None}
},
"StylePrint" -> {  {{None, __}, None}
},
"Subfactorial" -> {  {{_Integer}, _Integer}
},
"Subgraph" -> {  {{_Graph, _List, __}, _Graph}
},
"SubMinus" -> {},"SubPlus" -> {},"Subresultants" -> {  {{None, None, _Symbol}, _List}
},
"Subscript" -> {  {{None, __}, None}
},
"SubscriptBox" -> {  {{None, __}, None}
},
"Subsequences" -> {  {{_List}, _List},
  {{_List, _List}, _List}
},
"Subset" -> {},"SubsetCases" -> {  {{_List, None, __}, _List}
},
"SubsetEqual" -> {},"SubsetMap" -> {  {{None, _List, __}, _List}
},
"SubsetPosition" -> {  {{_List, _List, __}, _List}
},
"SubsetReplace" -> {  {{_List, None, __}, _List}
},
"SubStar" -> {},"SubstitutionSystem" -> {  {{None, None}, _List},
  {{None, None, _Integer}, _List}
},
"Subsuperscript" -> {  {{None, None, None}, None}
},
"SubsuperscriptBox" -> {  {{None, None, None}, None}
},
"SubtractFrom" -> {  {{None, None}, None}
},
"SubValues" -> {  {{_Symbol}, _List}
},
"Succeeds" -> {},"SucceedsEqual" -> {},"SucceedsSlantEqual" -> {},"SucceedsTilde" -> {},"Success" -> {  {{_String, _Association}, None}
},
"SuchThat" -> {},"SumConvergence" -> {  {{None, _Symbol}, _?BooleanQ},
  {{None, _List}, _?BooleanQ}
},
"SummationLayer" -> {  {{}, None},
  {{_Integer}, None}
},
"SunPosition" -> {  {{}, None},
  {{None, __}, None}
},
"Sunrise" -> {  {{None}, None},
  {{None, None}, None}
},
"Sunset" -> {  {{None}, None},
  {{None, None}, None}
},
"SuperDagger" -> {},"SuperMinus" -> {},"SupernovaData" -> {  {{None, __}, None}
},
"SuperPlus" -> {},"Superscript" -> {  {{None, __}, None}
},
"SuperscriptBox" -> {  {{None, __}, None}
},
"Superset" -> {},"SupersetEqual" -> {},"SuperStar" -> {},"Surd" -> {  {{None, _Integer}, _?NumericQ}
},
"SurfaceArea" -> {  {{None}, _?NumericQ},
  {{None, _List, _List}, _?NumericQ}
},
"SurfaceData" -> {  {{None, __}, None}
},
"SurfaceIntegrate" -> {  {{None, None}, _?NumericQ}
},
"SurvivalDistribution" -> {  {{None}, None}
},
"SurvivalFunction" -> {  {{None, None}, _?NumericQ}
},
"SurvivalModel" -> {  {{None, __}, None}
},
"SurvivalModelFit" -> {  {{None, __}, None}
},
"SuspendPacket" -> {  {{}, None}
},

"SuzukiDistribution" -> {  {{None, None}, None}
},
"SuzukiGroupSuz" -> {  {{}, None}
},
"SwatchLegend" -> {  {{_List, _List}, None}
},
"Symbol" -> {  {{_String}, None}
},
"SymbolicDeltaProductArray" -> {  {{_List}, None}
},
"SymbolicIdentityArray" -> {  {{None}, None}
},
"SymbolicOnesArray" -> {  {{_List}, None}
},
"SymbolicZerosArray" -> {  {{_List}, None}
},
"SymletWavelet" -> {  {{}, None},
  {{_Integer}, None}
},
"Symmetric" -> {  {{_List}, None}
},
"SymmetricDifference" -> {  {{_List, __}, _List}
},
"SymmetricGroup" -> {  {{_Integer}, None}
},
"SymmetricKey" -> {  {{None}, None}
},
"SymmetricReduction" -> {  {{None, _List}, _List},
  {{None, _List, _List}, _List}
},
"Symmetrize" -> {  {{_List, None}, _List}
},
"SymmetrizedArray" -> {  {{None, _List, None}, None}
},
"SymmetrizedArrayRules" -> {  {{None}, _List}
},
"SymmetrizedDependentComponents" -> {  {{None}, _List}
},
"SymmetrizedIndependentComponents" -> {  {{None}, _List}
},
"SymmetrizedReplacePart" -> {  {{None, None, _List}, None}
},
"Synonyms" -> {  {{_String}, _List}
},
"SyntaxInformation" -> {  {{_Symbol}, _Association}
},
"SyntaxLength" -> {  {{_String}, _Integer}
},
"SyntaxPacket" -> {  {{_String}, None}
},
"SynthesizeMissingValues" -> {  {{None}, None}
},
"SystemCredential" -> {  {{_String}, None}
},
"SystemCredentialData" -> {  {{__}, None}
},
"SystemCredentialKeys" -> {  {{__}, _List}
},
"SystemCredentialStoreObject" -> {  {{__}, None}
},
"SystemDialogInput" -> {  {{_String}, None}
},
"SystemInformation" -> {  {{}, _Association},
  {{_String}, None}
},
"SystemInstall" -> {  {{__}, None}
},
"SystemModel" -> {  {{_String}, None}
},
"SystemModelAlways" -> {  {{None}, None}
},
"SystemModelCalibrate" -> {  {{None, None, _List}, None}
},
"SystemModelDelay" -> {  {{__}, None}
},
"SystemModeler" -> {  {{__}, None}
},
"SystemModelEventually" -> {  {{None}, None}
},
"SystemModelExamples" -> {  {{__}, None}
},
"SystemModelLinearize" -> {  {{None, __}, None}
},
"SystemModelMeasurements" -> {  {{None}, None}
},
"SystemModelParametricSimulate" -> {  {{None, _List, __}, None}
},
"SystemModelReliability" -> {  {{None, __}, None}
},
"SystemModels" -> {  {{}, _List},
  {{_String}, _List}
},
"SystemModelSimulate" -> {  {{None, None}, None}
},
"SystemModelSimulateSensitivity" -> {  {{None, _List, None}, None}
},
"SystemModelSimulationData" -> {  {{__}, None}
},
"SystemModelSurrogate" -> {  {{None, __}, None}
},
"SystemModelSurrogateTrain" -> {  {{None, __}, None}
},
"SystemModelSustain" -> {  {{None}, None}
},
"SystemModelUntil" -> {  {{None, None}, None}
},
"SystemModelValidate" -> {  {{None, _String}, None}
},
"SystemModelValidationData" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"SystemOpen" -> {  {{_String}, None},
  {{None}, None}
},
"SystemOptions" -> {  {{_String}, None},
  {{}, None}
},
"SystemProcessData" -> {  {{}, None},
  {{_String}, None},
  {{_Rule}, None}
},
"SystemProcesses" -> {  {{}, _List},
  {{None}, _List},
  {{_String}, _List},
  {{_Rule}, _List}
},
"SystemsConnectionsModel" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"SystemsModelControllerData" -> {  {{}, None},
  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"SystemsModelDelay" -> {  {{None}, None}
},
"SystemsModelDelayApproximate" -> {  {{None, None}, None}
},
"SystemsModelDelete" -> {  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"SystemsModelDimensions" -> {  {{None}, _Integer}
},
"SystemsModelExtract" -> {  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"SystemsModelFeedbackConnect" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"SystemsModelLinearity" -> {  {{None}, None}
},
"SystemsModelMerge" -> {  {{None}, None}
},
"SystemsModelOrder" -> {  {{None}, None}
},
"SystemsModelParallelConnect" -> {  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"SystemsModelSeriesConnect" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"SystemsModelStateFeedbackConnect" -> {  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"SystemsModelVectorRelativeOrders" -> {  {{None}, _List}
},
"TableView" -> {  {{_List, _String}, None},
  {{}, None}
},
"Tabular" -> {  {{_List}, None},
  {{_List, None}, None}
},
"TabularColumn" -> {  {{None}, _List},
  {{None, None}, _List},
  {{None, None, _String}, _List}
},
"TabularRow" -> {  {{_List}, None},
  {{_List, None}, None}
},
"TabularSchema" -> {  {{None, _String}, None}
},
"TabularStructure" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"TabularSummary" -> {  {{None}, None},
  {{None, None}, None}
},
"TabView" -> {  {{_Association, __}, None}
},
"TagBox" -> {  {{None, _String}, None}
},
"TaggedNestGraph" -> {  {{None, None, _Integer}, _Graph},
  {{None, _List, _Integer}, _Graph}
},
"TagSet" -> {  {{None, None, None}, None}
},
"TagSetDelayed" -> {  {{None, None, None}, None}
},
"TagUnset" -> {  {{None, None}, None}
},
"Take" -> {  {{_List, _Integer}, None},
  {{_List, None}, None},
  {{_Association, _Integer}, _Association}
},
"TakeDrop" -> {  {{_List, _Integer}, None},
  {{_List, _List}, None}
},
"TakeWhile" -> {  {{_List, None}, None}
},
"TanDegrees" -> {  {{None}, _?NumericQ}
},
"TaskAbort" -> {  {{None}, None}
},
"TaskExecute" -> {  {{None}, None}
},
"TaskObject" -> {  {{None}, None}
},
"TaskRemove" -> {  {{None}, None}
},
"TaskResume" -> {  {{None}, None}
},
"Tasks" -> {  {{}, _List},
  {{None}, _List}
},
"TaskSuspend" -> {  {{None}, None}
},
"TaskWait" -> {  {{None}, None}
},
"TelegraphProcess" -> {  {{None, None}, None}
},
"TemplateBox" -> {  {{None, _String}, None},
  {{None, None, None, _String}, None}
},
"TemplateExpression" -> {  {{_String}, None}
},
"TemplateIf" -> {  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"TemplateObject" -> {  {{None}, None},
  {{None, _List}, None}
},
"TemplateSequence" -> {  {{None}, None},
  {{None, None}, None}
},
"TemplateSlot" -> {  {{_Integer}, None},
  {{_String}, None}
},
"TemplateWith" -> {  {{None, None}, None}
},
"TemporalData" -> {  {{None, None}, None}
},
"TensorContract" -> {  {{None, _List}, None}
},
"TensorDimensions" -> {  {{None}, _List}
},
"TensorExpand" -> {  {{None}, None}
},
"TensorProduct" -> {  {{__}, None}
},
"TensorRank" -> {  {{None}, _Integer}
},
"TensorReduce" -> {  {{None}, None}
},
"TensorSymmetry" -> {  {{None}, None},
  {{None, _List}, None}
},
"TensorTranspose" -> {  {{None}, None},
  {{None, _List}, None}
},
"TensorWedge" -> {  {{__}, None}
},
"TerminatedEvaluation" -> {  {{None}, None}
},
"TestCreate" -> {  {{None, None}, None},
  {{None, None, __}, None}
},
"TestEvaluate" -> {  {{None}, None}
},
"TestObject" -> {  {{None, __}, None}
},
"TestReport" -> {  {{None}, None},
  {{_List}, None}
},
"TestReportObject" -> {  {{None, __}, None}
},
"TestResultObject" -> {  {{None, __}, None}
},
"Tetrahedron" -> {  {{}, None},
  {{_List}, None}
},
"TeXSave" -> {  {{_String, None}, None}
},
"Text" -> {  {{None}, None},
  {{None, None}, None},
  {{None, None, None}, None}
},
"TextCases" -> {  {{_String, None}, _List}
},
"TextCell" -> {  {{None}, None},
  {{None, __}, None}
},
"TextContents" -> {  {{_String}, None}
},
"TextData" -> {  {{None}, None}
},
"TextElement" -> {  {{None}, None},
  {{None, _Association}, None}
},
"TextGrid" -> {  {{_List}, None},
  {{_List, __}, None}
},
"TextPacket" -> {  {{_String}, None}
},
"TextPosition" -> {  {{_String, None}, _List}
},
"TextRecognize" -> {  {{_Image}, _String},
  {{_Image, None}, None}
},
"TextSearch" -> {  {{None, None}, None},
  {{None, None, __}, None}
},
"TextSearchReport" -> {  {{None, __}, None}
},
"TextSentences" -> {  {{_String}, _List},
  {{_String, None}, _List}
},
"TextStructure" -> {  {{_String}, None},
  {{_String, None}, None}
},
"TextSummarize" -> {  {{_String}, _String},
  {{_String, None}, _String}
},
"TextTranslation" -> {  {{_String, None}, _String}
},
"Texture" -> {  {{None}, None}
},
"TextWords" -> {  {{_String}, _List},
  {{_String, None}, _List}
},
"Therefore" -> {},"ThermodynamicData" -> {  {{_String, _String}, None},
  {{_String, _String, None}, None}
},
"ThermometerGauge" -> {  {{None}, _Graphics},
  {{None, None}, _Graphics}
},
"Thickness" -> {  {{None}, None}
},
"Thinning" -> {  {{_Image}, _Image}
},
"ThomasPointProcess" -> {  {{None, None, None, _Integer}, None}
},
"ThompsonGroupTh" -> {},"Threaded" -> {  {{None}, None}
},
"ThreadingLayer" -> {  {{None}, None}
},
"ThreeJSymbol" -> {  {{_List, _List, _List}, _?NumericQ}
},
"Threshold" -> {  {{None}, None},
  {{None, None}, None}
},
"Through" -> {  {{None}, None},
  {{None, _Symbol}, None}
},
"ThrowException" -> {  {{None}, None}
},
"ThueMorse" -> {  {{_Integer}, _Integer}
},
"Thumbnail" -> {  {{None}, None},
  {{None, None}, None}
},
"TideData" -> {  {{__}, None}
},
"Tilde" -> {},"TildeEqual" -> {},"TildeFullEqual" -> {},"TildeTilde" -> {},"Timing" -> {  {{None}, _List}
},
"TitsGroupT" -> {},"ToBoxes" -> {  {{None}, None},
  {{None, None}, None}
},
"ToContinuousTimeModel" -> {  {{None}, None},
  {{None, None}, None}
},
"ToDate" -> {  {{None}, _List}
},
"ToDiscreteTimeModel" -> {  {{None, None}, None}
},
"ToEntity" -> {  {{None}, None},
  {{None, _String}, None}
},
"ToFiniteField" -> {  {{None, None}, None}
},
"Toggler" -> {  {{None, _List}, None}
},
"TogglerBar" -> {  {{None, _List}, None}
},
"TogglerBox" -> {  {{None, _List}, None}
},
"ToInvertibleTimeSeries" -> {  {{None}, None}
},
"ToMemory" -> {  {{None}, None}
},
"Tooltip" -> {  {{None}, None},
  {{None, None}, None}
},
"ToonShading" -> {  {{__}, None}
},
"TopHatTransform" -> {  {{_Image, None}, _Image}
},
"ToPolarCoordinates" -> {  {{_List}, _List}
},
"TopologicalSort" -> {  {{_Graph}, _List}
},
"ToRadicals" -> {  {{None}, None}
},
"ToRawPointer" -> {  {{None}, None}
},
"Torus" -> {  {{__}, None}
},
"TorusGraph" -> {  {{_List}, _Graph},
  {{_Integer, _Integer}, _Graph}
},
"ToSphericalCoordinates" -> {  {{_List}, _List}
},
"ToTabular" -> {  {{None}, None}
},
"TotalLayer" -> {  {{__}, None}
},
"TotalVariationFilter" -> {  {{_Image, _Real}, _Image}
},
"TouchPosition" -> {},"Tour3DVideo" -> {  {{None}, None}
},
"TourVideo" -> {  {{None}, None}
},
"TraceDialog" -> {  {{None}, None}
},
"TraceLevel" -> {},"TracePrint" -> {  {{None}, None},
  {{None, None}, None}
},
"TraceScan" -> {  {{None, None}, None},
  {{None, None, None}, None},
  {{None, None, None, None}, None}
},
"TracyWidomDistribution" -> {  {{None}, None}
},
"TraditionalForm" -> {  {{None}, None}
},
"TrainImageContentDetector" -> {  {{None}, None}
},
"TrainTextContentDetector" -> {  {{None}, None}
},
"TransferFunctionCancel" -> {  {{None}, None}
},

"TransferFunctionExpand" -> {  {{None}, None}
},
"TransferFunctionFactor" -> {  {{None}, None}
},
"TransferFunctionModel" -> {  {{None, None}, None}
},
"TransferFunctionPoles" -> {  {{None}, _List}
},
"TransferFunctionTransform" -> {  {{None, None}, None}
},
"TransferFunctionZeros" -> {  {{None}, _List}
},
"TransformAnomalies" -> {  {{None, None}, None}
},
"TransformationFunction" -> {  {{None}, None}
},
"TransformColumns" -> {  {{None, None}, None}
},
"TransformedDistribution" -> {  {{None, None}, None}
},
"TransformedProcess" -> {  {{None, None}, None}
},
"TransformMissing" -> {  {{None, None}, None}
},
"TransitiveClosureGraph" -> {  {{_Graph}, _Graph}
},
"TransitiveReductionGraph" -> {  {{_Graph}, _Graph}
},
"Translate" -> {  {{None, None}, None}
},
"TranslationTransform" -> {  {{None}, None}
},
"Transliterate" -> {  {{_String}, _String},
  {{_String, _String}, _String}
},
"TransposeLayer" -> {  {{}, None},
  {{None}, None}
},
"TravelDirections" -> {  {{_List}, None}
},
"TravelDirectionsData" -> {  {{_List}, None}
},
"TravelDistance" -> {  {{_List}, None}
},
"TravelTime" -> {  {{_List}, None}
},
"Tree" -> {  {{None}, None},
  {{None, _List}, None}
},
"TreeCases" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"TreeChildren" -> {  {{None}, _List}
},
"TreeData" -> {  {{None}, None}
},
"TreeDelete" -> {  {{None, None}, None}
},
"TreeDepth" -> {  {{None}, _Integer}
},
"TreeExpression" -> {  {{None}, None},
  {{None, None}, None}
},
"TreeExtract" -> {  {{None, None}, None}
},
"TreeFold" -> {  {{None, None}, None}
},
"TreeForm" -> {  {{None}, None}
},
"TreeGame" -> {  {{None, None}, None}
},
"TreeGamePayoff" -> {  {{None}, None}
},
"TreeGraph" -> {  {{None}, _Graph},
  {{None, None}, _Graph}
},
"TreeInsert" -> {  {{None, None, None}, None}
},
"TreeLeaves" -> {  {{None}, _List}
},
"TreeLevel" -> {  {{None, None}, _List}
},
"TreeMap" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"TreeMapAt" -> {  {{None, None, None}, None}
},
"TreeOutline" -> {  {{None}, None}
},
"TreePosition" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"TreeReplacePart" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"TreeRules" -> {  {{None}, _List}
},
"TreeScan" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"TreeSelect" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"TreeSize" -> {  {{None}, _Integer}
},
"Triangle" -> {  {{_List}, None}
},
"TriangleCenter" -> {  {{None}, None},
  {{None, _String}, None}
},
"TriangleConstruct" -> {  {{None, _String}, None}
},
"TriangleMeasurement" -> {  {{None, _String}, _?NumericQ}
},
"TriangleWave" -> {  {{None}, _?NumericQ}
},
"TriangularDistribution" -> {  {{_List}, None},
  {{_List, _Real}, None}
},
"TrigExpand" -> {  {{None}, None}
},
"TrigFactor" -> {  {{None}, None}
},
"Trigger" -> {  {{None}, None}
},
"TrigReduce" -> {  {{None}, None}
},
"TrigToExp" -> {  {{None}, None}
},
"TrimmedMean" -> {  {{_List, _Real}, _?NumericQ},
  {{_List, _List}, _?NumericQ}
},
"TrimmedVariance" -> {  {{_List, _Real}, _?NumericQ},
  {{_List, _List}, _?NumericQ}
},
"TropicalStormData" -> {  {{None, None}, None}
},
"TruncatedPolyhedron" -> {  {{None}, None},
  {{None, _Real}, None}
},
"TruncateSum" -> {  {{None, None}, None}
},
"TsallisQExponentialDistribution" -> {  {{_Real, _Real}, None}
},
"TsallisQGaussianDistribution" -> {  {{_Real, _Real}, None}
},
"TTest" -> {  {{_List}, None},
  {{_List, None}, None},
  {{_List, None, _String}, None}
},
"Tube" -> {  {{_List}, None},
  {{_List, _Real}, None}
},
"TukeyLambdaDistribution" -> {  {{_Real}, None}
},
"TukeyWindow" -> {  {{_Real}, _?NumericQ},
  {{_Real, _Real}, _?NumericQ}
},
"TunnelData" -> {  {{None, None}, None}
},
"TuranGraph" -> {  {{_Integer, _Integer}, _Graph}
},
"TuringMachine" -> {  {{None, _List, _Integer}, _List}
},
"TwoWayRule" -> {  {{None, None}, None}
},
"Typed" -> {  {{None, None}, None}
},
"TypeDeclaration" -> {  {{None, None}, None}
},
"TypeEvaluate" -> {  {{None}, None}
},
"TypeHint" -> {  {{None, None}, None}
},
"TypeOf" -> {  {{None}, None}
},
"TypeSpecifier" -> {  {{None}, None}
},
"UnderBar" -> {  {{None}, None}
},
"Underoverscript" -> {  {{None, None, None}, None}
},
"UnderoverscriptBox" -> {  {{None, None, None}, None}
},
"Underscript" -> {  {{None, None}, None}
},
"UnderscriptBox" -> {  {{None, None}, None}
},
"UnderseaFeatureData" -> {  {{None, None}, None}
},
"UndirectedEdge" -> {  {{None, None}, None}
},
"UndirectedGraph" -> {  {{None}, _Graph}
},
"UnequalTo" -> {  {{None}, None}
},
"Unevaluated" -> {  {{None}, None}
},
"UniformDistribution" -> {  {{}, None},
  {{_List}, None}
},
"UniformGraphDistribution" -> {  {{_Integer, _Integer}, None}
},
"UniformPolyhedron" -> {  {{_String}, None},
  {{_List}, None}
},
"UniformSumDistribution" -> {  {{_Integer}, None},
  {{_Integer, _List}, None}
},
"UnilateralConvolve" -> {  {{None, None, None, None}, None}
},
"UnilateralDiscreteConvolve" -> {  {{None, None, None, None}, None}
},
"Uninstall" -> {  {{None}, None}
},
"UnionedEntityClass" -> {  {{__}, None}
},
"UnionPlus" -> {},"Unique" -> {  {{}, _Symbol},
  {{_String}, _Symbol},
  {{_Symbol}, _Symbol}
},
"UniqueElements" -> {  {{_List}, _List}
},
"UnitBox" -> {  {{None}, _?NumericQ}
},
"UnitConvert" -> {  {{None}, None},
  {{None, None}, None}
},
"UnitDimensions" -> {  {{None}, _List}
},
"Unitize" -> {  {{None}, _?NumericQ}
},
"UnitRootTest" -> {  {{None}, _?NumericQ},
  {{None, _String}, None}
},
"UnitSimplify" -> {  {{None}, None}
},
"UnitStep" -> {  {{None, __}, _?NumericQ}
},
"UnitTriangle" -> {  {{None}, _?NumericQ}
},
"UnitVector" -> {  {{None, None}, _List}
},
"UnitVectorLayer" -> {  {{None}, None}
},
"UniverseModelData" -> {  {{__}, None}
},
"UniversityData" -> {  {{None}, None},
  {{None, None}, None}
},
"UnixTime" -> {  {{__}, _Integer}
},
"UnlabeledTree" -> {  {{None}, _Graph}
},
"UnmanageObject" -> {  {{None}, None}
},
"Unprotect" -> {  {{__}, _List}
},
"UnregisterExternalEvaluator" -> {  {{None, None}, None}
},
"Unset" -> {},"UnsetShared" -> {  {{None}, None}
},
"Until" -> {  {{None, None}, None}
},
"UpArrow" -> {},"UpArrowBar" -> {},"UpArrowDownArrow" -> {},"Update" -> {  {{None}, None}
},
"UpdateSearchIndex" -> {  {{None}, None},
  {{None, None}, None}
},
"UpdateSemanticSearchIndex" -> {  {{None, None}, None}
},
"UpDownArrow" -> {},"UpEquilibrium" -> {},"UpperLeftArrow" -> {},"UpperRightArrow" -> {},"Upsample" -> {  {{None, None}, _List},
  {{None, None, None}, _List}
},
"UpSet" -> {},"UpSetDelayed" -> {},"UpTee" -> {},"UpTeeArrow" -> {},"UpTo" -> {  {{_Integer}, None}
},
"UpValues" -> {  {{_Symbol}, _List}
},
"URL" -> {  {{_String}, None}
},
"URLBuild" -> {  {{None}, _String},
  {{None, None}, _String}
},
"URLDecode" -> {  {{_String}, _String}
},
"URLDispatcher" -> {  {{None}, None}
},
"URLDownload" -> {  {{None}, None},
  {{None, None}, None}
},
"URLDownloadSubmit" -> {  {{None, None}, None},
  {{None, None, None}, None}
},
"URLEncode" -> {  {{_String}, _String}
},
"URLExecute" -> {  {{None}, None},
  {{None, None}, None}
},
"URLExpand" -> {  {{_String}, _String}
},
"URLFetch" -> {  {{__}, None}
},
"URLFetchAsynchronous" -> {  {{None, None}, None}
},
"URLParse" -> {  {{_String}, _Association}
},
"URLQueryDecode" -> {  {{_String}, _Association}
},
"URLQueryEncode" -> {  {{None}, _String}
},
"URLRead" -> {  {{None}, None},
  {{None, None}, None}
},
"URLResponseTime" -> {  {{None}, _?NumericQ}
},
"URLSave" -> {  {{None}, _String},
  {{None, _String}, _String}
},
"URLSaveAsynchronous" -> {  {{None}, None}
},
"URLShorten" -> {  {{_String}, _String}
},
"URLSubmit" -> {  {{None}, None}
},
"UsingFrontEnd" -> {  {{None}, None}
},
"V2Get" -> {  {{_String}, None}
},
"Variables" -> {  {{None}, _List}
},
"VarianceEquivalenceTest" -> {  {{_List}, None},
  {{_List, _List}, None}
},
"VarianceGammaDistribution" -> {  {{_Real, _Real, _Real, _Real}, None}
},
"VarianceGammaPointProcess" -> {  {{_Real, _Real, _Real, _Real}, None}
},
"VarianceTest" -> {  {{_List}, None},
  {{_List, _List}, None}
},
"VariogramModel" -> {  {{_String, _List}, None}
},
"VectorAround" -> {  {{_List, _List}, _List}
},
"VectorDatabaseObject" -> {  {{None}, None}
},
"VectorDatabaseObjects" -> {  {{__}, _List}
},
"VectorDatabaseSearch" -> {  {{None, None}, _List}
},
"VectorGreater" -> {  {{None}, _?BooleanQ}
},
"VectorGreaterEqual" -> {  {{None}, _?BooleanQ}
},
"VectorLess" -> {  {{None}, _?BooleanQ}
},
"VectorLessEqual" -> {  {{None}, _?BooleanQ}
},
"Vectors" -> {  {{_Integer}, None},
  {{_Integer, None}, None}
},
"VectorSymbol" -> {  {{_String}, None}
},
"Vee" -> {},"Verbatim" -> {  {{None}, None}
},
"VerificationTest" -> {  {{None, __}, None}
},
"VerifyDerivedKey" -> {  {{None, None}, _?BooleanQ}
},
"VerifyTreeGameStrategy" -> {  {{None, None}, _?BooleanQ}
},
"VerticalBar" -> {},"VerticalGauge" -> {  {{None}, _Graphics}
},
"VerticalSeparator" -> {},"VerticalSlider" -> {  {{None}, None}
},
"VerticalTilde" -> {},"VoiceStyleData" -> {  {{__}, None}
},
"VoigtDistribution" -> {  {{_Real, _Real}, None}
},
"VolcanoData" -> {  {{None}, None},
  {{None, _String}, None}
},
"Volume" -> {  {{None}, _?NumericQ}
},
"VonMisesDistribution" -> {  {{_Real, _Real}, None}
},
"VonMisesStress" -> {  {{None}, _?NumericQ}
},
"WaitAll" -> {  {{None}, None}
},
"WaitAsynchronousTask" -> {  {{None}, None}
},
"WaitNext" -> {  {{None}, _List}
},
"WakebyDistribution" -> {  {{_Real, _Real, _Real, _Real, _Real}, None}
},
"WalleniusHypergeometricDistribution" -> {  {{_Integer, _Integer, _Integer, _Real}, None}
},
"WaringYuleDistribution" -> {  {{_Real, _Real}, None}
},
"WarpingCorrespondence" -> {  {{None, None}, _List}
},
"WarpingDistance" -> {  {{None, None}, _?NumericQ}
},
"WatershedComponents" -> {  {{_Image}, _List}
},

"WatsonUSquareTest" -> {  {{_List}, None},
  {{_List, None}, None}
},
"WattsStrogatzGraphDistribution" -> {  {{_Integer, _Integer, _Real}, None}
},
"WaveletBestBasis" -> {{{None}, None}},"WaveletFilterCoefficients" -> {  {{None}, _List},
  {{None, _String}, _List}
},
"WaveletMapIndexed" -> {{{"Expression", None}, None}},"WaveletPhi" -> {{{None, None}, _?NumericQ}},"WaveletPsi" -> {{{None, None}, _?NumericQ}},"WaveletScalogram" -> {{{None}, _Graphics}},"WaveletThreshold" -> {  {{None}, None},
  {{None, None}, None}
},
"WavePDEComponent" -> {{{"Expression", _List}, None}},"WeaklyConnectedComponents" -> {{{_Graph}, _List}},"WeaklyConnectedGraphComponents" -> {{{_Graph}, _List}},"WeakStationarity" -> {  {{_List}, None},
  {{_List, None}, None}
},
"WeatherData" -> {  {{None}, None},
  {{None, _String}, None},
  {{None, _String, None}, None}
},
"WeatherForecastData" -> {  {{None}, None},
  {{None, _String}, None}
},
"WebAudioSearch" -> {{{_String}, _List}},"WebColumn" -> {{{_List}, None}},"WebElementObject" -> {},"WeberE" -> {{{_Integer, None}, _?NumericQ}},"WebExecute" -> {  {{"Expression"}, None},
  {{None, "Expression"}, None}
},
"WebImage" -> {{{_String}, _Image}},"WebImageSearch" -> {{{_String}, _List}},"WebItem" -> {{{None}, None}},"WebRow" -> {{{_List}, None}},"WebSearch" -> {{{_String}, _List}},"WebSessionObject" -> {},"WebSessions" -> {{{}, _List}},"WebWindowObject" -> {},"Wedge" -> {},"WeibullDistribution" -> {{{_Real, _Real}, None}},"WeierstrassE1" -> {{{_List}, _?NumericQ}},"WeierstrassE2" -> {{{_List}, _?NumericQ}},"WeierstrassE3" -> {{{_List}, _?NumericQ}},"WeierstrassEta1" -> {{{_List}, _?NumericQ}},"WeierstrassEta2" -> {{{_List}, _?NumericQ}},"WeierstrassEta3" -> {{{_List}, _?NumericQ}},"WeierstrassHalfPeriods" -> {{{_List}, _List}},"WeierstrassHalfPeriodW1" -> {{{_List}, _?NumericQ}},"WeierstrassHalfPeriodW2" -> {{{_List}, _?NumericQ}},"WeierstrassHalfPeriodW3" -> {{{_List}, _?NumericQ}},"WeierstrassInvariantG2" -> {{{_List}, _?NumericQ}},"WeierstrassInvariantG3" -> {{{_List}, _?NumericQ}},"WeierstrassInvariants" -> {{{_List}, _List}},"WeierstrassP" -> {{{None, _List}, _?NumericQ}},"WeierstrassPPrime" -> {{{None, _List}, _?NumericQ}},"WeierstrassSigma" -> {{{None, _List}, _?NumericQ}},"WeierstrassZeta" -> {{{None, _List}, _?NumericQ}},"WeightedAdjacencyGraph" -> {  {{_List}, _Graph},
  {{_List, _List}, _Graph}
},
"WeightedData" -> {{{_List, _List}, None}},"WelchWindow" -> {{{None}, _?NumericQ}},"WeylAlgebra" -> {  {{None, __}, None}
},
"WheelGraph" -> {  {{_Integer}, _Graph}
},
"WhenEvent" -> {  {{None, None}, None}
},
"While" -> {  {{None, None}, None}
},
"WhiteNoiseProcess" -> {  {{}, None},
  {{None}, None}
},
"WienerFilter" -> {  {{_Image, None}, _Image}
},
"WienerProcess" -> {  {{}, None},
  {{None, None}, None}
},
"WignerD" -> {  {{_List, None}, _Complex},
  {{_List, None, None, None}, _Complex}
},
"WignerSemicircleDistribution" -> {  {{None}, None}
},
"WikidataData" -> {  {{None}, None},
  {{None, None}, None}
},
"WikidataSearch" -> {  {{_String}, _List}
},
"WikipediaData" -> {  {{_String}, None},
  {{_String, None}, None}
},
"WikipediaSearch" -> {  {{_String}, _List}
},
"WilksW" -> {  {{None}, _Real}
},
"WilksWTest" -> {  {{None}, None},
  {{None, None}, None}
},
"WindDirectionData" -> {  {{None}, None},
  {{None, None}, None}
},
"WindingPolygon" -> {  {{None}, None}
},
"WindSpeedData" -> {  {{None}, None},
  {{None, None}, None}
},
"WindVectorData" -> {  {{None}, None},
  {{None, None}, None}
},
"WinsorizedMean" -> {  {{_List, None}, _Real}
},
"WinsorizedVariance" -> {  {{_List, None}, _Real}
},
"WithCleanup" -> {  {{None, None}, None}
},
"WithLock" -> {  {{None, None}, None}
},
"WolframLanguageData" -> {  {{None}, None},
  {{None, None}, None}
},
"WordCloud" -> {  {{None}, _Graphics}
},
"WordCounts" -> {  {{_String}, _Association}
},
"WordData" -> {  {{_String}, None},
  {{_String, None}, None}
},
"WordDefinition" -> {  {{_String}, _String}
},
"WordFrequency" -> {  {{None, _String}, _Real}
},
"WordFrequencyData" -> {  {{_String}, None},
  {{_String, None}, None}
},
"WordStem" -> {  {{_String}, _String}
},
"WordTranslation" -> {  {{_String, None}, _String}
},
"WriteLine" -> {  {{_String}, None},
  {{None, _String}, None}
},
"Wronskian" -> {  {{_List, None}, None}
},
"XMLElement" -> {  {{_String, _List, _List}, None}
},
"XMLObject" -> {  {{_String}, None}
},
"XMLTemplate" -> {  {{None}, None}
},
"Xnor" -> {  {{None, __}, _?BooleanQ}
},
"YuleDissimilarity" -> {  {{_List, _List}, _Real}
},
"ZernikeR" -> {  {{_Integer, _Integer, None}, _?NumericQ}
},
"ZeroSymmetric" -> {  {{_Integer}, None}
},
"ZetaZero" -> {  {{_Integer}, _Complex}
},
"ZIPCodeData" -> {  {{None}, None},
  {{None, None}, None}
},
"ZipfDistribution" -> {  {{None}, None},
  {{None, None}, None}
},
"ZTest" -> {  {{None}, None},
  {{None, None}, None}
}
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
"StringCases", "StringSplit", "StringCount", "StringPosition",
"StringApply"};
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

(* Convert entries from string format to WL pattern format before writing.
   Auto-generated entries use string type names ("Integer", "...", etc.);
   $identityOverrides entries are already in WL pattern format.
   This pass produces a uniform WL-pattern format for the output file. *)
convertArgSpec[s_String] :=
  Which[
    s === "...", BlankSequence[],
    s === "*",   BlankNullSequence[],
    StringEndsQ[s, "..."], BlankSequence[Symbol[StringDrop[s, -3]]],
    StringEndsQ[s, "*"],   BlankNullSequence[Symbol[StringDrop[s, -1]]],
    StringStartsQ[s, "_"], ToExpression[s],
    True, Blank[Symbol[s]]
  ]
convertArgSpec[None] := Blank[]
convertArgSpec[p_] := p  (* already a WL pattern — pass through *)

convertReturnPat[None] := None
convertReturnPat[s_String] :=
  If[StringMatchQ[s, "_[" ~~ DigitCharacter.. ~~ "]" ~~ ___],
    s,  (* parametric like "_[1]" — keep as string *)
    ToExpression[s]  (* "_Integer" -> _Integer, "_?BooleanQ" -> _?BooleanQ, etc. *)
  ]
convertReturnPat[p_] := p  (* already a WL pattern — pass through *)

results = Map[
  Function[{ovs}, Map[
    Function[{ov}, {convertArgSpec /@ ov[[1]], convertReturnPat[ov[[2]]]}],
    ovs
  ]],
  results
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
<> "   Each overload is {inputPatternList, returnPattern}.\n"
<> "   inputPatternList entries: WL blank pattern (_String, _Integer, _Real,\n"
<> "     _Image, _Audio, _Video, _Association, _List, _Graph,\n"
<> "     _SparseArray, _Symbol, _Complex, _Rational, _Rule,\n"
<> "     _RuleDelayed, _Quantity, _?BooleanQ, _?NumericQ) or None (untyped),\n"
<> "     or sequence pattern __ (1+ untyped), ___ (0+ untyped),\n"
<> "     __Type or ___Type for typed sequences (e.g. __Integer).\n"
<> "     {} means zero arguments.\n"
<> "   returnPattern: WL pattern expression (_Integer, _String, _?BooleanQ, _List,\n"
<> "     _Image, _Audio, _Real, _Association, _Graph, _Quantity, etc.),\n"
<> "     or \"_[1]\" string (return type = type of arg 1, e.g. Echo or Identity),\n"
<> "     or None.\n"
<> "   Sources: DownValues (input patterns), WolframLanguageData PlaintextUsage\n"
<> "     (return types + PU-only fallback), TypeSystem`$Signatures (formal type rules),\n"
<> "     Attributes (NumericFunction, Listable), SyntaxInformation (arity),\n"
<> "     deeper RHS analysis (Module/Block/With/If traversal),\n"
<> "     cross-overload propagation,\n"
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
WriteString[stream, "|>\n"];
