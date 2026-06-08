(* Test fixture for options-arity diagnostic.
   Functions that accept trailing options should not produce arity warnings
   when called with Rule/RuleDelayed option arguments. *)

(*  Case A: fixed-arity function called with a trailing option.
    No arity warning should fire — the Rule arg is an option, not a positional arg. *)
fixedArityFn[x_String] := x

fixedArityFn["hello", Method -> "auto"]

(*  Case B: type mismatch should still fire even when options are present.
    Passing Integer where _String expected is still wrong. *)
fixedArityFn[42, Method -> "auto"]

(*  Case C: RuleDelayed should also be ignored as an option.
    No arity warning. *)
fixedArityFn["world", Method :> "auto"]

(*  Case D: a named OptionsPattern parameter forwarded to another option-aware
    function should be treated as an option sequence, not a positional argument. *)
optionForwardTarget[x_Integer, opts: OptionsPattern[]] := 1

optionForwardWrapper[x_String, opts: OptionsPattern[]] :=
  optionForwardTarget[Internal`StringToMInteger[x], opts]

optionAliasWrapper[x_String, opts: OptionsPattern[]] :=
  With[{passedOpts = opts},
    optionForwardTarget[Internal`StringToMInteger[x], passedOpts]
  ]

(* Case E: a conditioned overload LHS should be indexed as a definition,
   not treated as a call to the one-argument overload. *)
conditionedOverloadFn[x_String] := x

conditionedOverloadFn[x_String, y_String] /; StringQ[x] := x <> y

conditionedOverloadFn["left", "right"]

(* Case F: a named default OptionsPattern parameter should infer the owning head. *)
discoverDomainRoots // PackageExported;

discoverDomainRoots[startURL_String, opts : OptionsPattern[]] :=
  With[{urls = crawlDomainURLs[startURL, opts]},
    opts
  ]

(* Case G: OptionsPattern[] and Rules-style option captures should be
   interchangeable for forwarding and arity/type checks. *)
optionsTargetForRules[x_Integer, opts : OptionsPattern[]] := x

rulesSequenceWrapper[x_Integer, opts___Rules] :=
  optionsTargetForRules[x, opts]

rulesListWrapper[x_Integer, opts : {___Rules}] :=
  optionsTargetForRules[x, opts]

rulesSequenceTarget[x_Integer, opts___Rules] := x

optionsWrapperForRulesSequence[x_Integer, opts : OptionsPattern[]] :=
  rulesSequenceTarget[x, opts]

rulesListTarget[x_Integer, opts : {___Rules}] := x

optionsWrapperForRulesList[x_Integer, opts : OptionsPattern[]] :=
  rulesListTarget[x, opts]

Options[sequenceFilterRulesTarget] = {Method -> Automatic};

sequenceFilterRulesTarget[x_Integer, opts : OptionsPattern[]] := x

sequenceFilterRulesTarget[
  1,
  Sequence @@ FilterRules[{Method -> "auto"}, Options[sequenceFilterRulesTarget]]
]
