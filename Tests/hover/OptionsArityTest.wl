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
