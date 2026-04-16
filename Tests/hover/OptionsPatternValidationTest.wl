Options[optionOnlyFn] = {
  Method -> "auto",
  ProjectChoice -> Automatic
};

optionOnlyFn[OptionsPattern[]] := OptionValue[Method]

optionOnlyFn[]

optionOnlyFn[Method -> "auto"]

optionOnlyFn[UnknownOption -> True]
