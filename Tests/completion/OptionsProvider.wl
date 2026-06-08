Options[projectOptionFn] = {
  ProjectChoice -> Automatic,
  ProjectFlag -> False
};

projectOptionFn[x_, OptionsPattern[]] := x

projectOptionWrapper[x_, OptionsPattern[projectOptionFn]] := projectOptionFn[x]
