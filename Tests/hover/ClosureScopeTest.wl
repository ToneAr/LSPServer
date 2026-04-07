(* Test fixture for closure-scope type inference.
   Line numbers are 1-based; LSP positions are 0-based (line - 1). *)

(*  Case A: flow-sensitive hover after body reassignment.
    x = 5 in var-list (Integer), then x = "hello" in body.
    Hovering on x after the reassignment should give _String. *)
Module[{x = 5},
  x = "hello";
  x
]

(*  Case B: flow-sensitive hover before body reassignment.
    x = 5 in var-list (Integer), x = "hello" appears later.
    Hovering on x; (before reassignment) should give _Integer. *)
Module[{x = 5},
  x;
  x = "hello"
]

(*  Case C: uninitialized local variable.
    Hovering on x inside Module[{x}, ...] should give _ (Blank[]). *)
Module[{x},
  x
]

(*  Case D: scope bleed check.
    x inside the module is Integer. x at file scope should have no type.
    Hover test (below) checks module-local x. Diagnostic tests are in ClosureScopeDiag.wlt:
    needsStr[x] at file scope should NOT warn; needsStr[x] inside Module SHOULD warn. *)
needsStr[x_String] := x

Module[{x = 5},
  x
];

x

needsStr[x]

Module[{x = 5},
  needsStr[x]
]
