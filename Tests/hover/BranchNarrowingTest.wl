(* Branch narrowing test fixture. Line numbers are 1-based; LSP positions are 0-based. *)
bNarrowSq[x_Integer] := x * x

(* Case 1: x == literal -> true branch: x inferred as Integer 5 -> no warn
            false branch: x inferred as Except[5] -> conservative, no warn *)
If[ x1 == 5,
  bNarrowSq[ x1 ],
  bNarrowSq[ x1 ]
]

(* Case 2: StringQ[x] -> true branch has x as String -> WARN
            false branch: no narrowing -> no warn *)
If[ StringQ[ x2 ],
  bNarrowSq[ x2 ],
  bNarrowSq[ x2 ]
]

(* Case 3: !StringQ[x] -> false branch has x as String -> WARN
            true branch: no narrowing -> no warn *)
If[ !StringQ[ x3 ],
  bNarrowSq[ x3 ],
  bNarrowSq[ x3 ]
]

(* Case 4: Switch _Integer branch -> no warn; _String branch -> WARN *)
Switch[ x4,
  _Integer, bNarrowSq[ x4 ],
  _String,  bNarrowSq[ x4 ]
]
