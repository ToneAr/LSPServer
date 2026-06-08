
if(NOT EXISTS ${WOLFRAMKERNEL})
message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
endif()

if(NOT DEFINED BUILDDIR OR BUILDDIR STREQUAL "")
  get_filename_component(BUILDDIR ${PACLET_OUTPUT} DIRECTORY)
endif()

set(CODE_TEMPLATE [=[
Print[OutputForm["Calling PacletInstall..."]];
Check[
res = PacletInstall["@PACLET_OUTPUT@", ForceVersionInstall -> True];
,
Print[OutputForm[Row[{"$VersionNumber: ", NumberForm[$VersionNumber, {2, 1}]}]]];
Print[OutputForm[Row[{"Paclet WolframVersion: ", "@PACLET_WOLFRAMVERSION@"}]]];
Print[OutputForm[Row[{"To prevent this PacletInstall::compat message, update PacletInfo.wl.in with WolframVersion -> \"", NumberForm[$VersionNumber, {2, 1}] ,"\" and build and install again."}]]];
res
,
{PacletInstall::compat}
];
Print[res // OutputForm];
Print[OutputForm["Done PacletInstall"]];
If[!PacletObjectQ[res],
  Quit[1]
]
]=])

string(CONFIGURE "${CODE_TEMPLATE}" CODE @ONLY)

set(INSTALL_SCRIPT_DIR ${BUILDDIR}/CMakeFiles/WolframKernelScripts)
set(INSTALL_SCRIPT ${INSTALL_SCRIPT_DIR}/InstallPaclet.wl)

file(MAKE_DIRECTORY ${INSTALL_SCRIPT_DIR})
file(WRITE ${INSTALL_SCRIPT} "${CODE}\n")

execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -noinit -noprompt -script ${INSTALL_SCRIPT}
  TIMEOUT
    ${KERNEL_TIMEOUT}
  RESULT_VARIABLE
    INSTALL_RESULT
)

if(NOT ${INSTALL_RESULT} EQUAL "0")
  message(FATAL_ERROR "Bad exit code from install: ${INSTALL_RESULT}")
endif()
