
#
# Resolve which executable to use:
#   - If WOLFRAMSCRIPT_ENTITLEMENT_ID is not set as a cmake variable, fall back
#     to the WOLFRAMSCRIPT_ENTITLEMENTID environment variable (used automatically
#     by wolframscript and also set in the GitHub Actions workflow).
#   - When an entitlement ID is available, invoke wolframscript with -entitlement
#     so that the Wolfram Engine can activate on CI runners.
#   - Otherwise fall back to calling WOLFRAMKERNEL directly (local builds where
#     the kernel is already activated).
#
if(NOT DEFINED WOLFRAMSCRIPT_ENTITLEMENT_ID OR WOLFRAMSCRIPT_ENTITLEMENT_ID STREQUAL "")
  set(WOLFRAMSCRIPT_ENTITLEMENT_ID "$ENV{WOLFRAMSCRIPT_ENTITLEMENTID}")
endif()

if(NOT WOLFRAMSCRIPT_ENTITLEMENT_ID STREQUAL "")
  set(_wl_cmd wolframscript -entitlement ${WOLFRAMSCRIPT_ENTITLEMENT_ID})
else()
  if(NOT EXISTS ${WOLFRAMKERNEL})
    message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
  endif()
  set(_wl_cmd ${WOLFRAMKERNEL})
endif()

if(NOT DEFINED RETRY_ON_FAILURE)
set(RETRY_ON_FAILURE OFF)
endif()

if(NOT EXISTS ${SCRIPT})
message(FATAL_ERROR "SCRIPT does not exist. SCRIPT: ${SCRIPT}")
endif()

file(READ ${SCRIPT} script)

if(script STREQUAL "")
message(FATAL_ERROR "SCRIPT is empty. SCRIPT: ${SCRIPT}")
endif()

if(RETRY_ON_FAILURE)

#
# try twice
#

execute_process(
  COMMAND
    ${_wl_cmd} -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -pacletLayoutDir ${PACLET_LAYOUT_DIR} -paclet ${PACLET}
  TIMEOUT
    ${KERNEL_TIMEOUT}
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
message(WARNING "First try: Bad exit code from script: ${SCRIPT_RESULT}; retrying...")

execute_process(
  COMMAND
    ${_wl_cmd} -retry -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -pacletLayoutDir ${PACLET_LAYOUT_DIR} -paclet ${PACLET}
  TIMEOUT
    ${KERNEL_TIMEOUT}
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
message(FATAL_ERROR "Second try: Bad exit code from script: ${SCRIPT_RESULT}; stopping")
else()
message(STATUS "Second try: Success!")
endif()

endif()

else(RETRY_ON_FAILURE)

#
# only try once
#

execute_process(
  COMMAND
    ${_wl_cmd} -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -pacletLayoutDir ${PACLET_LAYOUT_DIR} -paclet ${PACLET}
  TIMEOUT
    ${KERNEL_TIMEOUT}
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
message(FATAL_ERROR "Bad exit code from script: ${SCRIPT_RESULT}")
endif()

endif()
