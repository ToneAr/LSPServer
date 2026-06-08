
macro(CheckPacletInfo)

  if(NOT EXISTS ${WOLFRAMKERNEL})
  message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
  endif()

  if(LOCAL_BUILD)
    message(STATUS "Paclet Version ignored in local build")
    set(LOCAL_BUILD_VERSION 999.9)
  else()
    #
    # if not local build, then get Version from PacletInfo.wl
    #
    set(_paclet_info_code
"pacletInfo = List @@ Get[\"${PACLETINFO_IN_SOURCE}\"]
Print[OutputForm[Version /. pacletInfo]]
Print[OutputForm[WolframVersion /. pacletInfo]]"
    )

    RunWolframKernelScript("${_paclet_info_code}" PACLET_VERSIONS_OUTPUT PACLETINFO_RESULT)

    if(NOT ${PACLETINFO_RESULT} EQUAL "0")
      message(FATAL_ERROR "Bad exit code from PacletInfo script: ${PACLETINFO_RESULT}")
    endif()

    string(REPLACE "\r\n" "\n" PACLET_VERSIONS_OUTPUT "${PACLET_VERSIONS_OUTPUT}")
    string(REPLACE "\n" ";" PACLET_VERSIONS_LIST "${PACLET_VERSIONS_OUTPUT}")

    list(LENGTH PACLET_VERSIONS_LIST PACLET_VERSIONS_LIST_LENGTH)
    if(NOT ${PACLET_VERSIONS_LIST_LENGTH} EQUAL "2")
      message(FATAL_ERROR "Could not parse PacletInfo version output: ${PACLET_VERSIONS_OUTPUT}")
    endif()

    list(GET PACLET_VERSIONS_LIST 0 PACLET_VERSION)
    list(GET PACLET_VERSIONS_LIST 1 PACLET_WOLFRAMVERSION)
    message(STATUS "PACLET_VERSION: ${PACLET_VERSION}")
    message(STATUS "PACLET_WOLFRAMVERSION: ${PACLET_WOLFRAMVERSION}")
    
  endif(LOCAL_BUILD)

endmacro(CheckPacletInfo)
