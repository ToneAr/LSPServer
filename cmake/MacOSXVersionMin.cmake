
macro(CheckMacOSXVersionMin)

  if(NOT EXISTS ${WOLFRAMKERNEL})
  message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
  endif()
  
  set(_macosx_version_min_code
"Needs[\"CCompilerDriver`\"]
Print[OutputForm[StringReplace[CCompilerDriver`CCompilerDriverBase`MacOSXVersionMinFlag[], \"-mmacosx-version-min=\" -> \"\"]]]"
  )

  RunWolframKernelScript("${_macosx_version_min_code}" MACOSX_VERSION_MIN MACOSX_VERSION_MIN_RESULT)

  if(NOT ${MACOSX_VERSION_MIN_RESULT} EQUAL "0")
    message(FATAL_ERROR "Bad exit code from MacOSXVersionMin script: ${MACOSX_VERSION_MIN_RESULT}")
  endif()

endmacro(CheckMacOSXVersionMin)
