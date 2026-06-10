
if(NOT DEFINED MATHEMATICA_INSTALL_DIR)
	# Auto-detect via wolframscript; override with -DMATHEMATICA_INSTALL_DIR=<path>
	find_program(_WOLFRAMSCRIPT wolframscript)
	if(NOT _WOLFRAMSCRIPT)
		file(GLOB _ws_candidates
			"$ENV{HOME}/Wolfram/Wolfram/*/Executables/wolframscript"
			"$ENV{HOME}/Wolfram/WolframEngine/*/Executables/wolframscript"
			"$ENV{HOME}/Wolfram/Mathematica/*/Executables/wolframscript"
			"/usr/local/Wolfram/Wolfram/*/Executables/wolframscript"
			"/usr/local/Wolfram/WolframEngine/*/Executables/wolframscript"
			"/usr/local/Wolfram/Mathematica/*/Executables/wolframscript"
		)
		if(_ws_candidates)
			if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.18)
				# Natural compare so e.g. 15.0 sorts after 9.0
				list(SORT _ws_candidates COMPARE NATURAL)
			else()
				list(SORT _ws_candidates)
			endif()
			list(GET _ws_candidates -1 _WOLFRAMSCRIPT)
		endif()
	endif()
	if(_WOLFRAMSCRIPT)
		#
		# A bare expression makes wolframscript print only its value.
		# Print[$InstallationDirectory] must NOT be used here: -code also prints
		# the value of the last expression, so Print emits the directory AND a
		# trailing "Null" line, which would be captured into the variable and
		# embed a newline in every derived path.
		#
		execute_process(
			COMMAND ${_WOLFRAMSCRIPT} -code "$InstallationDirectory"
			OUTPUT_VARIABLE MATHEMATICA_INSTALL_DIR
			RESULT_VARIABLE _ws_result
			OUTPUT_STRIP_TRAILING_WHITESPACE
			TIMEOUT 60
		)
		# Keep only the first line, in case of startup/licensing banners.
		string(REGEX REPLACE "\r?\n.*" "" MATHEMATICA_INSTALL_DIR "${MATHEMATICA_INSTALL_DIR}")
		if(NOT _ws_result EQUAL 0 OR NOT EXISTS "${MATHEMATICA_INSTALL_DIR}")
			message(WARNING "wolframscript (${_WOLFRAMSCRIPT}) did not report a usable $InstallationDirectory (exit ${_ws_result}: \"${MATHEMATICA_INSTALL_DIR}\"); falling back")
			unset(MATHEMATICA_INSTALL_DIR)
		endif()
	endif()
	if(NOT MATHEMATICA_INSTALL_DIR)
		# Look for an installed kernel layout directly.
		file(GLOB _wk_candidates
			"$ENV{HOME}/Wolfram/Wolfram/*/Executables/WolframKernel"
			"$ENV{HOME}/Wolfram/WolframEngine/*/Executables/WolframKernel"
			"$ENV{HOME}/Wolfram/Mathematica/*/Executables/WolframKernel"
			"/usr/local/Wolfram/Wolfram/*/Executables/WolframKernel"
			"/usr/local/Wolfram/WolframEngine/*/Executables/WolframKernel"
			"/usr/local/Wolfram/Mathematica/*/Executables/WolframKernel"
		)
		if(_wk_candidates)
			if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.18)
				list(SORT _wk_candidates COMPARE NATURAL)
			else()
				list(SORT _wk_candidates)
			endif()
			list(GET _wk_candidates -1 _wk_found)
			get_filename_component(_wk_exec_dir ${_wk_found} DIRECTORY)
			get_filename_component(MATHEMATICA_INSTALL_DIR ${_wk_exec_dir} DIRECTORY)
			message(STATUS "Found kernel layout without wolframscript: ${MATHEMATICA_INSTALL_DIR}")
		endif()
	endif()
	if(NOT MATHEMATICA_INSTALL_DIR)
		if(CMAKE_HOST_WIN32)
			set(MATHEMATICA_INSTALL_DIR "C:/Program Files/Wolfram Research/Wolfram Engine/14.1")
		elseif(CMAKE_HOST_APPLE)
			set(MATHEMATICA_INSTALL_DIR "/Applications/Wolfram Engine.app/Contents")
		else()
			set(MATHEMATICA_INSTALL_DIR "/usr/local/Wolfram/WolframEngine/14.1")
		endif()
		message(WARNING "wolframscript not found; using fallback MATHEMATICA_INSTALL_DIR: ${MATHEMATICA_INSTALL_DIR}")
	endif()
endif()

if(CMAKE_HOST_WIN32)
	set(WOLFRAMKERNEL_DEFAULT ${MATHEMATICA_INSTALL_DIR}/wolfram.exe)
	set(WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/IncludeFiles/C)
	#
	# in versions before 11.2, there were 2 separate paths:
	# SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/include
	# SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/lib
	#
	# starting in 11.2, the single path for MathLink includes and MathLink libs is:
	# SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions
	#
	if(EXISTS ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/include)
	set(MATHLINK_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/include)
	else()
	set(MATHLINK_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions)
	endif()
	if(EXISTS ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/lib)
	set(MATHLINK_LIB_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/lib)
	else()
	set(MATHLINK_LIB_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions)
	endif()
elseif(CMAKE_HOST_APPLE)
	set(WOLFRAMKERNEL_DEFAULT ${MATHEMATICA_INSTALL_DIR}/MacOS/WolframKernel)
	set(WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/IncludeFiles/C)
	set(MATHLINK_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions)
	set(MATHLINK_LIB_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions)
else()
	if(NOT EXISTS ${MATHEMATICA_INSTALL_DIR}/Executables/WolframKernel
			AND EXISTS ${MATHEMATICA_INSTALL_DIR}/Executables/wolfram)
		# Some Wolfram Engine layouts ship only the lowercase launcher
		set(WOLFRAMKERNEL_DEFAULT ${MATHEMATICA_INSTALL_DIR}/Executables/wolfram)
	else()
		set(WOLFRAMKERNEL_DEFAULT ${MATHEMATICA_INSTALL_DIR}/Executables/WolframKernel)
	endif()
	set(WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/IncludeFiles/C)
	set(MATHLINK_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Linux-x86-64/CompilerAdditions)
	set(MATHLINK_LIB_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Linux-x86-64/CompilerAdditions)
endif()

function(RunWolframKernelScript CODE OUTPUT_VARIABLE_NAME RESULT_VARIABLE_NAME)

	if(NOT EXISTS ${WOLFRAMKERNEL})
	message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
	endif()

	set(_script_code "Pause[${KERNEL_PAUSE}]\n${CODE}\n")
	string(MD5 _script_hash "${_script_code}")
	set(_script_dir ${PROJECT_BINARY_DIR}/CMakeFiles/WolframKernelScripts)
	set(_script ${_script_dir}/${_script_hash}.wl)

	file(MAKE_DIRECTORY ${_script_dir})
	file(WRITE ${_script} "${_script_code}")

	execute_process(
		COMMAND
			${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -nostartuppaclets -script ${_script}
		OUTPUT_VARIABLE
			_script_output
		OUTPUT_STRIP_TRAILING_WHITESPACE
		WORKING_DIRECTORY
			${PROJECT_SOURCE_DIR}
		TIMEOUT
			${KERNEL_TIMEOUT}
		RESULT_VARIABLE
			_script_result
	)

	set(${OUTPUT_VARIABLE_NAME} "${_script_output}" PARENT_SCOPE)
	set(${RESULT_VARIABLE_NAME} "${_script_result}" PARENT_SCOPE)

endfunction(RunWolframKernelScript)

macro(CheckWolframKernel)

	if(NOT EXISTS ${WOLFRAMKERNEL})
	message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
	endif()

	#
	# get $Version
	#
	RunWolframKernelScript("Print[OutputForm[$Version]]" VERSION VERSION_RESULT)

	message(STATUS "VERSION: ${VERSION}")

	if(NOT ${VERSION_RESULT} EQUAL "0")
	message(WARNING "Bad exit code from Version script: ${VERSION_RESULT}; Continuing")
	endif()

	#
	# get $VersionNumber
	#
	RunWolframKernelScript("Print[OutputForm[Floor[100 $VersionNumber + $ReleaseNumber]]]" VERSION_NUMBER VERSION_NUMBER_RESULT)

	message(STATUS "VERSION_NUMBER: ${VERSION_NUMBER}")

	if(NOT ${VERSION_NUMBER} GREATER_EQUAL 1100)
	message(FATAL_ERROR "Wolfram Kernel must be at least version 11.0: ${VERSION_NUMBER}")
	endif()

	if(NOT ${VERSION_NUMBER_RESULT} EQUAL "0")
	message(WARNING "Bad exit code from VersionNumber script: ${VERSION_NUMBER_RESULT}; Continuing")
	endif()

	#
	# get $SystemID
	#
	RunWolframKernelScript("Print[OutputForm[$SystemID]]" SYSTEMID SYSTEMID_RESULT)

	message(STATUS "SYSTEMID: ${SYSTEMID}")

	if(NOT ${SYSTEMID_RESULT} EQUAL "0")
	message(WARNING "Bad exit code from SystemID script: ${SYSTEMID_RESULT}; Continuing")
	endif()

	#
	# get $SystemWordLength
	#
	RunWolframKernelScript("Print[OutputForm[$SystemWordLength]]" SYSTEMWORDLENGTH SYSTEMWORDLENGTH_RESULT)

	message(STATUS "SYSTEMWORDLENGTH: ${SYSTEMWORDLENGTH}")

	if(NOT ${SYSTEMWORDLENGTH_RESULT} EQUAL "0")
	message(WARNING "Bad exit code from SystemWordLength script: ${SYSTEMWORDLENGTH_RESULT}; Continuing")
	endif()

	#
	# Make sure that CMake and Mathematica agree about 32-bit or 64-bit
	#
	if("${CMAKE_SIZEOF_VOID_P}" STREQUAL "")
	# CMAKE_SIZEOF_VOID_P is not set; CXX is probably not enabled
	elseif(${CMAKE_SIZEOF_VOID_P} EQUAL 4)
	if(NOT ${SYSTEMWORDLENGTH} EQUAL 32)
	message(FATAL_ERROR
		"CMake is reporting 32-bit; Mathematica is reporting: ${SYSTEMWORDLENGTH}\n"
		"HINT: On Windows, you probably need to specify -A x64"
	)
	endif()
	elseif(${CMAKE_SIZEOF_VOID_P} EQUAL 8)
	if(NOT ${SYSTEMWORDLENGTH} EQUAL 64)
	message(FATAL_ERROR "CMake is reporting 64-bit; Mathematica is reporting: ${SYSTEMWORDLENGTH}")
	endif()
	else()
	message(FATAL_ERROR "CMake is reporting neither 32-bit nor 64-bit. CMAKE_SIZEOF_VOID_P: ${CMAKE_SIZEOF_VOID_P}")
	endif()

endmacro(CheckWolframKernel)
