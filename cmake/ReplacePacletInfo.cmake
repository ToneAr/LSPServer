
file(READ ${PACLETINFO_IN_SOURCE} filedata)

string(TIMESTAMP DATESTRING "%a %d %b %Y %H:%M:%S")

string(REGEX REPLACE "BuildDate -> \"[a-zA-Z0-9 :]*\"" "BuildDate -> \"${DATESTRING}\"" filedata ${filedata})

string(REGEX REPLACE "BuildNumber -> [0-9]+" "BuildNumber -> ${BUILDNUMBER}" filedata ${filedata})

string(REGEX REPLACE "BuildWolframVersionNumber -> [0-9]+" "BuildWolframVersionNumber -> ${VERSION_NUMBER}" filedata ${filedata})

string(REGEX REPLACE "BuildWolframLibraryVersion -> [0-9]+" "BuildWolframLibraryVersion -> ${WOLFRAMLIBRARY_VERSION}" filedata ${filedata})

string(REGEX REPLACE "Transport -> \"[a-zA-Z]*\"" "Transport -> \"${TRANSPORT}\"" filedata ${filedata})

string(REGEX REPLACE "PlatformQualifier -> \"[a-zA-Z0-9 :]*\"" "PlatformQualifier -> \"${PlatformQualifier}\"" filedata ${filedata})

if(SYSTEMID AND NOT SYSTEMID STREQUAL "")
	if(${SYSTEMID} MATCHES "MacOSX")
		string(REGEX REPLACE "SystemID -> {[^}]*}" "SystemID -> {\"MacOSX-x86-64\",\"MacOSX-ARM64\"}" filedata ${filedata})
	else()
		string(REGEX REPLACE "SystemID -> {[^}]*}" "SystemID -> {\"${SYSTEMID}\"}" filedata ${filedata})
	endif()
else()
	# Auto-detect SystemIDs from LibraryResources subdirectories
	get_filename_component(PACLET_DIR "${REPLACED_PACLETINFO}" DIRECTORY)
	file(GLOB SYSID_CANDIDATES LIST_DIRECTORIES true "${PACLET_DIR}/LibraryResources/*")
	set(SYSID_WL_LIST "")
	foreach(CANDIDATE ${SYSID_CANDIDATES})
		if(IS_DIRECTORY "${CANDIDATE}")
			get_filename_component(SYSID_NAME "${CANDIDATE}" NAME)
			if(SYSID_WL_LIST)
				set(SYSID_WL_LIST "${SYSID_WL_LIST}, \"${SYSID_NAME}\"")
			else()
				set(SYSID_WL_LIST "\"${SYSID_NAME}\"")
			endif()
		endif()
	endforeach()
	if(SYSID_WL_LIST)
		string(REGEX REPLACE "SystemID -> {[^}]*}" "SystemID -> {${SYSID_WL_LIST}}" filedata ${filedata})
	endif()
endif()


if(LOCAL_BUILD)

string(REGEX REPLACE "Version -> \"[0-9\\.]+\"," "Version -> \"${LOCAL_BUILD_VERSION}\"(* local build *)," filedata ${filedata})

endif()

file(WRITE ${REPLACED_PACLETINFO} "${filedata}")
