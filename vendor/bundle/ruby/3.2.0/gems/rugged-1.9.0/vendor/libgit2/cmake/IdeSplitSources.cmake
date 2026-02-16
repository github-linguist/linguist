# This function splits the sources files up into their appropriate
# subdirectories.  This is especially useful for IDEs like Xcode and
# Visual Studio, so that you can navigate into the libgit2_tests project,
# and see the folders within the tests folder (instead of just seeing all
# source and tests in a single folder.)
function(IDE_SPLIT_SOURCES target)
	if(MSVC_IDE OR CMAKE_GENERATOR STREQUAL Xcode)
		get_target_property(sources ${target} SOURCES)
		foreach(source ${sources})
			if(source MATCHES ".*/")
				string(REPLACE ${PROJECT_SOURCE_DIR}/ "" rel ${source})
				if(rel)
					string(REGEX REPLACE "/([^/]*)$" "" rel ${rel})
					if(rel)
						string(REPLACE "/" "\\\\" rel ${rel})
						source_group(${rel} FILES ${source})
					endif()
				endif()
			endif()
		endforeach()
	endif()
endfunction()
