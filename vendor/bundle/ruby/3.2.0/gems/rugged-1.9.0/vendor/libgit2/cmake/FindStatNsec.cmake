include(FeatureSummary)

check_struct_has_member("struct stat" st_mtim "sys/types.h;sys/stat.h"
	HAVE_STRUCT_STAT_ST_MTIM LANGUAGE C)
check_struct_has_member("struct stat" st_mtimespec "sys/types.h;sys/stat.h"
	HAVE_STRUCT_STAT_ST_MTIMESPEC LANGUAGE C)
check_struct_has_member("struct stat" st_mtime_nsec sys/stat.h
	HAVE_STRUCT_STAT_MTIME_NSEC LANGUAGE C)

if(HAVE_STRUCT_STAT_ST_MTIM)
	check_struct_has_member("struct stat" st_mtim.tv_nsec sys/stat.h
		HAVE_STRUCT_STAT_NSEC LANGUAGE C)
elseif(HAVE_STRUCT_STAT_ST_MTIMESPEC)
	check_struct_has_member("struct stat" st_mtimespec.tv_nsec sys/stat.h
		HAVE_STRUCT_STAT_NSEC LANGUAGE C)
else()
	set(HAVE_STRUCT_STAT_NSEC ON )
endif()

add_feature_info(nanoseconds USE_NSEC "support nanosecond precision file mtimes and ctimes")
