// Copyright (c) 2010-2026 Zhang Weidong <zwd@funlang.org>
// SPDX-License-Identifier: MIT

########################################
# lib-json
########################################
#   built-in (2)
########################################
#     set.@toJson (format, json, isGbk, fd, kvd=32)   # save set as JSON/FD string
#     str.getJson (json=false, fd=false, sse = false) # load str as JSON/FD set/list/tree
########################################
# 1. format param of @toJson()
#    default 0, format levels
#    join (concat) if format = -1
# 2. json param of @toJson()
#    default false, name quoted as "name"
# 3. fd param of @toJson()/getJson()
#    format 0 - indent 2 white-space
#           1 - >
#           2 - n>
#               n from 1 to 2,3... level
#    kvd   32 - default whitespace
#    sse false- sse decompress
########################################

fun JSON()
  fun Parse(s)
    result = s.getJson(json: true);
  end fun;

  fun Stringify(s, level)
    result = s.@toJson(level, json: true);
  end fun;
end fun;

fun FD()
  fun Parse(s, sse)
    result = s.getJson(fd: true, sse: sse);
  end fun;

  fun Stringify(s, level, kvd)
    result = s.@toJson(level, fd: true, kvd: kvd or 32);
  end fun;
end fun;
