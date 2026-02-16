file(REMOVE_RECURSE
  "../../libgit2.a"
  "../../libgit2.pdb"
)

# Per-language clean rules from dependency scanning.
foreach(lang C)
  include(CMakeFiles/libgit2package.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
