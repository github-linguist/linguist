# APPLE LOCAL file string workaround 4943900
if { [istarget "*-*-darwin\[9123\]*"] } {
  set additional_flags "-framework Foundation -fconstant-cfstrings"
}
return 0
