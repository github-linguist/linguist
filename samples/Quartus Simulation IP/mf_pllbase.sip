set_global_assignment -entity "mf_pllbase" -library "lib_mf_pllbase" -name IP_TOOL_NAME "altera_pll"
set_global_assignment -entity "mf_pllbase" -library "lib_mf_pllbase" -name IP_TOOL_VERSION "20.1"
set_global_assignment -entity "mf_pllbase" -library "lib_mf_pllbase" -name IP_TOOL_ENV "mwpim"
set_global_assignment -library "lib_mf_pllbase" -name SPD_FILE [file join $::quartus(sip_path) "mf_pllbase.spd"]

set_global_assignment -library "lib_mf_pllbase" -name MISC_FILE [file join $::quartus(sip_path) "mf_pllbase_sim/mf_pllbase.vo"]
