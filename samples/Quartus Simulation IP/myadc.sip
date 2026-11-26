set_global_assignment -entity "myadc" -library "lib_myadc" -name IP_TOOL_NAME "Qsys"
set_global_assignment -entity "myadc" -library "lib_myadc" -name IP_TOOL_VERSION "18.0"
set_global_assignment -entity "myadc" -library "lib_myadc" -name IP_TOOL_ENV "Qsys"
set_global_assignment -library "lib_myadc" -name SPD_FILE [file join $::quartus(sip_path) "../myadc.spd"]
set_global_assignment -library "lib_myadc" -name MISC_FILE [file join $::quartus(sip_path) "../../myadc.qsys"]

set_global_assignment -library "lib_myadc" -name MISC_FILE [file join $::quartus(sip_path) "myadc.v"]
set_global_assignment -library "lib_myadc" -name MISC_FILE [file join $::quartus(sip_path) "submodules/myadc_modular_adc_0.v"]
set_global_assignment -library "lib_myadc" -name MISC_FILE [file join $::quartus(sip_path) "submodules/altera_modular_adc_control.v"]
set_global_assignment -library "lib_myadc" -name MISC_FILE [file join $::quartus(sip_path) "submodules/altera_modular_adc_control_avrg_fifo.v"]
set_global_assignment -library "lib_myadc" -name MISC_FILE [file join $::quartus(sip_path) "submodules/altera_modular_adc_control_fsm.v"]
set_global_assignment -library "lib_myadc" -name MISC_FILE [file join $::quartus(sip_path) "submodules/chsel_code_converter_sw_to_hw.v"]
set_global_assignment -library "lib_myadc" -name MISC_FILE [file join $::quartus(sip_path) "submodules/fiftyfivenm_adcblock_primitive_wrapper.v"]
set_global_assignment -library "lib_myadc" -name MISC_FILE [file join $::quartus(sip_path) "submodules/fiftyfivenm_adcblock_top_wrapper.v"]
