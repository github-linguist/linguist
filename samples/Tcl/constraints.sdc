# Add timing constraints here
set_time_format -unit ns -decimal_places 3
create_clock -period 10.000 -waveform {0.000 5.000} [get_ports {clk}]