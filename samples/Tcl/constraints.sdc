#
# Software License Agreement (MIT License)
#
# Copyright (c) 2018, DUKELEC, Inc.
# All rights reserved.
#
# Author: Duke Fong <duke@dukelec.com>
#

create_clock -name "clk_50m" -period 20ns [get_ports gclk]

create_clock -period "100 MHz" -name {refclk_pci_express} {pcie_refclk}
create_clock -name "clk_app_62.5m" -period 16ns inst|pcie_ip|pcie_internal_hip|cyclone_iii.cycloneiv_hssi_pcie_hip|coreclkout

derive_pll_clocks -create_base_clocks
derive_clock_uncertainty

set_clock_groups -asynchronous \
        -group {clk_50m} \
        -group {altpll_top|altpll_component|auto_generated|pll1|clk[0]} \
        -group {altpll_top|altpll_component|auto_generated|pll1|clk[1]} \
        -group {cdctl_sys_m|pcie_ip|pcie_internal_hip|cyclone_iii.cycloneiv_hssi_pcie_hip|coreclkout} \
        -group {refclk_pci_express}

set_false_path -from [get_ports {pcie_rx rst_n pio_0_pins[*]}] -to *
set_false_path -from * -to [get_ports {pcie_tx led0 pio_0_pins[*]}]
