EESchema Schematic File Version 2
LIBS:simonShield-rescue
LIBS:simonLib
LIBS:74xgxx
LIBS:74xx
LIBS:ac-dc
LIBS:actel
LIBS:adc-dac
LIBS:Altera
LIBS:analog_devices
LIBS:analog_switches
LIBS:atmel
LIBS:audio
LIBS:brooktre
LIBS:cmos4000
LIBS:cmos_ieee
LIBS:conn
LIBS:contrib
LIBS:cypress
LIBS:dc-dc
LIBS:device
LIBS:digital-audio
LIBS:diode
LIBS:display
LIBS:dsp
LIBS:elec-unifil
LIBS:ESD_Protection
LIBS:ftdi
LIBS:gennum
LIBS:graphic
LIBS:hc11
LIBS:intel
LIBS:interface
LIBS:ir
LIBS:Lattice
LIBS:linear
LIBS:logo
LIBS:maxim
LIBS:memory
LIBS:microchip
LIBS:microchip_dspic33dsc
LIBS:microchip_pic10mcu
LIBS:microchip_pic12mcu
LIBS:microchip_pic16mcu
LIBS:microchip_pic18mcu
LIBS:microchip_pic32mcu
LIBS:microcontrollers
LIBS:motor_drivers
LIBS:motorola
LIBS:msp430
LIBS:nordicsemi
LIBS:nxp_armmcu
LIBS:onsemi
LIBS:opto
LIBS:Oscillators
LIBS:philips
LIBS:power
LIBS:powerint
LIBS:Power_Management
LIBS:pspice
LIBS:references
LIBS:regul
LIBS:relays
LIBS:rfcom
LIBS:sensors
LIBS:silabs
LIBS:siliconi
LIBS:stm8
LIBS:stm32
LIBS:supertex
LIBS:switches
LIBS:texas
LIBS:transf
LIBS:transistors
LIBS:ttl_ieee
LIBS:valves
LIBS:video
LIBS:Worldsemi
LIBS:Xicor
LIBS:xilinx
LIBS:Zilog
LIBS:simonShield-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 3 5
Title "Poncho Simon EDU-CIAA"
Date "2016-07-26"
Rev "1.0"
Comp "Proyecto CIAA"
Comment1 "Juan Agustin Bassi"
Comment2 "CESE - Diseño PCB"
Comment3 "Licencia BSD"
Comment4 ""
$EndDescr
$Comp
L BUZZER U1
U 1 1 5777FCBE
P 5600 4150
F 0 "U1" H 5600 4150 60  0000 C CNN
F 1 "BUZZER" H 5600 4150 60  0000 C CNN
F 2 "footprints:MagneticBuzzer_ProSignal_ABT-410-RC" H 5600 4150 60  0001 C CNN
F 3 "" H 5600 4150 60  0000 C CNN
F 4 "445-2525-1-ND" H 5600 4150 60  0001 C CNN "Digikey#"
	1    5600 4150
	0    1    1    0   
$EndComp
$Comp
L BC547 Q1
U 1 1 5777FCC6
P 5500 3350
F 0 "Q1" H 5700 3425 50  0000 L CNN
F 1 "BC547" H 5700 3350 50  0000 L CNN
F 2 "simonShield:TO-92_Molded_Narrow" H 5700 3275 50  0000 L CIN
F 3 "" H 5500 3350 50  0000 L CNN
F 4 "BC33725TACT-ND" H 5500 3350 60  0001 C CNN "Digikey#"
	1    5500 3350
	1    0    0    -1  
$EndComp
$Comp
L R R9
U 1 1 5777FCCD
P 5050 3350
F 0 "R9" V 5130 3350 50  0000 C CNN
F 1 "3K3" V 5050 3350 50  0000 C CNN
F 2 "footprints:Resistor_Horizontal_RM7mm" V 4980 3350 50  0001 C CNN
F 3 "" H 5050 3350 50  0000 C CNN
F 4 "680EBK-ND" H 5050 3350 60  0001 C CNN "Digikey#"
	1    5050 3350
	0    1    1    0   
$EndComp
$Comp
L GND #PWR017
U 1 1 5777FCDA
P 5600 4850
F 0 "#PWR017" H 5600 4600 50  0001 C CNN
F 1 "GND" H 5600 4700 50  0000 C CNN
F 2 "" H 5600 4850 50  0000 C CNN
F 3 "" H 5600 4850 50  0000 C CNN
	1    5600 4850
	1    0    0    -1  
$EndComp
Wire Wire Line
	5600 2600 5600 3150
Wire Wire Line
	5600 3550 5600 3700
Wire Wire Line
	5600 4700 5600 4850
Wire Wire Line
	5200 3350 5300 3350
Text HLabel 4700 3050 0    60   Input ~ 0
PWM
Wire Wire Line
	4700 3050 4700 3350
Wire Wire Line
	4700 3350 4900 3350
$Comp
L R R10
U 1 1 577960FD
P 5600 2450
F 0 "R10" V 5680 2450 50  0000 C CNN
F 1 "1K" H 5600 2450 50  0000 C CNN
F 2 "footprints:Resistor_Horizontal_RM7mm" V 5530 2450 50  0001 C CNN
F 3 "" H 5600 2450 50  0000 C CNN
F 4 "680EBK-ND" H 5600 2450 60  0001 C CNN "Digikey#"
	1    5600 2450
	1    0    0    -1  
$EndComp
Wire Wire Line
	5600 2100 5600 2300
Wire Notes Line
	3800 1000 3800 5450
Wire Notes Line
	3800 5450 7650 5450
Wire Notes Line
	7650 5450 7650 1000
Wire Notes Line
	7650 1350 3800 1350
Wire Notes Line
	7650 1000 3800 1000
Text Notes 5350 1250 0    60   ~ 12
BUZZER CIRCUIT
$Comp
L VDD-RESCUE-simonShield #PWR018
U 1 1 5777FCD4
P 5600 2100
F 0 "#PWR018" H 5600 1950 50  0001 C CNN
F 1 "VDD" H 5600 2250 50  0000 C CNN
F 2 "" H 5600 2100 50  0000 C CNN
F 3 "" H 5600 2100 50  0000 C CNN
	1    5600 2100
	1    0    0    -1  
$EndComp
$EndSCHEMATC
