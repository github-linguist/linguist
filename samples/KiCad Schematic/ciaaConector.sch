EESchema Schematic File Version 2
LIBS:simonLib
LIBS:power
LIBS:simonShield-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 4 4
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
L GND #PWR029
U 1 1 57781CE9
P 3200 3000
F 0 "#PWR029" H 3200 2750 50  0001 C CNN
F 1 "GND" H 3200 2850 50  0000 C CNN
F 2 "" H 3200 3000 50  0000 C CNN
F 3 "" H 3200 3000 50  0000 C CNN
	1    3200 3000
	1    0    0    -1  
$EndComp
$Comp
L Conn_Poncho2P_2x_20x2 XA?
U 2 1 57781CEF
P 3900 3050
AR Path="/57781CEF" Ref="XA?"  Part="2" 
AR Path="/57781B52/57781CEF" Ref="XA1"  Part="2" 
F 0 "XA1" H 4200 3450 60  0000 C CNN
F 1 "Conn_Poncho2P_2x_20x2" H 4250 1350 60  0000 C CNN
F 2 "simonShield:Conn_Poncho_SinBorde" H 3900 3050 60  0001 C CNN
F 3 "" H 3900 3050 60  0000 C CNN
F 4 "952-2121-ND" H 3900 3050 60  0001 C CNN "Digikey#"
	2    3900 3050
	1    0    0    -1  
$EndComp
$Comp
L PWR_JUMPER P1
U 1 1 57781CF6
P 4250 2250
F 0 "P1" H 4250 2400 50  0000 C CNN
F 1 "PWR_JUMPER" H 4250 2100 50  0000 C CNN
F 2 "simonShield:Pin_Header_Straight_2x02" H 4250 1050 50  0001 C CNN
F 3 "" H 4250 1050 50  0000 C CNN
F 4 "952-2121-ND" H 4250 2250 60  0001 C CNN "Digikey#"
	1    4250 2250
	1    0    0    -1  
$EndComp
NoConn ~ 4850 2850
NoConn ~ 4850 2950
NoConn ~ 4850 3050
NoConn ~ 4850 3150
NoConn ~ 4850 3250
NoConn ~ 4850 3350
NoConn ~ 4850 3450
NoConn ~ 3650 4050
NoConn ~ 3650 3950
NoConn ~ 3650 3850
NoConn ~ 3650 3750
NoConn ~ 3650 3650
NoConn ~ 3650 3550
NoConn ~ 3650 3450
NoConn ~ 3650 3350
NoConn ~ 3650 3250
NoConn ~ 3650 3050
NoConn ~ 3650 2950
Text HLabel 4850 3750 2    60   Input ~ 0
BTN_CFG_1
Text HLabel 4850 3850 2    60   Input ~ 0
BTN_CFG_2
Text HLabel 4850 4450 2    60   Input ~ 0
BTN_CFG_3
Text HLabel 4850 4650 2    60   Input ~ 0
BTN_CFG_4
Text HLabel 3650 4350 0    60   Input ~ 0
LED_4
Text HLabel 4850 4350 2    60   Input ~ 0
LED_3
Text HLabel 4850 4050 2    60   Input ~ 0
LED_2
Text HLabel 4850 3550 2    60   Input ~ 0
LED_1
Text HLabel 3650 4450 0    60   Input ~ 0
BTN_LED_4
Text HLabel 4850 4550 2    60   Input ~ 0
BTN_LED_3
Text HLabel 4850 3950 2    60   Input ~ 0
BTN_LED_2
Text HLabel 4850 3650 2    60   Input ~ 0
BTN_LED_1
Text HLabel 3650 3150 0    60   Input ~ 0
PWM
$Comp
L Conn_Poncho2P_2x_20x2 XA1
U 1 1 577946F6
P 7400 3000
F 0 "XA1" H 7700 3400 60  0000 C CNN
F 1 "Conn_Poncho2P_2x_20x2" H 7750 1300 60  0000 C CNN
F 2 "simonShield:Conn_Poncho_SinBorde" H 7400 3000 60  0001 C CNN
F 3 "" H 7400 3000 60  0000 C CNN
F 4 "952-2121-ND" H 7400 3000 60  0001 C CNN "Digikey#"
	1    7400 3000
	1    0    0    -1  
$EndComp
NoConn ~ 7150 2700
NoConn ~ 7150 2800
NoConn ~ 7150 2900
NoConn ~ 7150 3000
NoConn ~ 7150 3100
NoConn ~ 7150 3200
NoConn ~ 7150 3300
NoConn ~ 7150 3400
NoConn ~ 7150 3500
NoConn ~ 7150 3600
NoConn ~ 7150 3700
NoConn ~ 7150 3800
NoConn ~ 7150 3900
NoConn ~ 7150 4000
NoConn ~ 7150 4100
NoConn ~ 7150 4200
NoConn ~ 7150 4300
NoConn ~ 7150 4400
NoConn ~ 7150 4500
NoConn ~ 7150 4600
NoConn ~ 8350 4600
NoConn ~ 8350 4500
NoConn ~ 8350 4400
NoConn ~ 8350 4300
NoConn ~ 8350 4200
NoConn ~ 8350 4100
NoConn ~ 8350 4000
NoConn ~ 8350 3900
NoConn ~ 8350 3800
NoConn ~ 8350 3700
NoConn ~ 8350 3600
NoConn ~ 8350 3500
NoConn ~ 8350 3400
NoConn ~ 8350 3300
NoConn ~ 8350 3200
NoConn ~ 8350 3100
NoConn ~ 8350 3000
NoConn ~ 8350 2900
NoConn ~ 8350 2800
NoConn ~ 8350 2700
Text Notes 5450 1150 0    60   ~ 12
EDU-CIAA NXP CONNECTORS
Text Notes 4100 1300 0    60   ~ 12
P2
Text Notes 7700 1300 0    60   ~ 12
P1
$Comp
L GND #PWR030
U 1 1 578F601E
P 3400 4750
F 0 "#PWR030" H 3400 4500 50  0001 C CNN
F 1 "GND" H 3400 4600 50  0000 C CNN
F 2 "" H 3400 4750 50  0000 C CNN
F 3 "" H 3400 4750 50  0000 C CNN
	1    3400 4750
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR031
U 1 1 57781D17
P 5150 2900
F 0 "#PWR031" H 5150 2650 50  0001 C CNN
F 1 "GND" H 5150 2750 50  0000 C CNN
F 2 "" H 5150 2900 50  0000 C CNN
F 3 "" H 5150 2900 50  0000 C CNN
	1    5150 2900
	1    0    0    -1  
$EndComp
$Comp
L C_100nF C1
U 1 1 57781D10
P 5150 2550
F 0 "C1" H 5150 2650 40  0000 L CNN
F 1 "C_100nF" H 5156 2465 40  0000 L CNN
F 2 "simonShield:C_Disc_D3_P2.5" H 5188 2400 30  0001 C CNN
F 3 "" H 5150 2550 60  0000 C CNN
F 4 "BC1160TR­ND" H 5150 2550 60  0001 C CNN "Digikey#"
	1    5150 2550
	1    0    0    -1  
$EndComp
$Comp
L VCC #PWR032
U 1 1 5798B140
P 4800 2050
F 0 "#PWR032" H 4800 1900 50  0001 C CNN
F 1 "VCC" H 4800 2200 50  0000 C CNN
F 2 "" H 4800 2050 50  0000 C CNN
F 3 "" H 4800 2050 50  0000 C CNN
	1    4800 2050
	1    0    0    -1  
$EndComp
Wire Wire Line
	4850 2750 4900 2750
Wire Wire Line
	4900 2750 4900 2500
Wire Wire Line
	4900 2500 3800 2500
Wire Wire Line
	3800 2500 3800 2300
Wire Wire Line
	3800 2300 4000 2300
Wire Wire Line
	3650 2750 3600 2750
Wire Wire Line
	3600 2750 3600 2200
Wire Wire Line
	3600 2200 4000 2200
Wire Wire Line
	3200 2850 3200 3000
Wire Wire Line
	3650 2850 3200 2850
Wire Notes Line
	1950 1050 1950 5450
Wire Notes Line
	1950 5450 9850 5450
Wire Notes Line
	9850 5450 9850 1050
Wire Notes Line
	9850 1050 1950 1050
Wire Notes Line
	1950 1200 9850 1200
Wire Notes Line
	9850 1350 1950 1350
Wire Notes Line
	6050 1200 6050 5450
Wire Wire Line
	3650 4650 3400 4650
Wire Wire Line
	3400 4550 3400 4750
Wire Wire Line
	3650 4550 3400 4550
Connection ~ 3400 4650
Wire Wire Line
	5150 2750 5150 2900
Wire Wire Line
	4500 2200 4800 2200
Wire Wire Line
	4800 2300 4800 2050
Wire Wire Line
	4500 2300 5150 2300
Connection ~ 4800 2200
Wire Wire Line
	5150 2300 5150 2350
Connection ~ 4800 2300
NoConn ~ 3650 4250
NoConn ~ 3650 4150
NoConn ~ 4850 4150
NoConn ~ 4850 4250
$EndSCHEMATC
