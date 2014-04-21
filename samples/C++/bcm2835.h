// bcm2835.h
//
// C and C++ support for Broadcom BCM 2835 as used in Raspberry Pi
//
// Author: Mike McCauley
// Copyright (C) 2011-2013 Mike McCauley
// $Id: bcm2835.h,v 1.8 2013/02/15 22:06:09 mikem Exp mikem $
//
/// \mainpage C library for Broadcom BCM 2835 as used in Raspberry Pi
///
/// This is a C library for Raspberry Pi (RPi). It provides access to 
/// GPIO and other IO functions on the Broadcom BCM 2835 chip,
/// allowing access to the GPIO pins on the
/// 26 pin IDE plug on the RPi board so you can control and interface with various external devices.
///
/// It provides functions for reading digital inputs and setting digital outputs, using SPI and I2C,
/// and for accessing the system timers.
/// Pin event detection is supported by polling (interrupts are not supported).
///
/// It is C++ compatible, and installs as a header file and non-shared library on 
/// any Linux-based distro (but clearly is no use except on Raspberry Pi or another board with 
/// BCM 2835).
///
/// The version of the package that this documentation refers to can be downloaded 
/// from http://www.airspayce.com/mikem/bcm2835/bcm2835-1.26.tar.gz
/// You can find the latest version at http://www.airspayce.com/mikem/bcm2835
///
/// Several example programs are provided.
///
/// Based on data in http://elinux.org/RPi_Low-level_peripherals and 
/// http://www.raspberrypi.org/wp-content/uploads/2012/02/BCM2835-ARM-Peripherals.pdf
/// and http://www.scribd.com/doc/101830961/GPIO-Pads-Control2
///
/// You can also find online help and discussion at http://groups.google.com/group/bcm2835
/// Please use that group for all questions and discussions on this topic. 
/// Do not contact the author directly, unless it is to discuss commercial licensing.
///
/// Tested on debian6-19-04-2012, 2012-07-15-wheezy-raspbian and Occidentalisv01
/// CAUTION: it has been observed that when detect enables such as bcm2835_gpio_len() 
/// are used and the pin is pulled LOW
/// it can cause temporary hangs on 2012-07-15-wheezy-raspbian and Occidentalisv01.
/// Reason for this is not yet determined, but suspect that an interrupt handler is
/// hitting a hard loop on those OSs.
/// If you must use bcm2835_gpio_len() and friends, make sure you disable the pins with 
/// bcm2835_gpio_cler_len() and friends after use. 
///
/// \par Installation
///
/// This library consists of a single non-shared library and header file, which will be
/// installed in the usual places by make install
///
/// \code
/// # download the latest version of the library, say bcm2835-1.xx.tar.gz, then:
/// tar zxvf bcm2835-1.xx.tar.gz
/// cd bcm2835-1.xx
/// ./configure
/// make
/// sudo make check
/// sudo make install
/// \endcode
///
/// \par Physical Addresses
///
/// The functions bcm2835_peri_read(), bcm2835_peri_write() and bcm2835_peri_set_bits() 
/// are low level peripheral register access functions. They are designed to use
/// physical addresses as described in section 1.2.3 ARM physical addresses
/// of the BCM2835 ARM Peripherals manual. 
/// Physical addresses range from 0x20000000 to 0x20FFFFFF for peripherals. The bus
/// addresses for peripherals are set up to map onto the peripheral bus address range starting at
/// 0x7E000000. Thus a peripheral advertised in the manual at bus address 0x7Ennnnnn is available at
/// physical address 0x20nnnnnn.
///
/// The base address of the various peripheral registers are available with the following
/// externals:
/// bcm2835_gpio
/// bcm2835_pwm
/// bcm2835_clk
/// bcm2835_pads
/// bcm2835_spio0
/// bcm2835_st
/// bcm2835_bsc0
/// bcm2835_bsc1
///
/// \par Pin Numbering
///
/// The GPIO pin numbering as used by RPi is different to and inconsistent with the underlying 
/// BCM 2835 chip pin numbering. http://elinux.org/RPi_BCM2835_GPIOs
/// 
/// RPi has a 26 pin IDE header that provides access to some of the GPIO pins on the BCM 2835,
/// as well as power and ground pins. Not all GPIO pins on the BCM 2835 are available on the 
/// IDE header.
///
/// RPi Version 2 also has a P5 connector with 4 GPIO pins, 5V, 3.3V and Gnd.
///
/// The functions in this library are designed to be passed the BCM 2835 GPIO pin number and _not_ 
/// the RPi pin number. There are symbolic definitions for each of the available pins
/// that you should use for convenience. See \ref RPiGPIOPin.
///
/// \par SPI Pins
/// 
/// The bcm2835_spi_* functions allow you to control the BCM 2835 SPI0 interface, 
/// allowing you to send and received data by SPI (Serial Peripheral Interface).
/// For more information about SPI, see http://en.wikipedia.org/wiki/Serial_Peripheral_Interface_Bus
///
/// When bcm2835_spi_begin() is called it changes the bahaviour of the SPI interface pins from their 
/// default GPIO behaviour in order to support SPI. While SPI is in use, you will not be able 
/// to control the state of the SPI pins through the usual bcm2835_spi_gpio_write().
/// When bcm2835_spi_end() is called, the SPI pins will all revert to inputs, and can then be
/// configured and controled with the usual bcm2835_gpio_* calls.
///
/// The Raspberry Pi GPIO pins used for SPI are:
/// 
/// - P1-19 (MOSI)
/// - P1-21 (MISO) 
/// - P1-23 (CLK) 
/// - P1-24 (CE0) 
/// - P1-26 (CE1)
///
/// \par I2C Pins
///
/// The bcm2835_i2c_* functions allow you to control the BCM 2835 BSC interface,
/// allowing you to send and received data by I2C ("eye-squared cee"; generically referred to as "two-wire interface") .
/// For more information about I?C, see http://en.wikipedia.org/wiki/I%C2%B2C
///
/// The Raspberry Pi V2 GPIO pins used for I2C are:
///
/// - P1-03 (SDA)
/// - P1-05 (SLC)
///
/// \par Real Time performance constraints
///
/// The bcm2835 is a library for user programs (i.e. they run in 'userland'). 
/// Such programs are not part of the kernel and are usually
/// subject to paging and swapping by the kernel while it does other things besides running your program. 
/// This means that you should not expect to get real-time performance or 
/// real-time timing constraints from such programs. In particular, there is no guarantee that the 
/// bcm2835_delay() and bcm2835_delayMicroseconds() will return after exactly the time requested. 
/// In fact, depending on other activity on the host, IO etc, you might get significantly longer delay times
/// than the one you asked for. So please dont expect to get exactly the time delay you request.
///
/// Arjan reports that you can prevent swapping on Linux with the following code fragment:
///
/// \code
///  struct sched_param sp;
///  memset(&sp, 0, sizeof(sp));
///  sp.sched_priority = sched_get_priority_max(SCHED_FIFO);
///  sched_setscheduler(0, SCHED_FIFO, &sp);
///  mlockall(MCL_CURRENT | MCL_FUTURE);
/// \endcode
///
/// \par Open Source Licensing GPL V2
///
/// This is the appropriate option if you want to share the source code of your
/// application with everyone you distribute it to, and you also want to give them
/// the right to share who uses it. If you wish to use this software under Open
/// Source Licensing, you must contribute all your source code to the open source
/// community in accordance with the GPL Version 2 when your application is
/// distributed. See http://www.gnu.org/copyleft/gpl.html and COPYING
///
/// \par Acknowledgements
///
/// Some of this code has been inspired by Dom and Gert.
/// The I2C code has been inspired by Alan Barr.
/// 
/// \par Revision History
///
/// \version 1.0 Initial release
/// \version 1.1 Minor bug fixes
/// \version 1.2 Added support for SPI
/// \version 1.3 Added bcm2835_spi_transfern()
/// \version 1.4 Fixed a problem that prevented SPI CE1 being used. Reported by David Robinson.
/// \version 1.5 Added bcm2835_close() to deinit the library. Suggested by C?sar Ortiz
/// \version 1.6 Document testing on 2012-07-15-wheezy-raspbian and Occidentalisv01
///              Functions bcm2835_gpio_ren(), bcm2835_gpio_fen(), bcm2835_gpio_hen()
///               bcm2835_gpio_len(), bcm2835_gpio_aren() and bcm2835_gpio_afen() now 
///               changes only the pin specified. Other pins that were already previously
///               enabled stay enabled.
///              Added  bcm2835_gpio_clr_ren(), bcm2835_gpio_clr_fen(), bcm2835_gpio_clr_hen()
///                bcm2835_gpio_clr_len(), bcm2835_gpio_clr_aren(), bcm2835_gpio_clr_afen() 
///                to clear the enable for individual pins, suggested by Andreas Sundstrom.
/// \version 1.7 Added bcm2835_spi_transfernb to support different buffers for read and write.
/// \version 1.8 Improvements to read barrier, as suggested by maddin.
/// \version 1.9 Improvements contributed by mikew: 
///              I noticed that it was mallocing memory for the mmaps on /dev/mem.
///              It's not necessary to do that, you can just mmap the file directly,
///              so I've removed the mallocs (and frees).
///              I've also modified delayMicroseconds() to use nanosleep() for long waits,
///              and a busy wait on a high resolution timer for the rest. This is because
///              I've found that calling nanosleep() takes at least 100-200 us.
///              You need to link using '-lrt' using this version.
///              I've added some unsigned casts to the debug prints to silence compiler
///              warnings I was getting, fixed some typos, and changed the value of
///              BCM2835_PAD_HYSTERESIS_ENABLED to 0x08 as per Gert van Loo's doc at
///              http://www.scribd.com/doc/101830961/GPIO-Pads-Control2
///              Also added a define for the passwrd value that Gert says is needed to
///              change pad control settings.
/// \version 1.10 Changed the names of the delay functions to bcm2835_delay() 
///              and bcm2835_delayMicroseconds() to prevent collisions with wiringPi.
///              Macros to map delay()-> bcm2835_delay() and
///              Macros to map delayMicroseconds()-> bcm2835_delayMicroseconds(), which
///              can be disabled by defining BCM2835_NO_DELAY_COMPATIBILITY
/// \version 1.11 Fixed incorrect link to download file
/// \version 1.12 New GPIO pin definitions for RPi version 2 (which has a different GPIO mapping)             
/// \version 1.13 New GPIO pin definitions for RPi version 2 plug P5
///               Hardware base pointers are now available (after initialisation) externally as bcm2835_gpio
///               bcm2835_pwm bcm2835_clk bcm2835_pads bcm2835_spi0.
/// \version 1.14 Now compiles even if CLOCK_MONOTONIC_RAW is not available, uses CLOCK_MONOTONIC instead.
///               Fixed errors in documentation of SPI divider frequencies based on 250MHz clock. 
///               Reported by Ben Simpson.
/// \version 1.15 Added bcm2835_close() to end of examples as suggested by Mark Wolfe.
/// \version 1.16 Added bcm2835_gpio_set_multi, bcm2835_gpio_clr_multi and bcm2835_gpio_write_multi
///               to allow a mask of pins to be set all at once. Requested by Sebastian Loncar.
/// \version 1.17  Added bcm2835_gpio_write_mask. Requested by Sebastian Loncar.
/// \version 1.18 Added bcm2835_i2c_* functions. Changes to bcm2835_delayMicroseconds: 
///               now uses the RPi system timer counter, instead of clock_gettime, for improved accuracy. 
///               No need to link with -lrt now. Contributed by Arjan van Vught.
/// \version 1.19 Removed inlines added by previous patch since they don't seem to work everywhere. 
///               Reported by olly.
/// \version 1.20 Patch from Mark Dootson to close /dev/mem after access to the peripherals has been granted.
/// \version 1.21 delayMicroseconds is now not susceptible to 32 bit timer overruns. 
///               Patch courtesy Jeremy Mortis.
/// \version 1.22 Fixed incorrect definition of BCM2835_GPFEN0 which broke the ability to set 
///               falling edge events. Reported by Mark Dootson.
/// \version 1.23 Added bcm2835_i2c_set_baudrate and bcm2835_i2c_read_register_rs. 
///               Improvements to bcm2835_i2c_read and bcm2835_i2c_write functions
///               to fix ocasional reads not completing. Patched by Mark Dootson.
/// \version 1.24 Mark Dootson p[atched a problem with his previously submitted code
///               under high load from other processes. 
/// \version 1.25 Updated author and distribution location details to airspayce.com
/// \version 1.26 Added missing unmapmem for pads in bcm2835_close to prevent a memory leak. 
///               Reported by Hartmut Henkel.
/// \author  Mike McCauley (mikem@airspayce.com) DO NOT CONTACT THE AUTHOR DIRECTLY: USE THE LISTS



// Defines for BCM2835
#ifndef BCM2835_H
#define BCM2835_H

#include <stdint.h>

/// \defgroup constants Constants for passing to and from library functions
/// The values here are designed to be passed to various functions in the bcm2835 library.
/// @{


/// This means pin HIGH, true, 3.3volts on a pin.
#define HIGH 0x1
/// This means pin LOW, false, 0volts on a pin.
#define LOW  0x0

/// Speed of the core clock core_clk
#define BCM2835_CORE_CLK_HZ				250000000	///< 250 MHz

// Physical addresses for various peripheral register sets
/// Base Physical Address of the BCM 2835 peripheral registers
#define BCM2835_PERI_BASE               0x20000000
/// Base Physical Address of the System Timer registers
#define BCM2835_ST_BASE			(BCM2835_PERI_BASE + 0x3000)
/// Base Physical Address of the Pads registers
#define BCM2835_GPIO_PADS               (BCM2835_PERI_BASE + 0x100000)
/// Base Physical Address of the Clock/timer registers
#define BCM2835_CLOCK_BASE              (BCM2835_PERI_BASE + 0x101000)
/// Base Physical Address of the GPIO registers
#define BCM2835_GPIO_BASE               (BCM2835_PERI_BASE + 0x200000)
/// Base Physical Address of the SPI0 registers
#define BCM2835_SPI0_BASE               (BCM2835_PERI_BASE + 0x204000)
/// Base Physical Address of the BSC0 registers
#define BCM2835_BSC0_BASE 		(BCM2835_PERI_BASE + 0x205000)
/// Base Physical Address of the PWM registers
#define BCM2835_GPIO_PWM                (BCM2835_PERI_BASE + 0x20C000)
 /// Base Physical Address of the BSC1 registers
#define BCM2835_BSC1_BASE		(BCM2835_PERI_BASE + 0x804000)


/// Base of the ST (System Timer) registers.
/// Available after bcm2835_init has been called
extern volatile uint32_t *bcm2835_st;

/// Base of the GPIO registers.
/// Available after bcm2835_init has been called
extern volatile uint32_t *bcm2835_gpio;

/// Base of the PWM registers.
/// Available after bcm2835_init has been called
extern volatile uint32_t *bcm2835_pwm;

/// Base of the CLK registers.
/// Available after bcm2835_init has been called
extern volatile uint32_t *bcm2835_clk;

/// Base of the PADS registers.
/// Available after bcm2835_init has been called
extern volatile uint32_t *bcm2835_pads;

/// Base of the SPI0 registers.
/// Available after bcm2835_init has been called
extern volatile uint32_t *bcm2835_spi0;

/// Base of the BSC0 registers.
/// Available after bcm2835_init has been called
extern volatile uint32_t *bcm2835_bsc0;

/// Base of the BSC1 registers.
/// Available after bcm2835_init has been called
extern volatile uint32_t *bcm2835_bsc1;

/// Size of memory page on RPi
#define BCM2835_PAGE_SIZE               (4*1024)
/// Size of memory block on RPi
#define BCM2835_BLOCK_SIZE              (4*1024)


// Defines for GPIO
// The BCM2835 has 54 GPIO pins.
//      BCM2835 data sheet, Page 90 onwards.
/// GPIO register offsets from BCM2835_GPIO_BASE. Offsets into the GPIO Peripheral block in bytes per 6.1 Register View
#define BCM2835_GPFSEL0                      0x0000 ///< GPIO Function Select 0
#define BCM2835_GPFSEL1                      0x0004 ///< GPIO Function Select 1
#define BCM2835_GPFSEL2                      0x0008 ///< GPIO Function Select 2
#define BCM2835_GPFSEL3                      0x000c ///< GPIO Function Select 3
#define BCM2835_GPFSEL4                      0x0010 ///< GPIO Function Select 4
#define BCM2835_GPFSEL5                      0x0014 ///< GPIO Function Select 5
#define BCM2835_GPSET0                       0x001c ///< GPIO Pin Output Set 0
#define BCM2835_GPSET1                       0x0020 ///< GPIO Pin Output Set 1
#define BCM2835_GPCLR0                       0x0028 ///< GPIO Pin Output Clear 0
#define BCM2835_GPCLR1                       0x002c ///< GPIO Pin Output Clear 1
#define BCM2835_GPLEV0                       0x0034 ///< GPIO Pin Level 0
#define BCM2835_GPLEV1                       0x0038 ///< GPIO Pin Level 1
#define BCM2835_GPEDS0                       0x0040 ///< GPIO Pin Event Detect Status 0
#define BCM2835_GPEDS1                       0x0044 ///< GPIO Pin Event Detect Status 1
#define BCM2835_GPREN0                       0x004c ///< GPIO Pin Rising Edge Detect Enable 0
#define BCM2835_GPREN1                       0x0050 ///< GPIO Pin Rising Edge Detect Enable 1
#define BCM2835_GPFEN0                       0x0058 ///< GPIO Pin Falling Edge Detect Enable 0
#define BCM2835_GPFEN1                       0x005c ///< GPIO Pin Falling Edge Detect Enable 1
#define BCM2835_GPHEN0                       0x0064 ///< GPIO Pin High Detect Enable 0
#define BCM2835_GPHEN1                       0x0068 ///< GPIO Pin High Detect Enable 1
#define BCM2835_GPLEN0                       0x0070 ///< GPIO Pin Low Detect Enable 0
#define BCM2835_GPLEN1                       0x0074 ///< GPIO Pin Low Detect Enable 1
#define BCM2835_GPAREN0                      0x007c ///< GPIO Pin Async. Rising Edge Detect 0
#define BCM2835_GPAREN1                      0x0080 ///< GPIO Pin Async. Rising Edge Detect 1
#define BCM2835_GPAFEN0                      0x0088 ///< GPIO Pin Async. Falling Edge Detect 0
#define BCM2835_GPAFEN1                      0x008c ///< GPIO Pin Async. Falling Edge Detect 1
#define BCM2835_GPPUD                        0x0094 ///< GPIO Pin Pull-up/down Enable
#define BCM2835_GPPUDCLK0                    0x0098 ///< GPIO Pin Pull-up/down Enable Clock 0
#define BCM2835_GPPUDCLK1                    0x009c ///< GPIO Pin Pull-up/down Enable Clock 1

/// \brief bcm2835PortFunction
/// Port function select modes for bcm2835_gpio_fsel()
typedef enum
{
    BCM2835_GPIO_FSEL_INPT  = 0b000,   ///< Input
    BCM2835_GPIO_FSEL_OUTP  = 0b001,   ///< Output
    BCM2835_GPIO_FSEL_ALT0  = 0b100,   ///< Alternate function 0
    BCM2835_GPIO_FSEL_ALT1  = 0b101,   ///< Alternate function 1
    BCM2835_GPIO_FSEL_ALT2  = 0b110,   ///< Alternate function 2
    BCM2835_GPIO_FSEL_ALT3  = 0b111,   ///< Alternate function 3
    BCM2835_GPIO_FSEL_ALT4  = 0b011,   ///< Alternate function 4
    BCM2835_GPIO_FSEL_ALT5  = 0b010,   ///< Alternate function 5
    BCM2835_GPIO_FSEL_MASK  = 0b111    ///< Function select bits mask
} bcm2835FunctionSelect;

/// \brief bcm2835PUDControl
/// Pullup/Pulldown defines for bcm2835_gpio_pud()
typedef enum
{
    BCM2835_GPIO_PUD_OFF     = 0b00,   ///< Off ? disable pull-up/down
    BCM2835_GPIO_PUD_DOWN    = 0b01,   ///< Enable Pull Down control
    BCM2835_GPIO_PUD_UP      = 0b10    ///< Enable Pull Up control
} bcm2835PUDControl;

/// Pad control register offsets from BCM2835_GPIO_PADS
#define BCM2835_PADS_GPIO_0_27               0x002c ///< Pad control register for pads 0 to 27
#define BCM2835_PADS_GPIO_28_45              0x0030 ///< Pad control register for pads 28 to 45
#define BCM2835_PADS_GPIO_46_53              0x0034 ///< Pad control register for pads 46 to 53

/// Pad Control masks
#define BCM2835_PAD_PASSWRD                  (0x5A << 24)  ///< Password to enable setting pad mask
#define BCM2835_PAD_SLEW_RATE_UNLIMITED      0x10 ///< Slew rate unlimited
#define BCM2835_PAD_HYSTERESIS_ENABLED       0x08 ///< Hysteresis enabled
#define BCM2835_PAD_DRIVE_2mA                0x00 ///< 2mA drive current
#define BCM2835_PAD_DRIVE_4mA                0x01 ///< 4mA drive current
#define BCM2835_PAD_DRIVE_6mA                0x02 ///< 6mA drive current
#define BCM2835_PAD_DRIVE_8mA                0x03 ///< 8mA drive current
#define BCM2835_PAD_DRIVE_10mA               0x04 ///< 10mA drive current
#define BCM2835_PAD_DRIVE_12mA               0x05 ///< 12mA drive current
#define BCM2835_PAD_DRIVE_14mA               0x06 ///< 14mA drive current
#define BCM2835_PAD_DRIVE_16mA               0x07 ///< 16mA drive current

/// \brief bcm2835PadGroup
/// Pad group specification for bcm2835_gpio_pad()
typedef enum
{
    BCM2835_PAD_GROUP_GPIO_0_27         = 0, ///< Pad group for GPIO pads 0 to 27
    BCM2835_PAD_GROUP_GPIO_28_45        = 1, ///< Pad group for GPIO pads 28 to 45
    BCM2835_PAD_GROUP_GPIO_46_53        = 2  ///< Pad group for GPIO pads 46 to 53
} bcm2835PadGroup;

/// \brief GPIO Pin Numbers
///
/// Here we define Raspberry Pin GPIO pins on P1 in terms of the underlying BCM GPIO pin numbers.
/// These can be passed as a pin number to any function requiring a pin.
/// Not all pins on the RPi 26 bin IDE plug are connected to GPIO pins
/// and some can adopt an alternate function.
/// RPi version 2 has some slightly different pinouts, and these are values RPI_V2_*.
/// At bootup, pins 8 and 10 are set to UART0_TXD, UART0_RXD (ie the alt0 function) respectively
/// When SPI0 is in use (ie after bcm2835_spi_begin()), pins 19, 21, 23, 24, 26 are dedicated to SPI
/// and cant be controlled independently
typedef enum
{
    RPI_GPIO_P1_03        =  0,  ///< Version 1, Pin P1-03
    RPI_GPIO_P1_05        =  1,  ///< Version 1, Pin P1-05
    RPI_GPIO_P1_07        =  4,  ///< Version 1, Pin P1-07
    RPI_GPIO_P1_08        = 14,  ///< Version 1, Pin P1-08, defaults to alt function 0 UART0_TXD
    RPI_GPIO_P1_10        = 15,  ///< Version 1, Pin P1-10, defaults to alt function 0 UART0_RXD
    RPI_GPIO_P1_11        = 17,  ///< Version 1, Pin P1-11
    RPI_GPIO_P1_12        = 18,  ///< Version 1, Pin P1-12
    RPI_GPIO_P1_13        = 21,  ///< Version 1, Pin P1-13
    RPI_GPIO_P1_15        = 22,  ///< Version 1, Pin P1-15
    RPI_GPIO_P1_16        = 23,  ///< Version 1, Pin P1-16
    RPI_GPIO_P1_18        = 24,  ///< Version 1, Pin P1-18
    RPI_GPIO_P1_19        = 10,  ///< Version 1, Pin P1-19, MOSI when SPI0 in use
    RPI_GPIO_P1_21        =  9,  ///< Version 1, Pin P1-21, MISO when SPI0 in use
    RPI_GPIO_P1_22        = 25,  ///< Version 1, Pin P1-22
    RPI_GPIO_P1_23        = 11,  ///< Version 1, Pin P1-23, CLK when SPI0 in use
    RPI_GPIO_P1_24        =  8,  ///< Version 1, Pin P1-24, CE0 when SPI0 in use
    RPI_GPIO_P1_26        =  7,  ///< Version 1, Pin P1-26, CE1 when SPI0 in use

    // RPi Version 2
    RPI_V2_GPIO_P1_03     =  2,  ///< Version 2, Pin P1-03
    RPI_V2_GPIO_P1_05     =  3,  ///< Version 2, Pin P1-05
    RPI_V2_GPIO_P1_07     =  4,  ///< Version 2, Pin P1-07
    RPI_V2_GPIO_P1_08     = 14,  ///< Version 2, Pin P1-08, defaults to alt function 0 UART0_TXD
    RPI_V2_GPIO_P1_10     = 15,  ///< Version 2, Pin P1-10, defaults to alt function 0 UART0_RXD
    RPI_V2_GPIO_P1_11     = 17,  ///< Version 2, Pin P1-11
    RPI_V2_GPIO_P1_12     = 18,  ///< Version 2, Pin P1-12
    RPI_V2_GPIO_P1_13     = 27,  ///< Version 2, Pin P1-13
    RPI_V2_GPIO_P1_15     = 22,  ///< Version 2, Pin P1-15
    RPI_V2_GPIO_P1_16     = 23,  ///< Version 2, Pin P1-16
    RPI_V2_GPIO_P1_18     = 24,  ///< Version 2, Pin P1-18
    RPI_V2_GPIO_P1_19     = 10,  ///< Version 2, Pin P1-19, MOSI when SPI0 in use
    RPI_V2_GPIO_P1_21     =  9,  ///< Version 2, Pin P1-21, MISO when SPI0 in use
    RPI_V2_GPIO_P1_22     = 25,  ///< Version 2, Pin P1-22
    RPI_V2_GPIO_P1_23     = 11,  ///< Version 2, Pin P1-23, CLK when SPI0 in use
    RPI_V2_GPIO_P1_24     =  8,  ///< Version 2, Pin P1-24, CE0 when SPI0 in use
    RPI_V2_GPIO_P1_26     =  7,  ///< Version 2, Pin P1-26, CE1 when SPI0 in use

    // RPi Version 2, new plug P5
    RPI_V2_GPIO_P5_03     = 28,  ///< Version 2, Pin P5-03
    RPI_V2_GPIO_P5_04     = 29,  ///< Version 2, Pin P5-04
    RPI_V2_GPIO_P5_05     = 30,  ///< Version 2, Pin P5-05
    RPI_V2_GPIO_P5_06     = 31,  ///< Version 2, Pin P5-06

} RPiGPIOPin;

// Defines for SPI
// GPIO register offsets from BCM2835_SPI0_BASE. 
// Offsets into the SPI Peripheral block in bytes per 10.5 SPI Register Map
#define BCM2835_SPI0_CS                      0x0000 ///< SPI Master Control and Status
#define BCM2835_SPI0_FIFO                    0x0004 ///< SPI Master TX and RX FIFOs
#define BCM2835_SPI0_CLK                     0x0008 ///< SPI Master Clock Divider
#define BCM2835_SPI0_DLEN                    0x000c ///< SPI Master Data Length
#define BCM2835_SPI0_LTOH                    0x0010 ///< SPI LOSSI mode TOH
#define BCM2835_SPI0_DC                      0x0014 ///< SPI DMA DREQ Controls

// Register masks for SPI0_CS
#define BCM2835_SPI0_CS_LEN_LONG             0x02000000 ///< Enable Long data word in Lossi mode if DMA_LEN is set
#define BCM2835_SPI0_CS_DMA_LEN              0x01000000 ///< Enable DMA mode in Lossi mode
#define BCM2835_SPI0_CS_CSPOL2               0x00800000 ///< Chip Select 2 Polarity
#define BCM2835_SPI0_CS_CSPOL1               0x00400000 ///< Chip Select 1 Polarity
#define BCM2835_SPI0_CS_CSPOL0               0x00200000 ///< Chip Select 0 Polarity
#define BCM2835_SPI0_CS_RXF                  0x00100000 ///< RXF - RX FIFO Full
#define BCM2835_SPI0_CS_RXR                  0x00080000 ///< RXR RX FIFO needs Reading ( full)
#define BCM2835_SPI0_CS_TXD                  0x00040000 ///< TXD TX FIFO can accept Data
#define BCM2835_SPI0_CS_RXD                  0x00020000 ///< RXD RX FIFO contains Data
#define BCM2835_SPI0_CS_DONE                 0x00010000 ///< Done transfer Done
#define BCM2835_SPI0_CS_TE_EN                0x00008000 ///< Unused
#define BCM2835_SPI0_CS_LMONO                0x00004000 ///< Unused
#define BCM2835_SPI0_CS_LEN                  0x00002000 ///< LEN LoSSI enable
#define BCM2835_SPI0_CS_REN                  0x00001000 ///< REN Read Enable
#define BCM2835_SPI0_CS_ADCS                 0x00000800 ///< ADCS Automatically Deassert Chip Select
#define BCM2835_SPI0_CS_INTR                 0x00000400 ///< INTR Interrupt on RXR
#define BCM2835_SPI0_CS_INTD                 0x00000200 ///< INTD Interrupt on Done
#define BCM2835_SPI0_CS_DMAEN                0x00000100 ///< DMAEN DMA Enable
#define BCM2835_SPI0_CS_TA                   0x00000080 ///< Transfer Active
#define BCM2835_SPI0_CS_CSPOL                0x00000040 ///< Chip Select Polarity
#define BCM2835_SPI0_CS_CLEAR                0x00000030 ///< Clear FIFO Clear RX and TX
#define BCM2835_SPI0_CS_CLEAR_RX             0x00000020 ///< Clear FIFO Clear RX 
#define BCM2835_SPI0_CS_CLEAR_TX             0x00000010 ///< Clear FIFO Clear TX 
#define BCM2835_SPI0_CS_CPOL                 0x00000008 ///< Clock Polarity
#define BCM2835_SPI0_CS_CPHA                 0x00000004 ///< Clock Phase
#define BCM2835_SPI0_CS_CS                   0x00000003 ///< Chip Select

/// \brief bcm2835SPIBitOrder SPI Bit order
/// Specifies the SPI data bit ordering for bcm2835_spi_setBitOrder()
typedef enum
{
    BCM2835_SPI_BIT_ORDER_LSBFIRST = 0,  ///< LSB First
    BCM2835_SPI_BIT_ORDER_MSBFIRST = 1   ///< MSB First
}bcm2835SPIBitOrder;

/// \brief SPI Data mode
/// Specify the SPI data mode to be passed to bcm2835_spi_setDataMode()
typedef enum
{
    BCM2835_SPI_MODE0 = 0,  ///< CPOL = 0, CPHA = 0
    BCM2835_SPI_MODE1 = 1,  ///< CPOL = 0, CPHA = 1
    BCM2835_SPI_MODE2 = 2,  ///< CPOL = 1, CPHA = 0
    BCM2835_SPI_MODE3 = 3,  ///< CPOL = 1, CPHA = 1
}bcm2835SPIMode;

/// \brief bcm2835SPIChipSelect
/// Specify the SPI chip select pin(s)
typedef enum
{
    BCM2835_SPI_CS0 = 0,     ///< Chip Select 0
    BCM2835_SPI_CS1 = 1,     ///< Chip Select 1
    BCM2835_SPI_CS2 = 2,     ///< Chip Select 2 (ie pins CS1 and CS2 are asserted)
    BCM2835_SPI_CS_NONE = 3, ///< No CS, control it yourself
} bcm2835SPIChipSelect;

/// \brief bcm2835SPIClockDivider
/// Specifies the divider used to generate the SPI clock from the system clock.
/// Figures below give the divider, clock period and clock frequency.
/// Clock divided is based on nominal base clock rate of 250MHz
/// It is reported that (contrary to the documentation) any even divider may used.
/// The frequencies shown for each divider have been confirmed by measurement
typedef enum
{
    BCM2835_SPI_CLOCK_DIVIDER_65536 = 0,       ///< 65536 = 262.144us = 3.814697260kHz
    BCM2835_SPI_CLOCK_DIVIDER_32768 = 32768,   ///< 32768 = 131.072us = 7.629394531kHz
    BCM2835_SPI_CLOCK_DIVIDER_16384 = 16384,   ///< 16384 = 65.536us = 15.25878906kHz
    BCM2835_SPI_CLOCK_DIVIDER_8192  = 8192,    ///< 8192 = 32.768us = 30/51757813kHz
    BCM2835_SPI_CLOCK_DIVIDER_4096  = 4096,    ///< 4096 = 16.384us = 61.03515625kHz
    BCM2835_SPI_CLOCK_DIVIDER_2048  = 2048,    ///< 2048 = 8.192us = 122.0703125kHz
    BCM2835_SPI_CLOCK_DIVIDER_1024  = 1024,    ///< 1024 = 4.096us = 244.140625kHz
    BCM2835_SPI_CLOCK_DIVIDER_512   = 512,     ///< 512 = 2.048us = 488.28125kHz
    BCM2835_SPI_CLOCK_DIVIDER_256   = 256,     ///< 256 = 1.024us = 976.5625MHz
    BCM2835_SPI_CLOCK_DIVIDER_128   = 128,     ///< 128 = 512ns = = 1.953125MHz
    BCM2835_SPI_CLOCK_DIVIDER_64    = 64,      ///< 64 = 256ns = 3.90625MHz
    BCM2835_SPI_CLOCK_DIVIDER_32    = 32,      ///< 32 = 128ns = 7.8125MHz
    BCM2835_SPI_CLOCK_DIVIDER_16    = 16,      ///< 16 = 64ns = 15.625MHz
    BCM2835_SPI_CLOCK_DIVIDER_8     = 8,       ///< 8 = 32ns = 31.25MHz
    BCM2835_SPI_CLOCK_DIVIDER_4     = 4,       ///< 4 = 16ns = 62.5MHz
    BCM2835_SPI_CLOCK_DIVIDER_2     = 2,       ///< 2 = 8ns = 125MHz, fastest you can get
    BCM2835_SPI_CLOCK_DIVIDER_1     = 1,       ///< 0 = 262.144us = 3.814697260kHz, same as 0/65536
} bcm2835SPIClockDivider;

// Defines for I2C
// GPIO register offsets from BCM2835_BSC*_BASE.
// Offsets into the BSC Peripheral block in bytes per 3.1 BSC Register Map
#define BCM2835_BSC_C 							0x0000 ///< BSC Master Control
#define BCM2835_BSC_S 							0x0004 ///< BSC Master Status
#define BCM2835_BSC_DLEN						0x0008 ///< BSC Master Data Length
#define BCM2835_BSC_A 							0x000c ///< BSC Master Slave Address
#define BCM2835_BSC_FIFO						0x0010 ///< BSC Master Data FIFO
#define BCM2835_BSC_DIV							0x0014 ///< BSC Master Clock Divider
#define BCM2835_BSC_DEL							0x0018 ///< BSC Master Data Delay
#define BCM2835_BSC_CLKT						0x001c ///< BSC Master Clock Stretch Timeout

// Register masks for BSC_C
#define BCM2835_BSC_C_I2CEN 					0x00008000 ///< I2C Enable, 0 = disabled, 1 = enabled
#define BCM2835_BSC_C_INTR 						0x00000400 ///< Interrupt on RX
#define BCM2835_BSC_C_INTT 						0x00000200 ///< Interrupt on TX
#define BCM2835_BSC_C_INTD 						0x00000100 ///< Interrupt on DONE
#define BCM2835_BSC_C_ST 						0x00000080 ///< Start transfer, 1 = Start a new transfer
#define BCM2835_BSC_C_CLEAR_1 					0x00000020 ///< Clear FIFO Clear
#define BCM2835_BSC_C_CLEAR_2 					0x00000010 ///< Clear FIFO Clear
#define BCM2835_BSC_C_READ 						0x00000001 ///<	Read transfer

// Register masks for BSC_S
#define BCM2835_BSC_S_CLKT 						0x00000200 ///< Clock stretch timeout
#define BCM2835_BSC_S_ERR 						0x00000100 ///< ACK error
#define BCM2835_BSC_S_RXF 						0x00000080 ///< RXF FIFO full, 0 = FIFO is not full, 1 = FIFO is full
#define BCM2835_BSC_S_TXE 						0x00000040 ///< TXE FIFO full, 0 = FIFO is not full, 1 = FIFO is full
#define BCM2835_BSC_S_RXD 						0x00000020 ///< RXD FIFO contains data
#define BCM2835_BSC_S_TXD 						0x00000010 ///< TXD FIFO can accept data
#define BCM2835_BSC_S_RXR 						0x00000008 ///< RXR FIFO needs reading (full)
#define BCM2835_BSC_S_TXW 						0x00000004 ///< TXW FIFO needs writing (full)
#define BCM2835_BSC_S_DONE 						0x00000002 ///< Transfer DONE
#define BCM2835_BSC_S_TA 						0x00000001 ///< Transfer Active

#define BCM2835_BSC_FIFO_SIZE   				16 ///< BSC FIFO size

/// \brief bcm2835I2CClockDivider
/// Specifies the divider used to generate the I2C clock from the system clock.
/// Clock divided is based on nominal base clock rate of 250MHz
typedef enum
{
    BCM2835_I2C_CLOCK_DIVIDER_2500   = 2500,      ///< 2500 = 10us = 100 kHz
    BCM2835_I2C_CLOCK_DIVIDER_626    = 626,       ///< 622 = 2.504us = 399.3610 kHz
    BCM2835_I2C_CLOCK_DIVIDER_150    = 150,       ///< 150 = 60ns = 1.666 MHz (default at reset)
    BCM2835_I2C_CLOCK_DIVIDER_148    = 148,       ///< 148 = 59ns = 1.689 MHz
} bcm2835I2CClockDivider;

/// \brief bcm2835I2CReasonCodes
/// Specifies the reason codes for the bcm2835_i2c_write and bcm2835_i2c_read functions.
typedef enum
{
    BCM2835_I2C_REASON_OK   		 = 0x00,      ///< Success
    BCM2835_I2C_REASON_ERROR_NACK    = 0x01,      ///< Received a NACK
    BCM2835_I2C_REASON_ERROR_CLKT    = 0x02,      ///< Received Clock Stretch Timeout
    BCM2835_I2C_REASON_ERROR_DATA    = 0x04,      ///< Not all data is sent / received
} bcm2835I2CReasonCodes;

// Defines for ST
// GPIO register offsets from BCM2835_ST_BASE.
// Offsets into the ST Peripheral block in bytes per 12.1 System Timer Registers
// The System Timer peripheral provides four 32-bit timer channels and a single 64-bit free running counter.
// BCM2835_ST_CLO is the System Timer Counter Lower bits register.
// The system timer free-running counter lower register is a read-only register that returns the current value
// of the lower 32-bits of the free running counter.
// BCM2835_ST_CHI is the System Timer Counter Upper bits register.
// The system timer free-running counter upper register is a read-only register that returns the current value
// of the upper 32-bits of the free running counter.
#define BCM2835_ST_CS 							0x0000 ///< System Timer Control/Status
#define BCM2835_ST_CLO 							0x0004 ///< System Timer Counter Lower 32 bits
#define BCM2835_ST_CHI 							0x0008 ///< System Timer Counter Upper 32 bits

/// @}


// Defines for PWM
#define BCM2835_PWM_CONTROL 0
#define BCM2835_PWM_STATUS  1
#define BCM2835_PWM0_RANGE  4
#define BCM2835_PWM0_DATA   5
#define BCM2835_PWM1_RANGE  8
#define BCM2835_PWM1_DATA   9

#define BCM2835_PWMCLK_CNTL     40
#define BCM2835_PWMCLK_DIV      41

#define BCM2835_PWM1_MS_MODE    0x8000  /// Run in MS mode
#define BCM2835_PWM1_USEFIFO    0x2000  /// Data from FIFO
#define BCM2835_PWM1_REVPOLAR   0x1000  /// Reverse polarity
#define BCM2835_PWM1_OFFSTATE   0x0800  /// Ouput Off state
#define BCM2835_PWM1_REPEATFF   0x0400  /// Repeat last value if FIFO empty
#define BCM2835_PWM1_SERIAL     0x0200  /// Run in serial mode
#define BCM2835_PWM1_ENABLE     0x0100  /// Channel Enable

#define BCM2835_PWM0_MS_MODE    0x0080  /// Run in MS mode
#define BCM2835_PWM0_USEFIFO    0x0020  /// Data from FIFO
#define BCM2835_PWM0_REVPOLAR   0x0010  /// Reverse polarity
#define BCM2835_PWM0_OFFSTATE   0x0008  /// Ouput Off state
#define BCM2835_PWM0_REPEATFF   0x0004  /// Repeat last value if FIFO empty
#define BCM2835_PWM0_SERIAL     0x0002  /// Run in serial mode
#define BCM2835_PWM0_ENABLE     0x0001  /// Channel Enable

// Historical name compatibility
#ifndef BCM2835_NO_DELAY_COMPATIBILITY
#define delay(x) bcm2835_delay(x)
#define delayMicroseconds(x) bcm2835_delayMicroseconds(x)
#endif

#ifdef __cplusplus
extern "C" {
#endif

    /// \defgroup init Library initialisation and management
    /// These functions allow you to intialise and control the bcm2835 library
    /// @{

    /// Initialise the library by opening /dev/mem and getting pointers to the 
    /// internal memory for BCM 2835 device registers. You must call this (successfully)
    /// before calling any other 
    /// functions in this library (except bcm2835_set_debug). 
    /// If bcm2835_init() fails by returning 0, 
    /// calling any other function may result in crashes or other failures.
    /// Prints messages to stderr in case of errors.
    /// \return 1 if successful else 0
    extern int bcm2835_init(void);

    /// Close the library, deallocating any allocated memory and closing /dev/mem
    /// \return 1 if successful else 0
    extern int bcm2835_close(void);

    /// Sets the debug level of the library.
    /// A value of 1 prevents mapping to /dev/mem, and makes the library print out
    /// what it would do, rather than accessing the GPIO registers.
    /// A value of 0, the default, causes normal operation.
    /// Call this before calling bcm2835_init();
    /// \param[in] debug The new debug level. 1 means debug
    extern void  bcm2835_set_debug(uint8_t debug);

    /// @} // end of init

    /// \defgroup lowlevel Low level register access
    /// These functions provide low level register access, and should not generally
    /// need to be used 
    /// 
    /// @{

    /// Reads 32 bit value from a peripheral address
    /// The read is done twice, and is therefore always safe in terms of 
    /// manual section 1.3 Peripheral access precautions for correct memory ordering
    /// \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    /// \return the value read from the 32 bit register
    /// \sa Physical Addresses
    extern uint32_t bcm2835_peri_read(volatile uint32_t* paddr);


    /// Reads 32 bit value from a peripheral address without the read barrier
    /// You should only use this when your code has previously called bcm2835_peri_read()
    /// within the same peripheral, and no other peripheral access has occurred since.
    /// \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    /// \return the value read from the 32 bit register
    /// \sa Physical Addresses
    extern uint32_t bcm2835_peri_read_nb(volatile uint32_t* paddr);


    /// Writes 32 bit value from a peripheral address
    /// The write is done twice, and is therefore always safe in terms of 
    /// manual section 1.3 Peripheral access precautions for correct memory ordering
    /// \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    /// \param[in] value The 32 bit value to write
    /// \sa Physical Addresses
    extern void bcm2835_peri_write(volatile uint32_t* paddr, uint32_t value);

    /// Writes 32 bit value from a peripheral address without the write barrier
    /// You should only use this when your code has previously called bcm2835_peri_write()
    /// within the same peripheral, and no other peripheral access has occurred since.
    /// \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    /// \param[in] value The 32 bit value to write
    /// \sa Physical Addresses
    extern void bcm2835_peri_write_nb(volatile uint32_t* paddr, uint32_t value);

    /// Alters a number of bits in a 32 peripheral regsiter.
    /// It reads the current valu and then alters the bits deines as 1 in mask, 
    /// according to the bit value in value. 
    /// All other bits that are 0 in the mask are unaffected.
    /// Use this to alter a subset of the bits in a register.
    /// The write is done twice, and is therefore always safe in terms of 
    /// manual section 1.3 Peripheral access precautions for correct memory ordering
    /// \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
    /// \param[in] value The 32 bit value to write, masked in by mask.
    /// \param[in] mask Bitmask that defines the bits that will be altered in the register.
    /// \sa Physical Addresses
    extern void bcm2835_peri_set_bits(volatile uint32_t* paddr, uint32_t value, uint32_t mask);
    /// @} // end of lowlevel

    /// \defgroup gpio GPIO register access
    /// These functions allow you to control the GPIO interface. You can set the 
    /// function of each GPIO pin, read the input state and set the output state.
    /// @{

    /// Sets the Function Select register for the given pin, which configures
    /// the pin as Input, Output or one of the 6 alternate functions.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from RPiGPIOPin.
    /// \param[in] mode Mode to set the pin to, one of BCM2835_GPIO_FSEL_* from \ref bcm2835FunctionSelect
    extern void bcm2835_gpio_fsel(uint8_t pin, uint8_t mode);

    /// Sets the specified pin output to 
    /// HIGH.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    /// \sa bcm2835_gpio_write()
    extern void bcm2835_gpio_set(uint8_t pin);

    /// Sets the specified pin output to 
    /// LOW.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    /// \sa bcm2835_gpio_write()
    extern void bcm2835_gpio_clr(uint8_t pin);

    /// Sets any of the first 32 GPIO output pins specified in the mask to 
    /// HIGH.
    /// \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
    /// \sa bcm2835_gpio_write_multi()
    extern void bcm2835_gpio_set_multi(uint32_t mask);

    /// Sets any of the first 32 GPIO output pins specified in the mask to 
    /// LOW.
    /// \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
    /// \sa bcm2835_gpio_write_multi()
    extern void bcm2835_gpio_clr_multi(uint32_t mask);

    /// Reads the current level on the specified 
    /// pin and returns either HIGH or LOW. Works whether or not the pin
    /// is an input or an output.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    /// \return the current level  either HIGH or LOW
    extern uint8_t bcm2835_gpio_lev(uint8_t pin);

    /// Event Detect Status.
    /// Tests whether the specified pin has detected a level or edge
    /// as requested by bcm2835_gpio_ren(), bcm2835_gpio_fen(), bcm2835_gpio_hen(), 
    /// bcm2835_gpio_len(), bcm2835_gpio_aren(), bcm2835_gpio_afen().
    /// Clear the flag for a given pin by calling bcm2835_gpio_set_eds(pin);
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    /// \return HIGH if the event detect status for th given pin is true.
    extern uint8_t bcm2835_gpio_eds(uint8_t pin);

    /// Sets the Event Detect Status register for a given pin to 1, 
    /// which has the effect of clearing the flag. Use this afer seeing
    /// an Event Detect Status on the pin.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_set_eds(uint8_t pin);

    /// Enable Rising Edge Detect Enable for the specified pin.
    /// When a rising edge is detected, sets the appropriate pin in Event Detect Status.
    /// The GPRENn registers use
    /// synchronous edge detection. This means the input signal is sampled using the
    /// system clock and then it is looking for a ?011? pattern on the sampled signal. This
    /// has the effect of suppressing glitches.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_ren(uint8_t pin);

    /// Disable Rising Edge Detect Enable for the specified pin.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_clr_ren(uint8_t pin);

    /// Enable Falling Edge Detect Enable for the specified pin.
    /// When a falling edge is detected, sets the appropriate pin in Event Detect Status.
    /// The GPRENn registers use
    /// synchronous edge detection. This means the input signal is sampled using the
    /// system clock and then it is looking for a ?100? pattern on the sampled signal. This
    /// has the effect of suppressing glitches.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_fen(uint8_t pin);

    /// Disable Falling Edge Detect Enable for the specified pin.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_clr_fen(uint8_t pin);

    /// Enable High Detect Enable for the specified pin.
    /// When a HIGH level is detected on the pin, sets the appropriate pin in Event Detect Status.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_hen(uint8_t pin);

    /// Disable High Detect Enable for the specified pin.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_clr_hen(uint8_t pin);

    /// Enable Low Detect Enable for the specified pin.
    /// When a LOW level is detected on the pin, sets the appropriate pin in Event Detect Status.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_len(uint8_t pin);

    /// Disable Low Detect Enable for the specified pin.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_clr_len(uint8_t pin);

    /// Enable Asynchronous Rising Edge Detect Enable for the specified pin.
    /// When a rising edge is detected, sets the appropriate pin in Event Detect Status.
    /// Asynchronous means the incoming signal is not sampled by the system clock. As such
    /// rising edges of very short duration can be detected.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_aren(uint8_t pin);

    /// Disable Asynchronous Rising Edge Detect Enable for the specified pin.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_clr_aren(uint8_t pin);

    /// Enable Asynchronous Falling Edge Detect Enable for the specified pin.
    /// When a falling edge is detected, sets the appropriate pin in Event Detect Status.
    /// Asynchronous means the incoming signal is not sampled by the system clock. As such
    /// falling edges of very short duration can be detected.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_afen(uint8_t pin);

    /// Disable Asynchronous Falling Edge Detect Enable for the specified pin.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    extern void bcm2835_gpio_clr_afen(uint8_t pin);

    /// Sets the Pull-up/down register for the given pin. This is
    /// used with bcm2835_gpio_pudclk() to set the  Pull-up/down resistor for the given pin.
    /// However, it is usually more convenient to use bcm2835_gpio_set_pud().
    /// \param[in] pud The desired Pull-up/down mode. One of BCM2835_GPIO_PUD_* from bcm2835PUDControl
    /// \sa bcm2835_gpio_set_pud()
    extern void bcm2835_gpio_pud(uint8_t pud);

    /// Clocks the Pull-up/down value set earlier by bcm2835_gpio_pud() into the pin.
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    /// \param[in] on HIGH to clock the value from bcm2835_gpio_pud() into the pin. 
    /// LOW to remove the clock. 
    /// \sa bcm2835_gpio_set_pud()
    extern void bcm2835_gpio_pudclk(uint8_t pin, uint8_t on);

    /// Reads and returns the Pad Control for the given GPIO group.
    /// \param[in] group The GPIO pad group number, one of BCM2835_PAD_GROUP_GPIO_*
    /// \return Mask of bits from BCM2835_PAD_* from \ref bcm2835PadGroup
    extern uint32_t bcm2835_gpio_pad(uint8_t group);

    /// Sets the Pad Control for the given GPIO group.
    /// \param[in] group The GPIO pad group number, one of BCM2835_PAD_GROUP_GPIO_*
    /// \param[in] control Mask of bits from BCM2835_PAD_* from \ref bcm2835PadGroup
    extern void bcm2835_gpio_set_pad(uint8_t group, uint32_t control);

    /// Delays for the specified number of milliseconds.
    /// Uses nanosleep(), and therefore does not use CPU until the time is up.
    /// However, you are at the mercy of nanosleep(). From the manual for nanosleep():
    /// If the interval specified in req is not an exact multiple of the granularity  
    /// underlying  clock  (see  time(7)),  then the interval will be
    /// rounded up to the next multiple. Furthermore, after the sleep completes, 
    /// there may still be a delay before the CPU becomes free to once
    /// again execute the calling thread.
    /// \param[in] millis Delay in milliseconds
    extern void bcm2835_delay (unsigned int millis);

    /// Delays for the specified number of microseconds.
    /// Uses a combination of nanosleep() and a busy wait loop on the BCM2835 system timers,
    /// However, you are at the mercy of nanosleep(). From the manual for nanosleep():
    /// If the interval specified in req is not an exact multiple of the granularity  
    /// underlying  clock  (see  time(7)),  then the interval will be
    /// rounded up to the next multiple. Furthermore, after the sleep completes, 
    /// there may still be a delay before the CPU becomes free to once
    /// again execute the calling thread.
    /// For times less than about 450 microseconds, uses a busy wait on the System Timer.
    /// It is reported that a delay of 0 microseconds on RaspberryPi will in fact
    /// result in a delay of about 80 microseconds. Your mileage may vary.
    /// \param[in] micros Delay in microseconds
    extern void bcm2835_delayMicroseconds (uint64_t micros);

    /// Sets the output state of the specified pin
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    /// \param[in] on HIGH sets the output to HIGH and LOW to LOW.
    extern void bcm2835_gpio_write(uint8_t pin, uint8_t on);

    /// Sets any of the first 32 GPIO output pins specified in the mask to the state given by on
    /// \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
    /// \param[in] on HIGH sets the output to HIGH and LOW to LOW.
    extern void bcm2835_gpio_write_multi(uint32_t mask, uint8_t on);

    /// Sets the first 32 GPIO output pins specified in the mask to the value given by value
    /// \param[in] value values required for each bit masked in by mask, eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
    /// \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
    extern void bcm2835_gpio_write_mask(uint32_t value, uint32_t mask);

    /// Sets the Pull-up/down mode for the specified pin. This is more convenient than
    /// clocking the mode in with bcm2835_gpio_pud() and bcm2835_gpio_pudclk().
    /// \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
    /// \param[in] pud The desired Pull-up/down mode. One of BCM2835_GPIO_PUD_* from bcm2835PUDControl
    extern void bcm2835_gpio_set_pud(uint8_t pin, uint8_t pud);

    /// @} 

    /// \defgroup spi SPI access
    /// These functions let you use SPI0 (Serial Peripheral Interface) to 
    /// interface with an external SPI device.
    /// @{

    /// Start SPI operations.
    /// Forces RPi SPI0 pins P1-19 (MOSI), P1-21 (MISO), P1-23 (CLK), P1-24 (CE0) and P1-26 (CE1)
    /// to alternate function ALT0, which enables those pins for SPI interface.
    /// You should call bcm2835_spi_end() when all SPI funcitons are complete to return the pins to 
    /// their default functions
    /// \sa  bcm2835_spi_end()
    extern void bcm2835_spi_begin(void);

    /// End SPI operations.
    /// SPI0 pins P1-19 (MOSI), P1-21 (MISO), P1-23 (CLK), P1-24 (CE0) and P1-26 (CE1)
    /// are returned to their default INPUT behaviour.
    extern void bcm2835_spi_end(void);

    /// Sets the SPI bit order
    /// NOTE: has no effect. Not supported by SPI0.
    /// Defaults to 
    /// \param[in] order The desired bit order, one of BCM2835_SPI_BIT_ORDER_*, 
    /// see \ref bcm2835SPIBitOrder
    extern void bcm2835_spi_setBitOrder(uint8_t order);

    /// Sets the SPI clock divider and therefore the 
    /// SPI clock speed. 
    /// \param[in] divider The desired SPI clock divider, one of BCM2835_SPI_CLOCK_DIVIDER_*, 
    /// see \ref bcm2835SPIClockDivider
    extern void bcm2835_spi_setClockDivider(uint16_t divider);

    /// Sets the SPI data mode
    /// Sets the clock polariy and phase
    /// \param[in] mode The desired data mode, one of BCM2835_SPI_MODE*, 
    /// see \ref bcm2835SPIMode
    extern void bcm2835_spi_setDataMode(uint8_t mode);

    /// Sets the chip select pin(s)
    /// When an bcm2835_spi_transfer() is made, the selected pin(s) will be asserted during the
    /// transfer.
    /// \param[in] cs Specifies the CS pins(s) that are used to activate the desired slave. 
    ///   One of BCM2835_SPI_CS*, see \ref bcm2835SPIChipSelect
    extern void bcm2835_spi_chipSelect(uint8_t cs);

    /// Sets the chip select pin polarity for a given pin
    /// When an bcm2835_spi_transfer() occurs, the currently selected chip select pin(s) 
    /// will be asserted to the 
    /// value given by active. When transfers are not happening, the chip select pin(s) 
    /// return to the complement (inactive) value.
    /// \param[in] cs The chip select pin to affect
    /// \param[in] active Whether the chip select pin is to be active HIGH
    extern void bcm2835_spi_setChipSelectPolarity(uint8_t cs, uint8_t active);

    /// Transfers one byte to and from the currently selected SPI slave.
    /// Asserts the currently selected CS pins (as previously set by bcm2835_spi_chipSelect) 
    /// during the transfer.
    /// Clocks the 8 bit value out on MOSI, and simultaneously clocks in data from MISO. 
    /// Returns the read data byte from the slave.
    /// Uses polled transfer as per section 10.6.1 of the BCM 2835 ARM Peripherls manual
    /// \param[in] value The 8 bit data byte to write to MOSI
    /// \return The 8 bit byte simultaneously read from  MISO
    /// \sa bcm2835_spi_transfern()
    extern uint8_t bcm2835_spi_transfer(uint8_t value);
    
    /// Transfers any number of bytes to and from the currently selected SPI slave.
    /// Asserts the currently selected CS pins (as previously set by bcm2835_spi_chipSelect) 
    /// during the transfer.
    /// Clocks the len 8 bit bytes out on MOSI, and simultaneously clocks in data from MISO. 
    /// The data read read from the slave is placed into rbuf. rbuf must be at least len bytes long
    /// Uses polled transfer as per section 10.6.1 of the BCM 2835 ARM Peripherls manual
    /// \param[in] tbuf Buffer of bytes to send. 
    /// \param[out] rbuf Received bytes will by put in this buffer
    /// \param[in] len Number of bytes in the tbuf buffer, and the number of bytes to send/received
    /// \sa bcm2835_spi_transfer()
    extern void bcm2835_spi_transfernb(char* tbuf, char* rbuf, uint32_t len);

    /// Transfers any number of bytes to and from the currently selected SPI slave
    /// using bcm2835_spi_transfernb.
    /// The returned data from the slave replaces the transmitted data in the buffer.
    /// \param[in,out] buf Buffer of bytes to send. Received bytes will replace the contents
    /// \param[in] len Number of bytes int eh buffer, and the number of bytes to send/received
    /// \sa bcm2835_spi_transfer()
    extern void bcm2835_spi_transfern(char* buf, uint32_t len);

    /// Transfers any number of bytes to the currently selected SPI slave.
    /// Asserts the currently selected CS pins (as previously set by bcm2835_spi_chipSelect)
    /// during the transfer.
    /// \param[in] buf Buffer of bytes to send.
    /// \param[in] len Number of bytes in the tbuf buffer, and the number of bytes to send
    extern void bcm2835_spi_writenb(char* buf, uint32_t len);

    /// @}

    /// \defgroup i2c I2C access
    /// These functions let you use I2C (The Broadcom Serial Control bus with the Philips
    /// I2C bus/interface version 2.1 January 2000.) to interface with an external I2C device.
    /// @{

    /// Start I2C operations.
    /// Forces RPi I2C pins P1-03 (SDA) and P1-05 (SCL)
    /// to alternate function ALT0, which enables those pins for I2C interface.
    /// You should call bcm2835_i2c_end() when all I2C functions are complete to return the pins to
    /// their default functions
    /// \sa  bcm2835_i2c_end()
    extern void bcm2835_i2c_begin(void);

    /// End I2C operations.
    /// I2C pins P1-03 (SDA) and P1-05 (SCL)
    /// are returned to their default INPUT behaviour.
    extern void bcm2835_i2c_end(void);

    /// Sets the I2C slave address.
    /// \param[in] addr The I2C slave address.
    extern void bcm2835_i2c_setSlaveAddress(uint8_t addr);

    /// Sets the I2C clock divider and therefore the I2C clock speed.
    /// \param[in] divider The desired I2C clock divider, one of BCM2835_I2C_CLOCK_DIVIDER_*,
    /// see \ref bcm2835I2CClockDivider
    extern void bcm2835_i2c_setClockDivider(uint16_t divider);

    /// Sets the I2C clock divider by converting the baudrate parameter to
    /// the equivalent I2C clock divider. ( see \sa bcm2835_i2c_setClockDivider)
    /// For the I2C standard 100khz you would set baudrate to 100000
    /// The use of baudrate corresponds to its use in the I2C kernel device
    /// driver. (Of course, bcm2835 has nothing to do with the kernel driver)
    extern void bcm2835_i2c_set_baudrate(uint32_t baudrate);

    /// Transfers any number of bytes to the currently selected I2C slave.
    /// (as previously set by \sa bcm2835_i2c_setSlaveAddress)
    /// \param[in] buf Buffer of bytes to send.
    /// \param[in] len Number of bytes in the buf buffer, and the number of bytes to send.
	/// \return reason see \ref bcm2835I2CReasonCodes
    extern uint8_t bcm2835_i2c_write(const char * buf, uint32_t len);

    /// Transfers any number of bytes from the currently selected I2C slave.
    /// (as previously set by \sa bcm2835_i2c_setSlaveAddress)
    /// \param[in] buf Buffer of bytes to receive.
    /// \param[in] len Number of bytes in the buf buffer, and the number of bytes to received.
	/// \return reason see \ref bcm2835I2CReasonCodes
    extern uint8_t bcm2835_i2c_read(char* buf, uint32_t len);

    /// Allows reading from I2C slaves that require a repeated start (without any prior stop)
    /// to read after the required slave register has been set. For example, the popular
    /// MPL3115A2 pressure and temperature sensor. Note that your device must support or
    /// require this mode. If your device does not require this mode then the standard
    /// combined:
    ///   \sa bcm2835_i2c_write
    ///   \sa bcm2835_i2c_read
    /// are a better choice.
    /// Will read from the slave previously set by \sa bcm2835_i2c_setSlaveAddress
    /// \param[in] regaddr Buffer containing the slave register you wish to read from.
    /// \param[in] buf Buffer of bytes to receive.
    /// \param[in] len Number of bytes in the buf buffer, and the number of bytes to received.
	/// \return reason see \ref bcm2835I2CReasonCodes
    extern uint8_t bcm2835_i2c_read_register_rs(char* regaddr, char* buf, uint32_t len);

    /// @}

    /// \defgroup st System Timer access
    /// Allows access to and delays using the System Timer Counter.
    /// @{

    /// Read the System Timer Counter register.
    /// \return the value read from the System Timer Counter Lower 32 bits register
    uint64_t bcm2835_st_read(void);

    /// Delays for the specified number of microseconds with offset.
    /// \param[in] offset_micros Offset in microseconds
    /// \param[in] micros Delay in microseconds
    extern void bcm2835_st_delay(uint64_t offset_micros, uint64_t micros);

    /// @} 

#ifdef __cplusplus
}
#endif

#endif // BCM2835_H

/// @example blink.c
/// Blinks RPi GPIO pin 11 on and off

/// @example input.c
/// Reads the state of an RPi input pin

/// @example event.c
/// Shows how to use event detection on an input pin

/// @example spi.c
/// Shows how to use SPI interface to transfer a byte to and from an SPI device

/// @example spin.c
/// Shows how to use SPI interface to transfer a number of bytes to and from an SPI device
