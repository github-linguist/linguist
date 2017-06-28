/* CarbonOS System/Kernel
 * Copyright 2015-2017 David Aylaian, No Rights Reserved
 * Licensed under the Unlicense: http://unlicense.org
 */

#ifndef ASM_H
#define ASM_H

#include <stdint.h>

// macros for enabling and disabling interrupts
#define enable()	asm("sti");
#define disable()	asm("cli");

// inb instruction
uint8_t inb (uint16_t port) {
	uint8_t val;
	asm volatile ("inb %0, %1" : "=a"(val): "Nd"(port));
	return val;
}

// outb instruction
void outb (uint16_t port, uint8_t val) {
	asm volatile ("outb %1, %0" : : "a"(val), "Nd"(port));
}

#endif
