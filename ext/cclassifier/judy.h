#ifndef __LINGUIST_JUDY_H__
#define __LINGUIST_JUDY_H__

#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <stdint.h>

#ifdef linux
	#include <endian.h>
#else
	#ifdef __BIG_ENDIAN__
		#ifndef BYTE_ORDER
			#define BYTE_ORDER 4321
		#endif
	#else
		#ifndef BYTE_ORDER
			#define BYTE_ORDER 1234
		#endif
	#endif
	#ifndef BIG_ENDIAN
		#define BIG_ENDIAN 4321
	#endif
#endif

#if defined(__LP64__) || \
	defined(__x86_64__) || \
	defined(__amd64__) || \
	defined(_WIN64) || \
	defined(__sparc64__) || \
	defined(__arch64__) || \
	defined(__powerpc64__) || \
	defined (__s390x__) 
	//	defines for 64 bit
	
	typedef unsigned long long judyvalue;
	typedef unsigned long long JudySlot;
	#define JUDY_key_mask (0x07)
	#define JUDY_key_size 8
	#define JUDY_slot_size 8
	#define JUDY_span_bytes (3 * JUDY_key_size)
	#define JUDY_span_equiv JUDY_2
	#define JUDY_radix_equiv JUDY_8

	#define PRIjudyvalue	"llu"

#else
	//	defines for 32 bit
	
	typedef uint judyvalue;
	typedef uint JudySlot;
	#define JUDY_key_mask (0x03)
	#define JUDY_key_size 4
	#define JUDY_slot_size 4
	#define JUDY_span_bytes (7 * JUDY_key_size)
	#define JUDY_span_equiv JUDY_4
	#define JUDY_radix_equiv JUDY_8

	#define PRIjudyvalue	"u"

#endif

typedef struct Judy Judy;

extern void *judy_open(uint32_t max, uint32_t depth);
extern void judy_close(Judy *judy);
extern JudySlot *judy_cell(Judy *judy, uint8_t *buff, uint32_t max);
extern JudySlot *judy_slot(Judy *judy, uint8_t *buff, uint32_t max);

extern JudySlot *judy_strt(Judy *judy, uint8_t *buff, uint32_t max);
extern uint32_t judy_key(Judy *judy, uint8_t *buff, uint32_t max);
extern JudySlot *judy_nxt(Judy *judy);

#endif
