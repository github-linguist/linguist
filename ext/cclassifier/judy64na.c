//	Judy arrays	23 NOV 2012

//	Author Karl Malbrain, malbrain@yahoo.com
//	with assistance from Jan Weiss.

//	Simplified judy arrays for strings and integers
//	Adapted from the ideas of Douglas Baskins of HP.

//	The -D ASKITIS benchmarking option was implemented with 
//	assistance from Dr. Nikolas Askitis (www.naskitis.com). 

//	Map a set of keys to corresponding memory cells (uint32_ts).
//	Each cell must be set to a non-zero value by the caller.

//	STANDALONE is defined to compile into a string sorter.

//	String mappings are denoted by calling judy_open with zero as
//	the second argument.  Integer mappings are denoted by calling
//	judy_open with the Integer depth of the Judy Trie as the second
//	argument.

//#define STANDALONE

//	functions:
//	judy_open:	open a new judy array returning a judy object.
//	judy_close:	close an open judy array, freeing all memory.
//	judy_clone:	clone an open judy array, duplicating the stack.
//	judy_data:	allocate data memory within judy array for external use.
//	judy_cell:	insert a string into the judy array, return cell pointer.
//	judy_strt:	retrieve the cell pointer greater than or equal to given key
//	judy_slot:	retrieve the cell pointer, or return NULL for a given key.
//	judy_key:	retrieve the string value for the most recent judy query.
//	judy_end:	retrieve the cell pointer for the last string in the array.
//	judy_nxt:	retrieve the cell pointer for the next string in the array.
//	judy_prv:	retrieve the cell pointer for the prev string in the array.
//	judy_del:	delete the key and cell for the current stack entry.

#include "judy.h"

#define JUDY_mask (~(JudySlot)0x07)

//	define the alignment factor for judy nodes and allocations
//	to enable this feature, set to 64

#define JUDY_cache_line 8	// minimum size is 8 bytes
#define JUDY_seg	65536

enum JUDY_types {
	JUDY_radix		= 0,	// inner and outer radix fan-out
	JUDY_1			= 1,	// linear list nodes of designated count
	JUDY_2			= 2,
	JUDY_4			= 3,
	JUDY_8			= 4,
	JUDY_16			= 5,
	JUDY_32			= 6,
#ifdef ASKITIS
	JUDY_64			= 7
#else
	JUDY_span		= 7 	// up to 28 tail bytes of key contiguously stored
#endif
};

int JudySize[] = {
	(JUDY_slot_size * 16),						// JUDY_radix node size
	(JUDY_slot_size + JUDY_key_size),			// JUDY_1 node size
	(2 * JUDY_slot_size + 2 * JUDY_key_size),
	(4 * JUDY_slot_size + 4 * JUDY_key_size),
	(8 * JUDY_slot_size + 8 * JUDY_key_size),
	(16 * JUDY_slot_size + 16 * JUDY_key_size),
	(32 * JUDY_slot_size + 32 * JUDY_key_size),
#ifndef ASKITIS
	(JUDY_span_bytes + JUDY_slot_size)
#else
	(64 * JUDY_slot_size + 64 * JUDY_key_size)
#endif
};

judyvalue JudyMask[9] = {
0, 0xff, 0xffff, 0xffffff, 0xffffffff,
#if JUDY_key_size > 4
0xffffffffffULL, 0xffffffffffffULL, 0xffffffffffffffULL, 0xffffffffffffffffULL
#endif
};

typedef struct {
	void *seg;			// next used allocator
	uint32_t next;			// next available offset
} JudySeg;

typedef struct {
	JudySlot next;		// judy object
	uint32_t off;			// offset within key
	int slot;			// slot within object
} JudyStack;

typedef struct {
	JudySlot root[1];	// root of judy array
	void **reuse[8];	// reuse judy blocks
	JudySeg *seg;		// current judy allocator
	uint32_t level;			// current height of stack
	uint32_t max;			// max height of stack
	uint32_t depth;			// number of Integers in a key, or zero for string keys
	JudyStack stack[1];	// current cursor
} Judy;

#ifdef ASKITIS

#if JUDY_key_size < 8
#define JUDY_max	JUDY_16
#else
#define JUDY_max	JUDY_64
#endif
#else
#define JUDY_max	JUDY_32
#endif

//	open judy object
//		call with max key size
//		and Integer tree depth.

void *judy_open (uint32_t max, uint32_t depth)
{
JudySeg *seg;
Judy *judy;
uint32_t amt;

	max++;		// allow for zero terminator on keys

	if( (seg = malloc(JUDY_seg)) ) {
		seg->seg = NULL;
		seg->next = JUDY_seg;
	} else {
#if defined(STANDALONE) || defined(ASKITIS)
		judy_abort ("No virtual memory");
#else
		return NULL;
#endif
	}

	amt = sizeof(Judy) + max * sizeof(JudyStack);

	if( amt & (JUDY_cache_line - 1) )
		amt |= JUDY_cache_line - 1, amt++;

#if defined(STANDALONE) || defined(ASKITIS)
	MaxMem += JUDY_seg;
#endif

	seg->next -= (JudySlot)seg & (JUDY_cache_line - 1);
	seg->next -= amt;

	judy = (Judy *)((uint8_t *)seg + seg->next);
	memset(judy, 0, amt);
	judy->depth = depth;
 	judy->seg = seg;
	judy->max = max;
	return judy;
}

void judy_close (Judy *judy)
{
JudySeg *seg, *nxt = judy->seg;

	while( (seg = nxt) )
		nxt = seg->seg, free (seg);
}

//	allocate judy node

void *judy_alloc (Judy *judy, uint32_t type)
{
uint32_t amt, idx, min;
JudySeg *seg;
void **block;
void **rtn;

	if( !judy->seg )
#if defined(STANDALONE) || defined(ASKITIS)
			judy_abort("illegal allocation from judy clone");
#else
			return NULL;
#endif

	if( type == JUDY_radix )
		type = JUDY_radix_equiv;

#ifndef ASKITIS
	if( type == JUDY_span )
		type = JUDY_span_equiv;
#endif

	amt = JudySize[type];

	if( amt & 0x07 )
		amt |= 0x07, amt += 1;

	//	see if free block is already available

	if( (block = judy->reuse[type]) ) {
		judy->reuse[type] = *block;
		memset (block, 0, amt);
		return (void *)block;
	}

	//	break down available larger block
	//	for reuse into smaller blocks

	if( type >= JUDY_1 )
	  for( idx = type; idx++ < JUDY_max; )
		if( block = judy->reuse[idx] ) {
		  judy->reuse[idx] = *block;
		  while( idx-- > type) {
			judy->reuse[idx] = block + JudySize[idx] / sizeof(void *);
			block[JudySize[idx] / sizeof(void *)] = 0;
		  }
		  memset (block, 0, amt);
		  return (void *)block;
		}

	min = amt < JUDY_cache_line ? JUDY_cache_line : amt;

	if( judy->seg->next < min + sizeof(*seg) ) {
		if( (seg = malloc (JUDY_seg)) ) {
			seg->next = JUDY_seg;
			seg->seg = judy->seg;
			judy->seg = seg;
			seg->next -= (JudySlot)seg & (JUDY_cache_line - 1);
		} else {
#if defined(STANDALONE) || defined(ASKITIS)
			judy_abort("Out of virtual memory");
#else
			return NULL;
#endif
		}

#if defined(STANDALONE) || defined(ASKITIS)
		MaxMem += JUDY_seg;
#endif
	}

	//	generate additional free blocks
	//	to fill up to cache line size

	rtn = (void **)((uint8_t *)judy->seg + judy->seg->next - amt);

	for( idx = type; amt & (JUDY_cache_line - 1); amt <<= 1 ) {
		block = (void **)((uint8_t *)judy->seg + judy->seg->next - 2 * amt);
		judy->reuse[idx++] = block;
		*block = 0;
	}

	judy->seg->next -= amt;
	memset (rtn, 0, JudySize[type]);
	return (void *)rtn;
}

void *judy_data (Judy *judy, uint32_t amt)

{
JudySeg *seg;
void *block;

	if( !judy->seg )
#if defined(STANDALONE) || defined(ASKITIS)
			judy_abort("illegal allocation from judy clone");
#else
			return NULL;
#endif

	if( amt & (JUDY_cache_line - 1))
		amt |= (JUDY_cache_line - 1), amt += 1;

	if( judy->seg->next < amt + sizeof(*seg) ) {
		if( (seg = malloc (JUDY_seg)) ) {
			seg->next = JUDY_seg;
			seg->seg = judy->seg;
			judy->seg = seg;
			seg->next -= (JudySlot)seg & (JUDY_cache_line - 1);
		} else {
#if defined(STANDALONE) || defined(ASKITIS)
			judy_abort("Out of virtual memory");
#else
			return NULL;
#endif
		}
	
#if defined(STANDALONE) || defined(ASKITIS)
		MaxMem += JUDY_seg;
#endif
	}

	judy->seg->next -= amt;

	block = (void *)((uint8_t *)judy->seg + judy->seg->next);
	memset (block, 0, amt);
	return block;
}

void *judy_clone (Judy *judy)
{
Judy *clone;
uint32_t amt;

	amt = sizeof(Judy) + judy->max * sizeof(JudyStack);
	clone = judy_data (judy, amt);
	memcpy (clone, judy, amt);
	clone->seg = NULL;	// stop allocations from cloned array
	return clone;
}

void judy_free (Judy *judy, void *block, int type)
{
	if( type == JUDY_radix )
		type = JUDY_radix_equiv;

#ifndef ASKITIS
	if( type == JUDY_span )
		type = JUDY_span_equiv;
#endif

	*((void **)(block)) = judy->reuse[type];
	judy->reuse[type] = (void **)block;
	return;
}
		
//	assemble key from current path

uint32_t judy_key (Judy *judy, uint8_t *buff, uint32_t max)
{
judyvalue *dest = (judyvalue *)buff;
uint32_t len = 0, idx = 0, depth;
int slot, off, type;
judyvalue value;
uint8_t *base;
int keysize;

	if( judy->depth )
		max = judy->depth * JUDY_key_size;
	else
		max--;		// leave room for zero terminator

	while( len < max && ++idx <= judy->level ) {
		type = judy->stack[idx].next & 0x07;
		slot = judy->stack[idx].slot;
		depth = len / JUDY_key_size;

		if( judy->depth )
		  if( !(len & JUDY_key_mask) )
			dest[depth] = 0;

		switch( type ) {
		case JUDY_1:
		case JUDY_2:
		case JUDY_4:
		case JUDY_8:
		case JUDY_16:
		case JUDY_32:
#ifdef ASKITIS
		case JUDY_64:
#endif
			keysize = JUDY_key_size - (judy->stack[idx].off & JUDY_key_mask);
			base = (uint8_t *)(judy->stack[idx].next & JUDY_mask);

			if( judy->depth ) {
				value = *(judyvalue *)(base + slot * keysize);
				value &= JudyMask[keysize];
				dest[depth++] |= value;
				len += keysize;

				if( depth < judy->depth )
					continue;

				return len;
			}

#if BYTE_ORDER != BIG_ENDIAN
			off = keysize;

			while( off-- && len < max )
				if( buff[len] = base[slot * keysize + off] )
					len++;
				else
					break;
#else
			for( off = 0; off < keysize && len < max; off++ )
				if( buff[len] = base[slot * keysize + off] )
					len++;
				else
					break;
#endif
			continue;

		case JUDY_radix:
			if( judy->depth ) {
				dest[depth] |= (judyvalue)slot << (JUDY_key_size - (++len & JUDY_key_mask)) * 8;
		  		if( !(len & JUDY_key_mask) )
					depth++;
				if( depth < judy->depth )
					continue;

				return len;
			}

			if( !slot )
				break;
			buff[len++] = (uint8_t)slot;
			continue;

#ifndef ASKITIS
		case JUDY_span:
			base = (uint8_t *)(judy->stack[idx].next & JUDY_mask);

			for( slot = 0; slot < JUDY_span_bytes && base[slot]; slot++ )
			  if( len < max )
				buff[len++] = base[slot];
			continue;
#endif
		}
	}
	buff[len] = 0;
	return len;
}

//	find slot & setup cursor

JudySlot *judy_slot (Judy *judy, uint8_t *buff, uint32_t max)
{
judyvalue *src = (judyvalue *)buff;
int slot, size, keysize, tst, cnt;
JudySlot next = *judy->root;
judyvalue value, test = 0;
JudySlot *table;
JudySlot *node;
uint32_t depth = 0;
uint32_t off = 0;
uint8_t *base;

#ifndef ASKITIS
	judy->level = 0;
#endif

	while( next ) {
#ifndef ASKITIS
		if( judy->level < judy->max )
			judy->level++;

		judy->stack[judy->level].next = next;
		judy->stack[judy->level].off = off;
#endif
		size = JudySize[next & 0x07];

		switch( next & 0x07 ) {

		case JUDY_1:
		case JUDY_2:
		case JUDY_4:
		case JUDY_8:
		case JUDY_16:
		case JUDY_32:
#ifdef ASKITIS
		case JUDY_64:
#endif
			base = (uint8_t *)(next & JUDY_mask);
			node = (JudySlot *)((next & JUDY_mask) + size);
			keysize = JUDY_key_size - (off & JUDY_key_mask);
			cnt = size / (sizeof(JudySlot) + keysize);
			slot = cnt;
			value = 0;

			if( judy->depth ) {
				value = src[depth++];
				off |= JUDY_key_mask;
				off++;
				value &= JudyMask[keysize];
			} else
			  do {
				value <<= 8;
				if( off < max )
					value |= buff[off];
			  } while( ++off & JUDY_key_mask );

			//  find slot > key

			while( slot-- ) {
				test = *(judyvalue *)(base + slot * keysize);
#if BYTE_ORDER == BIG_ENDIAN
				test >>= 8 * (JUDY_key_size - keysize); 
#else
				test &= JudyMask[keysize];
#endif
				if( test <= value )
					break;
			}
#ifndef ASKITIS
			judy->stack[judy->level].slot = slot;
#endif
			if( test == value ) {

				// is this a leaf?

				if( !judy->depth && !(value & 0xFF) || judy->depth && depth == judy->depth )
					return &node[-slot-1];

				next = node[-slot-1];
				continue;
			}

			return NULL;

		case JUDY_radix:
			table = (JudySlot  *)(next & JUDY_mask); // outer radix

			if( judy->depth )
				slot = (src[depth] >> ((JUDY_key_size - ++off & JUDY_key_mask) * 8)) & 0xff;
			else if( off < max )
				slot = buff[off++];
			else
				slot = 0;
#ifndef ASKITIS
			//	put radix slot on judy stack

			judy->stack[judy->level].slot = slot;
#endif
			if( (next = table[slot >> 4]) )
				table = (JudySlot  *)(next & JUDY_mask); // inner radix
			else
				return NULL;

			if( judy->depth )
				if( !(off & JUDY_key_mask) )
					depth++;

			if( !judy->depth && !slot || judy->depth && depth == judy->depth )	// leaf?
				if( table[slot & 0x0F] )	// occupied?
					return &table[slot & 0x0F];
				else
					return NULL;

			next = table[slot & 0x0F];
			continue;

#ifndef ASKITIS
		case JUDY_span:
			node = (JudySlot *)((next & JUDY_mask) + JudySize[JUDY_span]);
			base = (uint8_t *)(next & JUDY_mask);
			cnt = tst = JUDY_span_bytes;
			if( tst > (int)(max - off) )
				tst = max - off;
			value = strncmp((const char *)base, (const char *)(buff + off), tst);
			if( !value && tst < cnt && !base[tst] ) // leaf?
				return &node[-1];

			if( !value && tst == cnt ) {
				next = node[-1];
				off += cnt;
				continue;
			}
			return NULL;
#endif
		}
	}

	return NULL;
}

//	promote full nodes to next larger size

JudySlot *judy_promote (Judy *judy, JudySlot *next, int idx, judyvalue value, int keysize)
{
uint8_t *base = (uint8_t *)(*next & JUDY_mask);
int oldcnt, newcnt, slot;
#if BYTE_ORDER == BIG_ENDIAN
	int i;
#endif
JudySlot *newnode, *node;
JudySlot *result;
uint8_t *newbase;
uint32_t type;

	type = (*next & 0x07) + 1;
	node = (JudySlot *)((*next & JUDY_mask) + JudySize[type-1]);
	oldcnt = JudySize[type-1] / (sizeof(JudySlot) + keysize);
	newcnt = JudySize[type] / (sizeof(JudySlot) + keysize);

	// promote node to next larger size

	newbase = judy_alloc (judy, type);
	newnode = (JudySlot *)(newbase + JudySize[type]);
	*next = (JudySlot)newbase | type;

	//	open up slot at idx

	memcpy(newbase + (newcnt - oldcnt - 1) * keysize, base, idx * keysize);	// copy keys

	for( slot = 0; slot < idx; slot++ )
		newnode[-(slot + newcnt - oldcnt)] = node[-(slot + 1)];	// copy ptr

	//	fill in new node

#if BYTE_ORDER != BIG_ENDIAN
	memcpy(newbase + (idx + newcnt - oldcnt - 1) * keysize, &value, keysize);	// copy key
#else
	i = keysize;

	while( i-- )
	  newbase[(idx + newcnt - oldcnt - 1) * keysize + i] = value, value >>= 8;
#endif
	result = &newnode[-(idx + newcnt - oldcnt)];

	//	copy rest of old node

	memcpy(newbase + (idx + newcnt - oldcnt) * keysize, base + (idx * keysize), (oldcnt - slot) * keysize);	// copy keys

	for( ; slot < oldcnt; slot++ )
		newnode[-(slot + newcnt - oldcnt + 1)] = node[-(slot + 1)];	// copy ptr

#ifndef ASKITIS
	judy->stack[judy->level].next = *next;
	judy->stack[judy->level].slot = idx + newcnt - oldcnt - 1;
#endif
	judy_free (judy, (void **)base, type - 1);
	return result;
}

//	construct new node for JUDY_radix entry
//	make node with slot - start entries
//	moving key over one offset

void judy_radix (Judy *judy, JudySlot *radix, uint8_t *old, int start, int slot, int keysize, uint8_t key, uint32_t depth)
{
int size, idx, cnt = slot - start, newcnt;
JudySlot *node, *oldnode;
uint32_t type = JUDY_1 - 1;
JudySlot *table;
uint8_t *base;

	//	if necessary, setup inner radix node

	if( !(table = (JudySlot *)(radix[key >> 4] & JUDY_mask)) ) {
		table = judy_alloc (judy, JUDY_radix);
		radix[key >> 4] = (JudySlot)table | JUDY_radix;
	}

	oldnode = (JudySlot *)(old + JudySize[JUDY_max]);

	// is this slot a leaf?

	if( !judy->depth && (!key || !keysize) || judy->depth && !keysize && depth == judy->depth) {
		table[key & 0x0F] = oldnode[-start-1];
		return;
	}

	//	calculate new node big enough to contain slots

	do {
		type++;
		size = JudySize[type];
		newcnt = size / (sizeof(JudySlot) + keysize);
	} while( cnt > newcnt && type < JUDY_max );

	//	store new node pointer in inner table

	base = judy_alloc (judy, type);
	node = (JudySlot *)(base + size);
	table[key & 0x0F] = (JudySlot)base | type;

	//	allocate node and copy old contents
	//	shorten keys by 1 byte during copy

	for( idx = 0; idx < cnt; idx++ ) {
#if BYTE_ORDER != BIG_ENDIAN
		memcpy (base + (newcnt - idx - 1) * keysize, old + (start + cnt - idx - 1) * (keysize + 1), keysize);
#else
		memcpy (base + (newcnt - idx - 1) * keysize, old + (start + cnt - idx - 1) * (keysize + 1) + 1, keysize);
#endif
		node[-(newcnt - idx)] = oldnode[-(start + cnt - idx)];
	}
}
			
//	decompose full node to radix nodes

void judy_splitnode (Judy *judy, JudySlot *next, uint32_t size, uint32_t keysize, uint32_t depth)
{
int cnt, slot, start = 0;
uint32_t key = 0x0100, nxt;
JudySlot *newradix;
uint8_t *base;

	base = (uint8_t  *)(*next & JUDY_mask);
	cnt = size / (sizeof(JudySlot) + keysize);

	//	allocate outer judy_radix node

	newradix = judy_alloc (judy, JUDY_radix);
	*next = (JudySlot)newradix | JUDY_radix;

	for( slot = 0; slot < cnt; slot++ ) {
#if BYTE_ORDER != BIG_ENDIAN
		nxt = base[slot * keysize + keysize - 1];
#else
		nxt = base[slot * keysize];
#endif

		if( key > 0xFF )
			key = nxt;
		if( nxt == key )
			continue;

		//	decompose portion of old node into radix nodes

		judy_radix (judy, newradix, base, start, slot, keysize - 1, (uint8_t)key, depth);
		start = slot;
		key = nxt;
	}

	judy_radix (judy, newradix, base, start, slot, keysize - 1, (uint8_t)key, depth);
	judy_free (judy, (void **)base, JUDY_max);
}

//	return first leaf

JudySlot *judy_first (Judy *judy, JudySlot next, uint32_t off, uint32_t depth)
{
JudySlot *table, *inner;
uint32_t keysize, size;
JudySlot *node;
int slot, cnt;
uint8_t *base;

	while( next ) {
		if( judy->level < judy->max )
			judy->level++;

		judy->stack[judy->level].off = off;
		judy->stack[judy->level].next = next;
		size = JudySize[next & 0x07];

		switch( next & 0x07 ) {
		case JUDY_1:
		case JUDY_2:
		case JUDY_4:
		case JUDY_8:
		case JUDY_16:
		case JUDY_32:
#ifdef ASKITIS
		case JUDY_64:
#endif
			keysize = JUDY_key_size - (off & JUDY_key_mask);
			node = (JudySlot *)((next & JUDY_mask) + size);
			base = (uint8_t *)(next & JUDY_mask);
			cnt = size / (sizeof(JudySlot) + keysize);

			for( slot = 0; slot < cnt; slot++ )
				if( node[-slot-1] )
					break;

			judy->stack[judy->level].slot = slot;
#if BYTE_ORDER != BIG_ENDIAN
			if( !judy->depth && !base[slot * keysize] || judy->depth && ++depth == judy->depth )
				return &node[-slot-1];
#else
			if( !judy->depth && !base[slot * keysize + keysize - 1] || judy->depth && ++depth == judy->depth )
				return &node[-slot-1];
#endif
			next = node[-slot - 1];
			off = (off | JUDY_key_mask) + 1;
			continue;
		case JUDY_radix:
			off++;

			if( judy->depth )
			  if( !(off & JUDY_key_mask) )
				depth++;

			table = (JudySlot *)(next & JUDY_mask);
			for( slot = 0; slot < 256; slot++ )
			  if( (inner = (JudySlot *)(table[slot >> 4] & JUDY_mask)) ) {
				if( (next = inner[slot & 0x0F]) ) {
				  judy->stack[judy->level].slot = slot;
				  if( !judy->depth && !slot || judy->depth && depth == judy->depth )
					return &inner[slot & 0x0F];
				  else
					break;
				}
			  } else
				slot |= 0x0F;
			continue;
#ifndef ASKITIS
		case JUDY_span:
			node = (JudySlot *)((next & JUDY_mask) + JudySize[JUDY_span]);
			base = (uint8_t *)(next & JUDY_mask);
			cnt = JUDY_span_bytes;
			if( !base[cnt - 1] )	// leaf node?
				return &node[-1];
			next = node[-1];
			off += cnt;
			continue;
#endif
		}
	}
	return NULL;
}

//	return last leaf cell pointer

JudySlot *judy_last (Judy *judy, JudySlot next, uint32_t off, uint32_t depth)
{
JudySlot *table, *inner;
uint32_t keysize, size;
JudySlot *node;
int slot, cnt;
uint8_t *base;

	while( next ) {
		if( judy->level < judy->max )
			judy->level++;

		judy->stack[judy->level].next = next;
		judy->stack[judy->level].off = off;
		size = JudySize[next & 0x07];
		switch( next & 0x07 ) {
		case JUDY_1:
		case JUDY_2:
		case JUDY_4:
		case JUDY_8:
		case JUDY_16:
		case JUDY_32:
#ifdef ASKITIS
		case JUDY_64:
#endif
			keysize = JUDY_key_size - (off & JUDY_key_mask);
			slot = size / (sizeof(JudySlot) + keysize);
			base = (uint8_t *)(next & JUDY_mask);
			node = (JudySlot *)((next & JUDY_mask) + size);
			judy->stack[judy->level].slot = --slot;

#if BYTE_ORDER != BIG_ENDIAN
			if( !judy->depth && !base[slot * keysize] || judy->depth && ++depth == judy->depth )
#else
			if( !judy->depth && !base[slot * keysize + keysize - 1] || judy->depth && ++depth == judy->depth )
#endif
				return &node[-slot-1];

			next = node[-slot-1];
			off += keysize;
			continue;

		case JUDY_radix:
			table = (JudySlot *)(next & JUDY_mask);
			off++;

			if( judy->depth )
			  if( !(off & JUDY_key_mask) )
				depth++;

			for( slot = 256; slot--; ) {
			  judy->stack[judy->level].slot = slot;
			  if( (inner = (JudySlot *)(table[slot >> 4] & JUDY_mask)) ) {
				if( (next = inner[slot & 0x0F]) )
				  if( !judy->depth && !slot || judy->depth && depth == judy->depth )
					return &inner[0];
				  else
					break;
			  } else
				slot &= 0xF0;
			}
			continue;

#ifndef ASKITIS
		case JUDY_span:
			node = (JudySlot *)((next & JUDY_mask) + JudySize[JUDY_span]);
			base = (uint8_t *)(next & JUDY_mask);
			cnt = JUDY_span_bytes;
			if( !base[cnt - 1] )	// leaf node?
				return &node[-1];
			next = node[-1];
			off += cnt;
			continue;
#endif
		}
	}
	return NULL;
}

//	judy_end: return last entry

JudySlot *judy_end (Judy *judy)
{
	judy->level = 0;
	return judy_last (judy, *judy->root, 0, 0);
} //	judy_nxt: return next entry

JudySlot *judy_nxt (Judy *judy)
{
JudySlot *table, *inner;
int slot, size, cnt;
JudySlot *node;
JudySlot next;
uint32_t keysize;
uint8_t *base;
uint32_t depth;
uint32_t off;

	if( !judy->level )
		return judy_first (judy, *judy->root, 0, 0);

	while( judy->level ) {
		next = judy->stack[judy->level].next;
		slot = judy->stack[judy->level].slot;
		off = judy->stack[judy->level].off;
		keysize = JUDY_key_size - (off & JUDY_key_mask);
		size = JudySize[next & 0x07];
		depth = off / JUDY_key_size;

		switch( next & 0x07 ) {
		case JUDY_1:
		case JUDY_2:
		case JUDY_4:
		case JUDY_8:
		case JUDY_16:
		case JUDY_32:
#ifdef ASKITIS
		case JUDY_64:
#endif
			cnt = size / (sizeof(JudySlot) + keysize);
			node = (JudySlot *)((next & JUDY_mask) + size);
			base = (uint8_t *)(next & JUDY_mask);
			if( ++slot < cnt )
#if BYTE_ORDER != BIG_ENDIAN
				if( !judy->depth && !base[slot * keysize] || judy->depth && ++depth == judy->depth )
#else
				if( !judy->depth && !base[slot * keysize + keysize - 1] || judy->depth && ++depth == judy->depth )
#endif
				{
					judy->stack[judy->level].slot = slot;
					return &node[-slot - 1];
				} else {
					judy->stack[judy->level].slot = slot;
					return judy_first (judy, node[-slot-1], (off | JUDY_key_mask) + 1, depth);
				}
			judy->level--;
			continue;

		case JUDY_radix:
			table = (JudySlot *)(next & JUDY_mask);

			if( judy->depth )
			  if( !((off+1) & JUDY_key_mask) )
				depth++; 

			while( ++slot < 256 )
			  if( (inner = (JudySlot *)(table[slot >> 4] & JUDY_mask)) ) {
				if( inner[slot & 0x0F] ) {
				  judy->stack[judy->level].slot = slot;
				  if( !judy->depth || depth < judy->depth )
					return judy_first(judy, inner[slot & 0x0F], off + 1, depth);
				  return &inner[slot & 0x0F];
				}
			  } else
				slot |= 0x0F;

			judy->level--;
			continue;
#ifndef ASKITIS
		case JUDY_span:
			judy->level--;
			continue;
#endif
		}
	}
	return NULL;
}

//	judy_prv: return ptr to previous entry

JudySlot *judy_prv (Judy *judy)
{
int slot, size, keysize;
JudySlot *table, *inner;
JudySlot *node, next;
uint8_t *base;
uint32_t depth;
uint32_t off;

	if( !judy->level )
		return judy_last (judy, *judy->root, 0, 0);
	
	while( judy->level ) {
		next = judy->stack[judy->level].next;
		slot = judy->stack[judy->level].slot;
		off = judy->stack[judy->level].off;
		size = JudySize[next & 0x07];
		depth = off / JUDY_key_size;

		switch( next & 0x07 ) {
		case JUDY_1:
		case JUDY_2:
		case JUDY_4:
		case JUDY_8:
		case JUDY_16:
		case JUDY_32:
#ifdef ASKITIS
		case JUDY_64:
#endif
			node = (JudySlot *)((next & JUDY_mask) + size);
			if( !slot || !node[-slot] ) {
				judy->level--;
				continue;
			}

			base = (uint8_t *)(next & JUDY_mask);
			judy->stack[judy->level].slot--;
			keysize = JUDY_key_size - (off & JUDY_key_mask);

#if BYTE_ORDER != BIG_ENDIAN
			if( !judy->depth && !base[(slot - 1) * keysize] || judy->depth && ++depth == judy->depth )
#else
			if( !judy->depth && !base[(slot - 1) * keysize + keysize - 1] || judy->depth && ++depth == judy->depth )
#endif
				return &node[-slot];
			return judy_last (judy, node[-slot], (off | JUDY_key_mask) + 1, depth);

		case JUDY_radix:
			table = (JudySlot *)(next & JUDY_mask);

			if( judy->depth )
			  if( !((off + 1) & JUDY_key_mask) )
				depth++;

			while( slot-- ) {
			  judy->stack[judy->level].slot--;
			  if( (inner = (JudySlot *)(table[slot >> 4] & JUDY_mask)) )
				if( inner[slot & 0x0F] )
				  if( !judy->depth && !slot || judy->depth && depth == judy->depth )
					return &inner[0];
				  else
				    return judy_last(judy, inner[slot & 0x0F], off + 1, depth);
			}

			judy->level--;
			continue;

#ifndef ASKITIS
		case JUDY_span:
			judy->level--;
			continue;
#endif
		}
	}
	return NULL;
}

//	judy_del: delete string from judy array
//		returning previous entry.

JudySlot *judy_del (Judy *judy)
{
int slot, off, size, type, high;
JudySlot *table, *inner;
JudySlot next, *node;
int keysize, cnt;
uint8_t *base;

	while( judy->level ) {
		next = judy->stack[judy->level].next;
		slot = judy->stack[judy->level].slot;
		off = judy->stack[judy->level].off;
		size = JudySize[next & 0x07];

		switch( type = next & 0x07 ) {
		case JUDY_1:
		case JUDY_2:
		case JUDY_4:
		case JUDY_8:
		case JUDY_16:
		case JUDY_32:
#ifdef ASKITIS
		case JUDY_64:
#endif
			keysize = JUDY_key_size - (off & JUDY_key_mask);
			cnt = size / (sizeof(JudySlot) + keysize);
			node = (JudySlot *)((next & JUDY_mask) + size);
			base = (uint8_t *)(next & JUDY_mask);

			//	move deleted slot to first slot

			while( slot ) {
				node[-slot-1] = node[-slot];
				memcpy (base + slot * keysize, base + (slot - 1) * keysize, keysize);
				slot--;
			}

			//	zero out first slot

			node[-1] = 0;
			memset (base, 0, keysize);

			if( node[-cnt] ) {	// does node have any slots left?
				judy->stack[judy->level].slot++;
				return judy_prv (judy);
			}

			judy_free (judy, base, type);
			judy->level--;
			continue;

		case JUDY_radix:
			table = (JudySlot  *)(next & JUDY_mask);
			inner = (JudySlot *)(table[slot >> 4] & JUDY_mask);
			inner[slot & 0x0F] = 0;
			high = slot & 0xF0;

			for( cnt = 16; cnt--; )
				if( inner[cnt] )
					return judy_prv (judy);

			judy_free (judy, inner, JUDY_radix);
			table[slot >> 4] = 0;

			for( cnt = 16; cnt--; )
				if( table[cnt] )
					return judy_prv (judy);

			judy_free (judy, table, JUDY_radix);
			judy->level--;
			continue;

#ifndef ASKITIS
		case JUDY_span:
			base = (uint8_t *)(next & JUDY_mask);
			judy_free (judy, base, type);
			judy->level--;
			continue;
#endif
		}
	}

	//	tree is now empty

	*judy->root = 0;
	return NULL;
}

//	return cell for first key greater than or equal to given key

JudySlot *judy_strt (Judy *judy, uint8_t *buff, uint32_t max)
{
JudySlot *cell;

	judy->level = 0;

	if( !max )
		return judy_first (judy, *judy->root, 0, 0);

	if( (cell = judy_slot (judy, buff, max)) )
		return cell;

	return judy_nxt (judy);
}

//	split open span node

#ifndef ASKITIS
void judy_splitspan (Judy *judy, JudySlot *next, uint8_t *base)
{
JudySlot *node = (JudySlot *)(base + JudySize[JUDY_span]);
uint32_t cnt = JUDY_span_bytes;
uint8_t *newbase;
uint32_t off = 0;
#if BYTE_ORDER != BIG_ENDIAN
int i;
#endif

	do {
		newbase = judy_alloc (judy, JUDY_1);
		*next = (JudySlot)newbase | JUDY_1;

#if BYTE_ORDER != BIG_ENDIAN
		i = JUDY_key_size;
		while( i-- )
			*newbase++ = base[off + i];
#else
		memcpy (newbase, base + off, JUDY_key_size);
		newbase += JUDY_key_size;
#endif
		next = (JudySlot *)newbase;

		off += JUDY_key_size;
		cnt -= JUDY_key_size;
	} while( cnt && base[off - 1] );

	*next = node[-1];
	judy_free (judy, base, JUDY_span);
}
#endif

//	judy_cell: add string to judy array

JudySlot *judy_cell (Judy *judy, uint8_t *buff, uint32_t max)
{
judyvalue *src = (judyvalue *)buff;
int size, idx, slot, cnt, tst;
JudySlot *next = judy->root;
judyvalue test = 0, value;
uint32_t off = 0, start;
JudySlot *table;
JudySlot *node;
uint32_t depth = 0;
uint32_t keysize;
uint8_t *base;

	judy->level = 0;

	while( *next ) {
#ifndef ASKITIS
		if( judy->level < judy->max )
			judy->level++;

		judy->stack[judy->level].next = *next;
		judy->stack[judy->level].off = off;
#endif
		switch( *next & 0x07 ) {
		default:
			size = JudySize[*next & 0x07];
			keysize = JUDY_key_size - (off & JUDY_key_mask);
			cnt = size / (sizeof(JudySlot) + keysize);
			base = (uint8_t *)(*next & JUDY_mask);
			node = (JudySlot *)((*next & JUDY_mask) + size);
			start = off;
			slot = cnt;
			value = 0;

			if( judy->depth ) {
				value = src[depth++];
				off |= JUDY_key_mask;
				off++;
				value &= JudyMask[keysize];
			} else
			  do {
				value <<= 8;
				if( off < max )
					value |= buff[off];
			  } while( ++off & JUDY_key_mask );

			//  find slot > key

			while( slot-- ) {
				test = *(judyvalue *)(base + slot * keysize);
#if BYTE_ORDER == BIG_ENDIAN
				test >>= 8 * (JUDY_key_size - keysize); 
#else
				test &= JudyMask[keysize];
#endif
				if( test <= value )
					break;
			}
#ifndef ASKITIS
			judy->stack[judy->level].slot = slot;
#endif
			if( test == value ) {		// new key is equal to slot key
				next = &node[-slot-1];

				// is this a leaf?

				if( !judy->depth && !(value & 0xFF) || judy->depth && depth == judy->depth ) {
				  return next;
				}

				continue;
			}

			//	if this node is not full
			//	open up cell after slot

			if( !node[-1] ) {
		 	  memmove(base, base + keysize, slot * keysize);	// move keys less than new key down one slot
#if BYTE_ORDER != BIG_ENDIAN
			  memcpy(base + slot * keysize, &value, keysize);	// copy new key into slot
#else
			  test = value;
			  idx = keysize;

			  while( idx-- )
				  base[slot * keysize + idx] = test, test >>= 8;
#endif
			  for( idx = 0; idx < slot; idx++ )
				node[-idx-1] = node[-idx-2];// copy tree ptrs/cells down one slot

			  node[-slot-1] = 0;			// set new tree ptr/cell
			  next = &node[-slot-1];

			  if( !judy->depth && !(value & 0xFF) || judy->depth && depth == judy->depth ) {
			  	return next;
			  }

			  continue;
			}

			if( size < JudySize[JUDY_max] ) {
			  next = judy_promote (judy, next, slot+1, value, keysize);

			  if( !judy->depth && !(value & 0xFF) || judy->depth && depth == judy->depth ) {
				return next;
			  }

			  continue;
			}

			//	split full maximal node into JUDY_radix nodes
			//  loop to reprocess new insert

			judy_splitnode (judy, next, size, keysize, depth);
#ifndef ASKITIS
			judy->level--;
#endif
			off = start;
			if( judy->depth )
				depth--;
			continue;
		
		case JUDY_radix:
			table = (JudySlot *)(*next & JUDY_mask); // outer radix

			if( judy->depth )
				slot = (src[depth] >> ((JUDY_key_size - ++off & JUDY_key_mask) * 8)) & 0xff;
			else if( off < max )
				slot = buff[off++];
			else
				slot = 0, off++;

			if( judy->depth )
				if( !(off & JUDY_key_mask) )
					depth++;

			// allocate inner radix if empty

			if( !table[slot >> 4] )
				table[slot >> 4] = (JudySlot)judy_alloc (judy, JUDY_radix) | JUDY_radix;

			table = (JudySlot *)(table[slot >> 4] & JUDY_mask);
#ifndef ASKITIS
			judy->stack[judy->level].slot = slot;
#endif
			next = &table[slot & 0x0F];

			if( !judy->depth && !slot || judy->depth && depth == judy->depth ) { // leaf?
				return next;
			}

			continue;

#ifndef ASKITIS
		case JUDY_span:
			base = (uint8_t *)(*next & JUDY_mask);
			node = (JudySlot *)((*next & JUDY_mask) + JudySize[JUDY_span]);
			cnt = JUDY_span_bytes;
			tst = cnt;

			if( tst > (int)(max - off) )
				tst = max - off;

			value = strncmp((const char *)base, (const char *)(buff + off), tst);

			if( !value && tst < cnt && !base[tst] ) // leaf?
				return &node[-1];

			if( !value && tst == cnt ) {
				next = &node[-1];
				off += cnt;
				continue;
			}

			//	bust up JUDY_span node and produce JUDY_1 nodes
			//	then loop to reprocess insert

			judy_splitspan (judy, next, base);
			judy->level--;
			continue;
#endif
		}
	}

	// place JUDY_1 node under JUDY_radix node(s)

#ifndef ASKITIS
	if( off & JUDY_key_mask )
	 if( judy->depth || off <= max ) {
#else
	  while( off <= max ) {
#endif
		base = judy_alloc (judy, JUDY_1);
		keysize = JUDY_key_size - (off & JUDY_key_mask);
		node = (JudySlot  *)(base + JudySize[JUDY_1]);
		*next = (JudySlot)base | JUDY_1;

		//	fill in slot 0 with bytes of key

		if( judy->depth ) {
			value = src[depth];
#if BYTE_ORDER != BIG_ENDIAN
			  memcpy(base, &value, keysize);	// copy new key into slot
#else
			  while( keysize-- )
				  base[keysize] = value, value >>= 8;
#endif
		} else {
#if BYTE_ORDER != BIG_ENDIAN
		  while( keysize )
			if( off + keysize <= max )
				*base++ = buff[off + --keysize];
			else
				base++, --keysize;
#else
		  tst = keysize;

		  if( tst > (int)(max - off) )
			tst = max - off;

		  memcpy (base, buff + off, tst);
#endif
		}
#ifndef ASKITIS
		if( judy->level < judy->max )
			judy->level++;
		judy->stack[judy->level].next = *next;
		judy->stack[judy->level].slot = 0;
		judy->stack[judy->level].off = off;
#endif
		next = &node[-1];

		off |= JUDY_key_mask;
		depth++;
		off++;
	}

	//	produce span nodes to consume rest of key
	//  or judy_1 nodes if not string tree

#ifndef ASKITIS
	if( !judy->depth )
	  while( off <= max ) {
		base = judy_alloc (judy, JUDY_span);
		*next = (JudySlot)base | JUDY_span;
		node = (JudySlot  *)(base + JudySize[JUDY_span]);
		cnt = tst = JUDY_span_bytes;
		if( tst > (int)(max - off) )
			tst = max - off;
		memcpy (base, buff + off, tst);

		if( judy->level < judy->max )
			judy->level++;
		judy->stack[judy->level].next = *next;
		judy->stack[judy->level].slot = 0;
		judy->stack[judy->level].off = off;
		next = &node[-1];
		off += tst;
		depth++;

		if( !base[cnt-1] )	// done on leaf
			break;
	  }
	else
	  while( depth < judy->depth ) {
		base = judy_alloc (judy, JUDY_1);
		node = (JudySlot  *)(base + JudySize[JUDY_1]);
		*next = (JudySlot)base | JUDY_1;

		//	fill in slot 0 with bytes of key

		*(judyvalue *)base = src[depth];

		if( judy->level < judy->max )
			judy->level++;
		judy->stack[judy->level].next = *next;
		judy->stack[judy->level].slot = 0;
		judy->stack[judy->level].off = off;
		next = &node[-1];
		off |= JUDY_key_mask;
		depth++;
		off++;
	  }
#endif

	return next;
}

