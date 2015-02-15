#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint8_t byte;
typedef struct {
	FILE *fp;
	uint32_t accu;
	int bits;
} bit_io_t, *bit_filter;

bit_filter b_attach(FILE *f)
{
	bit_filter b = malloc(sizeof(bit_io_t));
	b->bits = b->accu = 0;
	b->fp = f;
	return b;
}

void b_write(byte *buf, size_t n_bits, size_t shift, bit_filter bf)
{
	uint32_t accu = bf->accu;
	int bits = bf->bits;

	buf += shift / 8;
	shift %= 8;

	while (n_bits || bits >= 8) {
		while (bits >= 8) {
			bits -= 8;
			fputc(accu >> bits, bf->fp);
			accu &= (1 << bits) - 1;
		}
		while (bits < 8 && n_bits) {
			accu = (accu << 1) | (((128 >> shift) & *buf) >> (7 - shift));
			--n_bits;
			bits++;
			if (++shift == 8) {
				shift = 0;
				buf++;
			}
		}
	}
	bf->accu = accu;
	bf->bits = bits;
}

size_t b_read(byte *buf, size_t n_bits, size_t shift, bit_filter bf)
{
	uint32_t accu = bf->accu;
	int bits = bf->bits;
	int mask, i = 0;

	buf += shift / 8;
	shift %= 8;

	while (n_bits) {
		while (bits && n_bits) {
			mask = 128 >> shift;
			if (accu & (1 << (bits - 1))) *buf |= mask;
			else *buf &= ~mask;

			n_bits--;
			bits--;

			if (++shift >= 8) {
				shift = 0;
				buf++;
			}
		}
		if (!n_bits) break;
		accu = (accu << 8) | fgetc(bf->fp);
		bits += 8;
	}
	bf->accu = accu;
	bf->bits = bits;

	return i;
}

void b_detach(bit_filter bf)
{
	if (bf->bits) {
		bf->accu <<= 8 - bf->bits;
		fputc(bf->accu, bf->fp);
	}
	free(bf);
}

int main()
{
	unsigned char s[] = "abcdefghijk";
	unsigned char s2[11] = {0};
	int i;

	FILE *f = fopen("test.bin", "wb");
	bit_filter b = b_attach(f);
	/* for each byte in s, write 7 bits skipping 1 */
	for (i = 0; i < 10; i++) b_write(s + i, 7, 1, b);
	b_detach(b);
	fclose(f);

	/* read 7 bits and expand to each byte of s2 skipping 1 bit */
	f = fopen("test.bin", "rb");
	b = b_attach(f);
	for (i = 0; i < 10; i++) b_read(s2 + i, 7, 1, b);
	b_detach(b);
	fclose(f);

	printf("%10s\n", s2); /* should be the same first 10 bytes as in s */

	return 0;
}
