#include <stdio.h>
#include <string.h>
#include <openssl/sha.h>

const char *coin_err;
#define bail(s) { coin_err = s; return 0; }

int unbase58(const char *s, unsigned char *out) {
	static const char *tmpl = "123456789"
		"ABCDEFGHJKLMNPQRSTUVWXYZ"
		"abcdefghijkmnopqrstuvwxyz";
	int i, j, c;
	const char *p;

	memset(out, 0, 25);
	for (i = 0; s[i]; i++) {
		if (!(p = strchr(tmpl, s[i])))
			bail("bad char");

		c = p - tmpl;
		for (j = 25; j--; ) {
			c += 58 * out[j];
			out[j] = c % 256;
			c /= 256;
		}

		if (c) bail("address too long");
	}

	return 1;
}

int valid(const char *s) {
	unsigned char dec[32], d1[SHA256_DIGEST_LENGTH], d2[SHA256_DIGEST_LENGTH];

	coin_err = "";
	if (!unbase58(s, dec)) return 0;

	SHA256(SHA256(dec, 21, d1), SHA256_DIGEST_LENGTH, d2);

	if (memcmp(dec + 21, d2, 4))
		bail("bad digest");

	return 1;
}

int main (void) {
	const char *s[] = {
		"1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9",
		"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",
		"1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9",
		"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I",
		0 };
	int i;
	for (i = 0; s[i]; i++) {
		int status = valid(s[i]);
		printf("%s: %s\n", s[i], status ? "Ok" : coin_err);
	}

	return 0;
}
