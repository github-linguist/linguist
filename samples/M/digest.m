;
;  GT.M Digest Extension
;  Copyright (C) 2012 Piotr Koper <piotr.koper@gmail.com>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU Affero General Public License as
;  published by the Free Software Foundation, either version 3 of the
;  License, or (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU Affero General Public License for more details.
;
;  You should have received a copy of the GNU Affero General Public License
;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

;  GT.M™ is a trademark of Fidelity Information Services, Inc.

; "GT.M™ is a vetted industrial strength, transaction processing application
;  platform consisting of a key-value database engine optimized for extreme
;  transaction processing throughput & business continuity."
;                                -- http://sourceforge.net/projects/fis-gtm/



; GT.M Digest Extension
;
; This simple OpenSSL based digest extension is a rewrite of OpenSSL
; EVP_DigestInit usage example with additional M wrapper.
; See http://www.openssl.org/docs/crypto/EVP_DigestInit.html for details.
;
; The return value from $&digest.init() is 0, usually when an invalid digest
; algorithm was specification. Anyway, properly used, should never fail.
;
; Please feel free to contact me if you have any questions or comments,
; Piotr Koper <piotr.koper@gmail.com>
;


digest(m,a) ; returns digest in ASCII HEX, all-in-one
	n c,d
	s c=$&digest.init(a)
	d &digest.update(.c,.m)
	d &digest.final(.c,.d)
	q d


init(alg) ; returns context handler, for alg try "md5", "sha256", etc
	; 0 is returned when an error occurs (e.g. unknown digest)
	q $&digest.init(alg)

update(ctx,msg) ; updates digest (ctx) by message msg
	d &digest.update(.ctx,.msg)
	q

final(ctx,digest) ; returns hex encoded message digest in digest
	; frees memory allocated for the ctx also
	d &digest.final(.ctx,.digest)
	q


; digest algorithms availability depends on libcrypto (OpenSSL) configuration

md4(m) q $$digest(.m,"md4")
md5(m) q $$digest(.m,"md5")
sha(m) q $$digest(.m,"sha")
sha1(m) q $$digest(.m,"sha1")
sha224(m) q $$digest(.m,"sha224")
sha256(m) q $$digest(.m,"sha256")
sha512(m) q $$digest(.m,"sha512")
dss1(m) q $$digest(.m,"dss1")
ripemd160(m) q $$digest(.m,"ripemd160")
