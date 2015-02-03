/*
 * Copyright (C) 2014 FH Bielefeld
 *
 * This file is part of a FH Bielefeld project.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA  02110-1301  USA
 */

/**
 * @file ntru_encrypt.h
 * Header for the internal API of ntru_encrypt.c.
 * @brief header for encrypt.c
 */

#ifndef PQC_ENCRYPT_H
#define PQC_ENCRYPT_H


#include "ntru_params.h"
#include "ntru_poly.h"
#include "ntru_string.h"

#include <fmpz_poly.h>
#include <fmpz.h>


/**
 * encrypt the msg, using the math:
 * e = (h ∗ r) + m (mod q)
 *
 * e = the encrypted poly
 *
 * h = the public key
 *
 * r = the random poly
 *
 * m = the message poly
 *
 * q = large mod
 *
 * @param msg_tern the message to encrypt, in ternary format
 * @param pub_key the public key
 * @param rnd the random poly (should have relatively small
 * coefficients, but not restricted to {-1, 0, 1})
 * @param out the output poly which is in the range {0, q-1}
 * (not ternary!) [out]
 * @param params ntru_params the ntru context
 */
void
ntru_encrypt_poly(
		const fmpz_poly_t msg_tern,
		const fmpz_poly_t pub_key,
		const fmpz_poly_t rnd,
		fmpz_poly_t out,
		const ntru_params *params);

/**
 * Encrypt a message in the form of a null-terminated char array and
 * return a string.
 *
 * @param msg the message
 * @param pub_key the public key
 * @param rnd the random poly (should have relatively small
 * coefficients, but not restricted to {-1, 0, 1})
 * @param params ntru_params the ntru context
 * @return the newly allocated encrypted string
 */
string *
ntru_encrypt_string(
		const string *msg,
		const fmpz_poly_t pub_key,
		const fmpz_poly_t rnd,
		const ntru_params *params);


#endif /* PQC_ENCRYPT_H */
