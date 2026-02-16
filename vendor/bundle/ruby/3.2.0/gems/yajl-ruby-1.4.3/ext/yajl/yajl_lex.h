/*
 * Copyright 2010, Lloyd Hilaiel.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 * 
 *  3. Neither the name of Lloyd Hilaiel nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */ 

#ifndef __YAJL_LEX_H__
#define __YAJL_LEX_H__

#include "api/yajl_common.h"

typedef enum {
    yajl_tok_bool,         // 0
    yajl_tok_colon,        // 1
    yajl_tok_comma,        // 2
    yajl_tok_eof,          // 3
    yajl_tok_error,        // 4
    yajl_tok_left_brace,   // 5
    yajl_tok_left_bracket, // 6
    yajl_tok_null,         // 7
    yajl_tok_right_brace,  // 8
    yajl_tok_right_bracket, // 9

    /* we differentiate between integers and doubles to allow the
     * parser to interpret the number without re-scanning */
    yajl_tok_integer, // 10
    yajl_tok_double,  // 11

    /* we differentiate between strings which require further processing,
     * and strings that do not */
    yajl_tok_string, // 12
    yajl_tok_string_with_escapes, // 13

    /* comment tokens are not currently returned to the parser, ever */
    yajl_tok_comment // 14
} yajl_tok;

typedef struct yajl_lexer_t * yajl_lexer;

const char *yajl_tok_name(yajl_tok tok);

YAJL_API
yajl_lexer yajl_lex_alloc(yajl_alloc_funcs * alloc,
                          unsigned int allowComments,
                          unsigned int validateUTF8);

YAJL_API
yajl_lexer yajl_lex_realloc(yajl_lexer orig);

YAJL_API
void yajl_lex_free(yajl_lexer lexer);

/**
 * run/continue a lex. "offset" is an input/output parameter.
 * It should be initialized to zero for a
 * new chunk of target text, and upon subsetquent calls with the same
 * target text should passed with the value of the previous invocation.
 *
 * the client may be interested in the value of offset when an error is
 * returned from the lexer.  This allows the client to render useful
n * error messages.
 *
 * When you pass the next chunk of data, context should be reinitialized
 * to zero.
 * 
 * Finally, the output buffer is usually just a pointer into the jsonText,
 * however in cases where the entity being lexed spans multiple chunks,
 * the lexer will buffer the entity and the data returned will be
 * a pointer into that buffer.
 *
 * This behavior is abstracted from client code except for the performance
 * implications which require that the client choose a reasonable chunk
 * size to get adequate performance.
 */
YAJL_API
yajl_tok yajl_lex_lex(yajl_lexer lexer, const unsigned char * jsonText,
                      unsigned int jsonTextLen, unsigned int * offset,
                      const unsigned char ** outBuf, unsigned int * outLen);

/** have a peek at the next token, but don't move the lexer forward */
YAJL_API
yajl_tok yajl_lex_peek(yajl_lexer lexer, const unsigned char * jsonText,
                       unsigned int jsonTextLen, unsigned int offset);


typedef enum {
    yajl_lex_e_ok = 0,
    yajl_lex_string_invalid_utf8,
    yajl_lex_string_invalid_escaped_char,
    yajl_lex_string_invalid_json_char,
    yajl_lex_string_invalid_hex_char,
    yajl_lex_invalid_char,
    yajl_lex_invalid_string,
    yajl_lex_missing_integer_after_decimal,
    yajl_lex_missing_integer_after_exponent,
    yajl_lex_missing_integer_after_minus,
    yajl_lex_unallowed_comment,
    yajl_lex_alloc_failed
} yajl_lex_error;

YAJL_API
const char * yajl_lex_error_to_string(yajl_lex_error error);

/** allows access to more specific information about the lexical
 *  error when yajl_lex_lex returns yajl_tok_error. */
YAJL_API
yajl_lex_error yajl_lex_get_error(yajl_lexer lexer);

/** get the current offset into the most recently lexed json string. */
YAJL_API
unsigned int yajl_lex_current_offset(yajl_lexer lexer);

/** get the number of lines lexed by this lexer instance */
YAJL_API
unsigned int yajl_lex_current_line(yajl_lexer lexer);

/** get the number of chars lexed by this lexer instance since the last
 *  \n or \r */
YAJL_API
unsigned int yajl_lex_current_char(yajl_lexer lexer);

#endif
