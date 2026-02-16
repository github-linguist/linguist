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

#include "yajl_lex.h"
#include "yajl_parser.h"
#include "yajl_encode.h"
#include "yajl_bytestack.h"

#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <math.h>

unsigned char *
yajl_render_error_string(yajl_handle hand, const unsigned char * jsonText,
                         unsigned int jsonTextLen, int verbose)
{
    unsigned int offset = hand->bytesConsumed;
    unsigned char * str;
    const char * errorType = NULL;
    const char * errorText = NULL;
    char text[72];
    const char * arrow = "                     (right here) ------^\n";    

    if (yajl_bs_current(hand->stateStack) == yajl_state_parse_error) {
        errorType = "parse";
        errorText = hand->parseError;
    } else if (yajl_bs_current(hand->stateStack) == yajl_state_lexical_error) {
        errorType = "lexical";
        errorText = yajl_lex_error_to_string(yajl_lex_get_error(hand->lexer));
    } else {
        errorType = "unknown";
    }

    {
        unsigned int memneeded = 0;
        memneeded += strlen(errorType);
        memneeded += strlen(" error");
        if (errorText != NULL) {
            memneeded += strlen(": ");            
            memneeded += strlen(errorText);            
        }
        str = (unsigned char *) YA_MALLOC(&(hand->alloc), memneeded + 2);
        str[0] = 0;
        strcat((char *) str, errorType);
        strcat((char *) str, " error");    
        if (errorText != NULL) {
            strcat((char *) str, ": ");            
            strcat((char *) str, errorText);            
        }
        strcat((char *) str, "\n");    
    }

    /* now we append as many spaces as needed to make sure the error
     * falls at char 41, if verbose was specified */
    if (verbose) {
        unsigned int start, end, i;
        unsigned int spacesNeeded;

        spacesNeeded = (offset < 30 ? 40 - offset : 10);
        start = (offset >= 30 ? offset - 30 : 0);
        end = (offset + 30 > jsonTextLen ? jsonTextLen : offset + 30);
    
        for (i=0;i<spacesNeeded;i++) text[i] = ' ';

        for (;start < end;start++, i++) {
            if (jsonText[start] != '\n' && jsonText[start] != '\r')
            {
                text[i] = jsonText[start];
            }
            else
            {
                text[i] = ' ';
            }
        }
        assert(i <= 71);
        text[i++] = '\n';
        text[i] = 0;
        {
            char * newStr = (char *)
                YA_MALLOC(&(hand->alloc), (unsigned int)(strlen((char *) str) +
                                                         strlen((char *) text) +
                                                         strlen(arrow) + 1));
            newStr[0] = 0;
            strcat((char *) newStr, (char *) str);
            strcat((char *) newStr, text);
            strcat((char *) newStr, arrow);    
            YA_FREE(&(hand->alloc), str);
            str = (unsigned char *) newStr;
        }
    }
    return str;
}

/* check for client cancelation */
#define _CC_CHK(x)                                                \
    if (!(x)) {                                                   \
        yajl_bs_set(hand->stateStack, yajl_state_parse_error);    \
        hand->parseError =                                        \
            "client cancelled parse via callback return value";   \
        return yajl_status_client_canceled;                       \
    }

/* check for buffer error */
#define _BUF_CHK(x)                                               \
    if (yajl_buf_err(x)) {                                        \
        yajl_bs_set(hand->stateStack, yajl_state_parse_error);    \
        hand->parseError =                                        \
            "allocation failed";                                  \
        return yajl_status_alloc_failed;                          \
    }

yajl_status
yajl_do_parse(yajl_handle hand, const unsigned char * jsonText,
              unsigned int jsonTextLen)
{
    yajl_tok tok;
    const unsigned char * buf;
    unsigned int bufLen;
    unsigned int * offset = &(hand->bytesConsumed);

    *offset = 0;
    

  around_again:
    switch (yajl_bs_current(hand->stateStack)) {
        case yajl_state_parse_complete:
            return yajl_status_ok;
        case yajl_state_lexical_error:
        case yajl_state_parse_error:            
            return yajl_status_error;
        case yajl_state_start:
        case yajl_state_map_need_val:
        case yajl_state_array_need_val:
        case yajl_state_array_start: {
            /* for arrays and maps, we advance the state for this
             * depth, then push the state of the next depth.
             * If an error occurs during the parsing of the nesting
             * entity, the state at this level will not matter.
             * a state that needs pushing will be anything other
             * than state_start */
            yajl_state stateToPush = yajl_state_start;

            tok = yajl_lex_lex(hand->lexer, jsonText, jsonTextLen,
                               offset, &buf, &bufLen);

            switch (tok) {
                case yajl_tok_eof:
                    return yajl_status_insufficient_data;
                case yajl_tok_error:
                    yajl_bs_set(hand->stateStack, yajl_state_lexical_error);
                    goto around_again;
                case yajl_tok_string:
                    if (hand->callbacks && hand->callbacks->yajl_string) {
                        _CC_CHK(hand->callbacks->yajl_string(hand->ctx,
                                                             buf, bufLen));
                    }
                    break;
                case yajl_tok_string_with_escapes:
                    if (hand->callbacks && hand->callbacks->yajl_string) {
                        yajl_buf_clear(hand->decodeBuf);
                        yajl_string_decode(hand->decodeBuf, buf, bufLen);
                        _BUF_CHK(hand->decodeBuf);
                        _CC_CHK(hand->callbacks->yajl_string(
                                    hand->ctx, yajl_buf_data(hand->decodeBuf),
                                    yajl_buf_len(hand->decodeBuf)));
                    }
                    break;
                case yajl_tok_bool: 
                    if (hand->callbacks && hand->callbacks->yajl_boolean) {
                        _CC_CHK(hand->callbacks->yajl_boolean(hand->ctx,
                                                              *buf == 't'));
                    }
                    break;
                case yajl_tok_null: 
                    if (hand->callbacks && hand->callbacks->yajl_null) {
                        _CC_CHK(hand->callbacks->yajl_null(hand->ctx));
                    }
                    break;
                case yajl_tok_left_bracket:
                    if (hand->callbacks && hand->callbacks->yajl_start_map) {
                        _CC_CHK(hand->callbacks->yajl_start_map(hand->ctx));
                    }
                    stateToPush = yajl_state_map_start;
                    break;
                case yajl_tok_left_brace:
                    if (hand->callbacks && hand->callbacks->yajl_start_array) {
                        _CC_CHK(hand->callbacks->yajl_start_array(hand->ctx));
                    }
                    stateToPush = yajl_state_array_start;
                    break;
                case yajl_tok_integer:
                    /*
                     * note.  strtol does not respect the length of
                     * the lexical token.  in a corner case where the
                     * lexed number is a integer with a trailing zero,
                     * immediately followed by the end of buffer,
                     * sscanf could run off into oblivion and cause a
                     * crash.  for this reason we copy the integer
                     * (and doubles), into our parse buffer (the same
                     * one used for unescaping strings), before
                     * calling strtol.  yajl_buf ensures null padding,
                     * so we're safe.
                     */
                    if (hand->callbacks) {
                        if (hand->callbacks->yajl_number) {
                            _CC_CHK(hand->callbacks->yajl_number(
                                        hand->ctx,(const char *) buf, bufLen));
                        } else if (hand->callbacks->yajl_integer) {
                            long int i = 0;
                            yajl_buf_clear(hand->decodeBuf);
                            yajl_buf_append(hand->decodeBuf, buf, bufLen);
                            _BUF_CHK(hand->decodeBuf);
                            buf = yajl_buf_data(hand->decodeBuf);
                            i = strtol((const char *) buf, NULL, 10);
                            if ((i == LONG_MIN || i == LONG_MAX) &&
                                errno == ERANGE)
                            {
                                yajl_bs_set(hand->stateStack,
                                            yajl_state_parse_error);
                                hand->parseError = "integer overflow" ;
                                /* try to restore error offset */
                                if (*offset >= bufLen) *offset -= bufLen;
                                else *offset = 0;
                                goto around_again;
                            }
                            _CC_CHK(hand->callbacks->yajl_integer(hand->ctx,
                                                                  i));
                        }
                    }
                    break;
                case yajl_tok_double:
                    if (hand->callbacks) {
                        if (hand->callbacks->yajl_number) {
                            _CC_CHK(hand->callbacks->yajl_number(
                                        hand->ctx, (const char *) buf, bufLen));
                        } else if (hand->callbacks->yajl_double) {
                            double d = 0.0;
                            yajl_buf_clear(hand->decodeBuf);
                            yajl_buf_append(hand->decodeBuf, buf, bufLen);
                            _BUF_CHK(hand->decodeBuf);
                            buf = yajl_buf_data(hand->decodeBuf);
                            d = strtod((char *) buf, NULL);
                            if ((d == HUGE_VAL || d == -HUGE_VAL) &&
                                errno == ERANGE)
                            {
                                yajl_bs_set(hand->stateStack,
                                            yajl_state_parse_error);
                                hand->parseError = "numeric (floating point) "
                                    "overflow";
                                /* try to restore error offset */
                                if (*offset >= bufLen) *offset -= bufLen;
                                else *offset = 0;
                                goto around_again;
                            }
                            _CC_CHK(hand->callbacks->yajl_double(hand->ctx,
                                                                 d));
                        }
                    }
                    break;
                case yajl_tok_right_brace: {
                    if (yajl_bs_current(hand->stateStack) ==
                        yajl_state_array_start)
                    {
                        if (hand->callbacks &&
                            hand->callbacks->yajl_end_array)
                        {
                            _CC_CHK(hand->callbacks->yajl_end_array(hand->ctx));
                        }
                        yajl_bs_pop(hand->stateStack);
                        goto around_again;                        
                    }
                    /* intentional fall-through */
                }
                case yajl_tok_colon: 
                case yajl_tok_comma: 
                case yajl_tok_right_bracket:                
                    yajl_bs_set(hand->stateStack, yajl_state_parse_error);
                    hand->parseError =
                        "unallowed token at this point in JSON text";
                    goto around_again;
                default:
                    yajl_bs_set(hand->stateStack, yajl_state_parse_error);
                    hand->parseError = "invalid token, internal error";
                    goto around_again;
            }
            /* got a value.  transition depends on the state we're in. */
            {
                yajl_state s = yajl_bs_current(hand->stateStack);
                if (s == yajl_state_start) {
                    /* HACK: is this even safe to do?
                     yajl_bs_set(hand->stateStack, yajl_state_parse_complete); */
                    yajl_reset_parser(hand);
                } else if (s == yajl_state_map_need_val) {
                    yajl_bs_set(hand->stateStack, yajl_state_map_got_val);
                } else { 
                    yajl_bs_set(hand->stateStack, yajl_state_array_got_val);
                }
            }
            if (stateToPush != yajl_state_start) {
                if (yajl_bs_push(hand->stateStack, stateToPush)) {
                    return yajl_status_alloc_failed;
                }
            }

            goto around_again;
        }
        case yajl_state_map_start: 
        case yajl_state_map_need_key: {
            /* only difference between these two states is that in
             * start '}' is valid, whereas in need_key, we've parsed
             * a comma, and a string key _must_ follow */
            tok = yajl_lex_lex(hand->lexer, jsonText, jsonTextLen,
                               offset, &buf, &bufLen);
            switch (tok) {
                case yajl_tok_eof:
                    return yajl_status_insufficient_data;
                case yajl_tok_error:
                    yajl_bs_set(hand->stateStack, yajl_state_lexical_error);
                    goto around_again;
                case yajl_tok_string_with_escapes:
                    if (hand->callbacks && hand->callbacks->yajl_map_key) {
                        yajl_buf_clear(hand->decodeBuf);
                        yajl_string_decode(hand->decodeBuf, buf, bufLen);
                        _BUF_CHK(hand->decodeBuf);
                        buf = yajl_buf_data(hand->decodeBuf);
                        bufLen = yajl_buf_len(hand->decodeBuf);
                    }
                    /* intentional fall-through */
                case yajl_tok_string:
                    if (hand->callbacks && hand->callbacks->yajl_map_key) {
                        _CC_CHK(hand->callbacks->yajl_map_key(hand->ctx, buf,
                                                              bufLen));
                    }
                    yajl_bs_set(hand->stateStack, yajl_state_map_sep);
                    goto around_again;
                case yajl_tok_right_bracket:
                    if (yajl_bs_current(hand->stateStack) ==
                        yajl_state_map_start)
                    {
                        if (hand->callbacks && hand->callbacks->yajl_end_map) {
                            _CC_CHK(hand->callbacks->yajl_end_map(hand->ctx));
                        }
                        yajl_bs_pop(hand->stateStack);
                        goto around_again;                        
                    }
                default:
                    yajl_bs_set(hand->stateStack, yajl_state_parse_error);
                    hand->parseError =
                        "invalid object key (must be a string)"; 
                    goto around_again;
            }
        }
        case yajl_state_map_sep: {
            tok = yajl_lex_lex(hand->lexer, jsonText, jsonTextLen,
                               offset, &buf, &bufLen);
            switch (tok) {
                case yajl_tok_colon:
                    yajl_bs_set(hand->stateStack, yajl_state_map_need_val);
                    goto around_again;                    
                case yajl_tok_eof:
                    return yajl_status_insufficient_data;
                case yajl_tok_error:
                    yajl_bs_set(hand->stateStack, yajl_state_lexical_error);
                    goto around_again;
                default:
                    yajl_bs_set(hand->stateStack, yajl_state_parse_error);
                    hand->parseError = "object key and value must "
                        "be separated by a colon (':')";
                    goto around_again;
            }
        }
        case yajl_state_map_got_val: {
            tok = yajl_lex_lex(hand->lexer, jsonText, jsonTextLen,
                               offset, &buf, &bufLen);
            switch (tok) {
                case yajl_tok_right_bracket:
                    if (hand->callbacks && hand->callbacks->yajl_end_map) {
                        _CC_CHK(hand->callbacks->yajl_end_map(hand->ctx));
                    }
                    yajl_bs_pop(hand->stateStack);
                    goto around_again;                        
                case yajl_tok_comma:
                    yajl_bs_set(hand->stateStack, yajl_state_map_need_key);
                    goto around_again;                    
                case yajl_tok_eof:
                    return yajl_status_insufficient_data;
                case yajl_tok_error:
                    yajl_bs_set(hand->stateStack, yajl_state_lexical_error);
                    goto around_again;
                default:
                    yajl_bs_set(hand->stateStack, yajl_state_parse_error);
                    hand->parseError = "after key and value, inside map, " 
                                       "I expect ',' or '}'"; 
                    /* try to restore error offset */
                    if (*offset >= bufLen) *offset -= bufLen;
                    else *offset = 0;
                    goto around_again;
            }
        }
        case yajl_state_array_got_val: {
            tok = yajl_lex_lex(hand->lexer, jsonText, jsonTextLen,
                               offset, &buf, &bufLen);
            switch (tok) {
                case yajl_tok_right_brace:
                    if (hand->callbacks && hand->callbacks->yajl_end_array) {
                        _CC_CHK(hand->callbacks->yajl_end_array(hand->ctx));
                    }
                    yajl_bs_pop(hand->stateStack);
                    goto around_again;                        
                case yajl_tok_comma:
                    yajl_bs_set(hand->stateStack, yajl_state_array_need_val);
                    goto around_again;                    
                case yajl_tok_eof:
                    return yajl_status_insufficient_data;
                case yajl_tok_error:
                    yajl_bs_set(hand->stateStack, yajl_state_lexical_error);
                    goto around_again;
                default:
                    yajl_bs_set(hand->stateStack, yajl_state_parse_error);
                    hand->parseError =
                        "after array element, I expect ',' or ']'";
                    goto around_again;
            }
        }
    }
    
    abort();
    return yajl_status_error;
}

