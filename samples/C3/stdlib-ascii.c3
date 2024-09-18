// Source: https://github.com/c3lang/c3c/blob/master/lib/std/ascii.c3
// Permalink: https://github.com/c3lang/c3c/blob/d61482d/lib/std/ascii.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::ascii;

macro bool in_range_m(c, start, len) => (uint)(c - start) < len;
macro bool is_lower_m(c) => in_range_m(c, 0x61, 26);
macro bool is_upper_m(c) => in_range_m(c, 0x41, 26);
macro bool is_digit_m(c) => in_range_m(c, 0x30, 10);
macro bool is_bdigit_m(c) => in_range_m(c, 0x30, 2);
macro bool is_odigit_m(c) => in_range_m(c, 0x30, 8);
macro bool is_xdigit_m(c) => in_range_m(c | 32, 0x61, 6) || is_digit_m(c);
macro bool is_alpha_m(c) => in_range_m(c | 32, 0x61, 26);
macro bool is_print_m(c) => in_range_m(c, 0x20, 95);
macro bool is_graph_m(c) => in_range_m(c, 0x21, 94);
macro bool is_space_m(c) => in_range_m(c, 0x9, 5) || c == 0x20;
macro bool is_alnum_m(c) => is_alpha_m(c) || is_digit_m(c);
macro bool is_punct_m(c) => !is_alnum_m(c) && is_graph_m(c);
macro bool is_blank_m(c) => c == 0x20 || c == 0x9;
macro bool is_cntrl_m(c) => c < 0x20 || c == 0x7f;
macro to_lower_m(c) => is_upper_m(c) ? c + 0x20 : c;
macro to_upper_m(c) => is_lower_m(c) ? c - 0x20 : c;

fn bool in_range(char c, char start, char len) => in_range_m(c, start, len);
fn bool is_lower(char c) => is_lower_m(c);
fn bool is_upper(char c) => is_upper_m(c);
fn bool is_digit(char c) => is_digit_m(c);
fn bool is_bdigit(char c) => is_bdigit_m(c);
fn bool is_odigit(char c) => is_odigit_m(c);
fn bool is_xdigit(char c) => is_xdigit_m(c);
fn bool is_alpha(char c) => is_alpha_m(c);
fn bool is_print(char c) => is_print_m(c);
fn bool is_graph(char c) => is_graph_m(c);
fn bool is_space(char c) => is_space_m(c);
fn bool is_alnum(char c) => is_alnum_m(c);
fn bool is_punct(char c) => is_punct_m(c);
fn bool is_blank(char c) => is_blank_m(c);
fn bool is_cntrl(char c) => is_cntrl_m(c);
fn char to_lower(char c) => (char)to_lower_m(c);
fn char to_upper(char c) => (char)to_upper_m(c);

fn bool char.in_range(char c, char start, char len) => in_range_m(c, start, len);
fn bool char.is_lower(char c) => is_lower_m(c);
fn bool char.is_upper(char c) => is_upper_m(c);
fn bool char.is_digit(char c) => is_digit_m(c);
fn bool char.is_bdigit(char c) => is_bdigit_m(c);
fn bool char.is_odigit(char c) => is_odigit_m(c);
fn bool char.is_xdigit(char c) => is_xdigit_m(c);
fn bool char.is_alpha(char c) => is_alpha_m(c);
fn bool char.is_print(char c) => is_print_m(c);
fn bool char.is_graph(char c) => is_graph_m(c);
fn bool char.is_space(char c) => is_space_m(c);
fn bool char.is_alnum(char c) => is_alnum_m(c);
fn bool char.is_punct(char c) => is_punct_m(c);
fn bool char.is_blank(char c) => is_blank_m(c);
fn bool char.is_cntrl(char c) => is_cntrl_m(c);
fn char char.to_lower(char c) => (char)to_lower_m(c);
fn char char.to_upper(char c) => (char)to_upper_m(c);
/**
 * @require c.is_xdigit()
 **/
fn char char.from_hex(char c) => c.is_digit() ? c - '0' : 10 + (c | 0x20) - 'a';

fn bool uint.in_range(uint c, uint start, uint len) => in_range_m(c, start, len);
fn bool uint.is_lower(uint c) => is_lower_m(c);
fn bool uint.is_upper(uint c) => is_upper_m(c);
fn bool uint.is_digit(uint c) => is_digit_m(c);
fn bool uint.is_bdigit(uint c) => is_bdigit_m(c);
fn bool uint.is_odigit(uint c) => is_odigit_m(c);
fn bool uint.is_xdigit(uint c) => is_xdigit_m(c);
fn bool uint.is_alpha(uint c) => is_alpha_m(c);
fn bool uint.is_print(uint c) => is_print_m(c);
fn bool uint.is_graph(uint c) => is_graph_m(c);
fn bool uint.is_space(uint c) => is_space_m(c);
fn bool uint.is_alnum(uint c) => is_alnum_m(c);
fn bool uint.is_punct(uint c) => is_punct_m(c);
fn bool uint.is_blank(uint c) => is_blank_m(c);
fn bool uint.is_cntrl(uint c) => is_cntrl_m(c);
fn uint uint.to_lower(uint c) => (uint)to_lower_m(c);
fn uint uint.to_upper(uint c) => (uint)to_upper_m(c);
