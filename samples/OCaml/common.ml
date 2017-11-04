(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let string_of format v = 
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in begin
    format fmt v;
    Format.pp_print_flush fmt ();
    Buffer.contents buf
  end
