*/**
* The MIT License (MIT)
* Copyright (c) 2012 René van Mil
* 
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
* 
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

*----------------------------------------------------------------------*
*       CLASS CL_CSV_PARSER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_csv_parser definition
  public
  inheriting from cl_object
  final
  create public .

  public section.
*"* public components of class CL_CSV_PARSER
*"* do not include other source files here!!!

    type-pools abap .
    methods constructor
      importing
        !delegate type ref to if_csv_parser_delegate
        !csvstring type string
        !separator type c
        !skip_first_line type abap_bool .
    methods parse
      raising
        cx_csv_parse_error .
  protected section.
*"* protected components of class CL_CSV_PARSER
*"* do not include other source files here!!!
  private section.
*"* private components of class CL_CSV_PARSER
*"* do not include other source files here!!!

    constants _textindicator type c value '"'.              "#EC NOTEXT
    data _delegate type ref to if_csv_parser_delegate .
    data _csvstring type string .
    data _separator type c .
    type-pools abap .
    data _skip_first_line type abap_bool .

    methods _lines
      returning
        value(returning) type stringtab .
    methods _parse_line
      importing
        !line type string
      returning
        value(returning) type stringtab
      raising
        cx_csv_parse_error .
endclass.                    "CL_CSV_PARSER DEFINITION



*----------------------------------------------------------------------*
*       CLASS CL_CSV_PARSER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_csv_parser implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method CL_CSV_PARSER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] DELEGATE                       TYPE REF TO IF_CSV_PARSER_DELEGATE
* | [--->] CSVSTRING                      TYPE        STRING
* | [--->] SEPARATOR                      TYPE        C
* | [--->] SKIP_FIRST_LINE                TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.
    super->constructor( ).
    _delegate = delegate.
    _csvstring = csvstring.
    _separator = separator.
    _skip_first_line = skip_first_line.
  endmethod.                    "constructor


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method CL_CSV_PARSER->PARSE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_CSV_PARSE_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method parse.
    data msg type string.
    if _csvstring is initial.
      message e002(csv) into msg.
      raise exception type cx_csv_parse_error
        exporting
          message = msg.
    endif.

    " Get the lines
    data is_first_line type abap_bool value abap_true.
    data lines type standard table of string.
    lines = _lines( ).
    field-symbols <line> type string.
    loop at lines assigning <line>.
      " Should we skip the first line?
      if _skip_first_line = abap_true and is_first_line = abap_true.
        is_first_line = abap_false.
        continue.
      endif.
      " Parse the line
      data values type standard table of string.
      values = _parse_line( <line> ).
      " Send values to delegate
      _delegate->values_found( values ).
    endloop.
  endmethod.                    "parse


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method CL_CSV_PARSER->_LINES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RETURNING                      TYPE        STRINGTAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _lines.
    split _csvstring at cl_abap_char_utilities=>cr_lf into table returning.
  endmethod.                    "_lines


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method CL_CSV_PARSER->_PARSE_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] LINE                           TYPE        STRING
* | [<-()] RETURNING                      TYPE        STRINGTAB
* | [!CX!] CX_CSV_PARSE_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _parse_line.
    data msg type string.

    data csvvalue type string.
    data csvvalues type standard table of string.

    data char type c.
    data pos type i value 0.
    data len type i.
    len = strlen( line ).
    while pos < len.
      char = line+pos(1).
      if char <> _separator.
        if char = _textindicator.
          data text_ended type abap_bool.
          text_ended = abap_false.
          while text_ended = abap_false.
            pos = pos + 1.
            if pos < len.
              char = line+pos(1).
              if char = _textindicator.
                text_ended = abap_true.
              else.
                if char is initial. " Space
                  concatenate csvvalue ` ` into csvvalue.
                else.
                  concatenate csvvalue char into csvvalue.
                endif.
              endif.
            else.
              " Reached the end of the line while inside a text value
              " This indicates an error in the CSV formatting
              text_ended = abap_true.
              message e003(csv) into msg.
              raise exception type cx_csv_parse_error
                exporting
                  message = msg.
            endif.
          endwhile.
          " Check if next character is a separator, otherwise the CSV formatting is incorrect
          data nextpos type i.
          nextpos = pos + 1.
          if nextpos < len and line+nextpos(1) <> _separator.
            message e003(csv) into msg.
            raise exception type cx_csv_parse_error
              exporting
                message = msg.
          endif.
        else.
          if char is initial. " Space
            concatenate csvvalue ` ` into csvvalue.
          else.
            concatenate csvvalue char into csvvalue.
          endif.
        endif.
      else.
        append csvvalue to csvvalues.
        clear csvvalue.
      endif.
      pos = pos + 1.
    endwhile.
    append csvvalue to csvvalues. " Don't forget the last value

    returning = csvvalues.
  endmethod.                    "_parse_line
endclass.                    "CL_CSV_PARSER IMPLEMENTATION