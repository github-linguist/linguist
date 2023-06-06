/*
 * Copyright 2013 the original author or authors
 * @license MIT, see LICENSE.txt for details
 * https://github.com/cujojs/rest/blob/master/parsers/rfc5988.pegjs
 *
 * @author Scott Andrews
 */

start =
  start:(i:LinkValue OptionalSP ',' OptionalSP {return i;})* last:LinkValue
  { return start.concat([last]) }

LinkValue =
  '<' href:URIReference '>' OptionalSP params:LinkParams*
  {
    var link = {};
    params.forEach(function (param) {
      link[param[0]] = param[1];
    });
    link.href = href;
    return link;
  }

LinkParams = 
  ';' OptionalSP param:LinkParam OptionalSP
  { return param }

URIReference =
  // TODO see http://tools.ietf.org/html/rfc3987#section-3.1
  url:[^>]+
  { return url.join('') }

LinkParam = 
  name:LinkParamName value:LinkParamValue?
  { return [name, value] }

LinkParamName =
  name:[a-z]+
  { return name.join('') }

LinkParamValue =
  "=" str:(PToken / QuotedString)
  { return str }

PToken =
  token:PTokenChar+
  { return token.join('') }

PTokenChar = '!' / '#' / '$' / '%' / '&' / "'" / '('
           / ')' / '*' / '+' / '-' / '.' / '|' / Digit
           / ':' / '<' / '=' / '>' / '?' / '@' / Alpha
           / '[' / ']' / '^' / '_' / '`' / '{' / [//]
           / '}' / '~'

OptionalSP =
  SP*

QuotedString =
  DQ str:QuotedStringInternal DQ
  { return str }

QuotedStringInternal =
  str:(QDText / QuotedPair )*
  { return str.join('') }

Char        = [\x00-\x7F]
UpAlpha     = [A-Z]
LoAlpha     = [a-z]
Alpha       = UpAlpha / LoAlpha
Digit       = [0-9]
SP          = [\x20]
DQ          = [\x22]
QDText      = [^"]
QuotedPair  = [\\] Char
