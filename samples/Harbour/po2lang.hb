#!/usr/bin/env hbmk2

/*
 * Converts .po files to core lang modules
 *
 * Copyright 2013 Viktor Szakats (vszakats.net/harbour)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * (or visit their website at https://www.gnu.org/licenses/).
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "hblang.ch"

PROCEDURE Main_po2lang()

   POToLang( "hu.po", "l_hu.c", "hu" )
   POToLang( "el.po", "l_el.c", "el" )

   RETURN

STATIC FUNCTION POToLang( cFileIn, cFileOut, cLang )

   LOCAL aTrans
   LOCAL cErrorMsg

   LOCAL cContent
   LOCAL cTranslator
   LOCAL nPos

   IF ( aTrans := __i18n_potArrayLoad( cFileIn, @cErrorMsg ) ) != NIL

      cContent := StrTran( _begin(), e"\n", hb_eol() )
      nPos := 0

      __i18n_potArrayClean( aTrans,,, {| cTrs, cOri | ProcessTrs( @cContent, cTrs, cOri, @cTranslator, @nPos, cLang ) } )

      cContent := "/* Last Translator: " + MaskEMail( cTranslator ) + " */" + hb_eol() + ;
         hb_StrShrink( cContent, Len( "," ) + Len( hb_eol() ) ) + hb_eol() + ;
         StrTran( StrTran( _end(), e"\n", hb_eol() ), "{LNG}", Upper( cLang ) )

      hb_MemoWrit( cFileOut, cContent )

      RETURN .T.
   ENDIF

   ? "i18n error", cErrorMsg

   RETURN .F.

STATIC FUNCTION MaskEMail( cEMail )
   RETURN hb_StrReplace( cEMail, { ;
      "@" => " " , ;
      "<" => "(" , ;
      ">" => ")" } )

STATIC PROCEDURE ProcessTrs( /* @ */ cContent, cTrs, cOri, /* @ */ cTranslator, /* @ */ nPos, cLang )

   STATIC sc_hEmpty := { ;
      3  => { "", "UTF8", "" }, ;
      47 => { "", "" }, ;
      57 => { "" }, ;
      64 => { "", "", "", "" }, ;
      80 => { "", "", "" } }

   LOCAL tmp

   SWITCH nPos
   CASE HB_LANG_ITEM_BASE_ID      ; tmp := "/* Identification */" ; EXIT
   CASE HB_LANG_ITEM_BASE_MONTH   ; tmp := "/* Month names */" ; EXIT
   CASE HB_LANG_ITEM_BASE_DAY     ; tmp := "/* Day names */" ; EXIT
   CASE HB_LANG_ITEM_BASE_NATMSG  ; tmp := "/* CA-Cl*pper compatible natmsg items */" ; EXIT
   CASE HB_LANG_ITEM_BASE_ERRDESC ; tmp := "/* Error description names */" ; EXIT
   CASE HB_LANG_ITEM_BASE_ERRINTR ; tmp := "/* Internal error names */" ; EXIT
   CASE HB_LANG_ITEM_BASE_TEXT    ; tmp := "/* Texts */" ; EXIT
   OTHERWISE                      ; tmp := NIL
   ENDSWITCH

   IF tmp != NIL
      cContent += iif( nPos > 0, hb_eol(), "" ) + Space( 2 * 3 ) + tmp + hb_eol() + hb_eol()
   ENDIF

   IF nPos == 0
      cTranslator := hb_regexAll( "Last-Translator: ([^\n]*)", cTrs,,,,, .T. )[ 1 ][ 2 ]
      IF cTranslator == "foo bar <foo.bar@example.org>"
         cTranslator := ""
      ENDIF
      cContent += Space( 2 * 3 ) + ConvToC( cLang ) + "," + hb_eol()
      ++nPos
   ELSE
      IF cTrs == ""
         cTrs := cOri
      ENDIF
      cContent += Space( 2 * 3 ) + ConvToC( cTrs ) + "," + hb_eol()
      ++nPos

      IF nPos $ sc_hEmpty
         FOR EACH tmp IN sc_hEmpty[ nPos ]
            cContent += Space( 2 * 3 ) + ConvToC( tmp ) + "," + hb_eol()
         NEXT
         nPos += Len( sc_hEmpty[ nPos ] )
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION ConvToC( cStr )
   RETURN '"' + hb_StrReplace( cStr, { '"' => '\"' } ) + '"'

STATIC FUNCTION _begin()
#pragma __cstream | RETURN %s

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
#pragma __endtext

STATIC FUNCTION _end()
#pragma __cstream | RETURN %s
   }
};

#define HB_LANG_ID      {LNG}
#include "hbmsgreg.h"
#pragma __endtext