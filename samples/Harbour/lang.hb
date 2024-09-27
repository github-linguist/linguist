#!/usr/bin/env hbmk2
/*
 * Manage translations and automatic doc generation
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

/* Requires:
 *   - curl (built with SSL)
 *   - hbmk2 and hbi18n in PATH
 *   - the target .prg be runnable as script (for doc_make only)
 * Reference: https://docs.transifex.com/api/introduction
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"

STATIC sc_hLangMapping := { ;
   "zh_CN.GB2312" => "zh_sim", ;
   "sr"           => "sr_cyr", ;
   "sr@latin"     => "sr_lat" }

PROCEDURE Main( cCommand, cMain, ... )

   LOCAL hCommand := { ;
      "doc_make" => @doc_make(), ; /* Generate doc files for all languages */
      "src_push" => @src_push(), ; /* Upload translation source to Transifex localization service */
      "trs_pull" => @trs_pull(), ; /* Download translations from Transifex localization service */
      "trs_push" => @trs_push() }  /* Upload local translations to Transifex localization service */

   IF ! Empty( cCommand ) .AND. cCommand $ hCommand .AND. HB_ISSTRING( cMain )
      Set( _SET_DEFEXTENSIONS, .F. )
      Eval( ;
         hCommand[ cCommand ], ;
         iif( Empty( hb_FNameName( cMain := hb_DirSepToOS( cMain ) ) ), cMain + hb_FNameName( hb_DirSepDel( cMain ) ) + ".prg", cMain ), ;
         ... )
   ELSE
      ? "unrecognized command or missing target"
   ENDIF

   RETURN

/* --- */

STATIC PROCEDURE doc_make( cMain )

   LOCAL hPar := LoadPar( cMain )

   LOCAL file
   LOCAL cLang
   LOCAL cTempHRB

   IF ! Empty( hPar[ "docoption" ] )

      cTempHRB := hb_FNameExtSet( hPar[ "entry" ], ".hrb" )

      ? hPar[ "name" ], "generating documentation:"

      hb_run( hb_StrFormat( "hbmk2 -hbraw -q0 %1$s -gh -o%2$s", hPar[ "entry" ], cTempHRB ) )

      FOR EACH cLang IN hb_AIns( hPar[ "langs" ], 1, hPar[ "baselang" ], .T. )

         ?? "", cLang

         hb_run( hb_StrFormat( "hbi18n -q -g %1$s -o%2$s", ;
            hPar[ "po" ] + hb_FNameName( hPar[ "entry" ] ) + "." + cLang + ".po", ;
            hb_FNameDir( hPar[ "entry" ] ) + hb_FNameName( hPar[ "entry" ] ) + "." + cLang + ".hbl" ) )

         file := hPar[ "doc" ] + hPar[ "name" ] + "." + cLang + hPar[ "docext" ]
         hb_run( hb_StrFormat( "hbmk2 %1$s %2$s > %3$s", ;
            cTempHRB, StrTran( ArrayToList( hPar[ "docoption" ] ), "{LNG}", cLang ), file ) )
         FToNativeEOL( file )

         FErase( hb_FNameDir( hPar[ "entry" ] ) + hb_FNameName( hPar[ "entry" ] ) + "." + cLang + ".hbl" )
      NEXT

      FErase( cTempHRB )
   ENDIF

   RETURN

STATIC FUNCTION FToNativeEOL( cFile )
   RETURN hb_MemoWrit( cFile, StrTran( hb_MemoRead( cFile ), e"\n", hb_eol() ) )

/* --- */

STATIC PROCEDURE src_push( cMain )

   LOCAL hPar := LoadPar( cMain )

   LOCAL json
   LOCAL cTempContent
   LOCAL cTempResult
   LOCAL cContent

   FClose( hb_FTempCreateEx( @cTempContent, , , ".pot" ) )
   FClose( hb_FTempCreateEx( @cTempResult ) )

   ? hPar[ "name" ], "generating translation source"

   IF Empty( hPar[ "po" ] )
      cContent := LangToPO( LangToCoreLang( hPar[ "baselang" ] ) )
   ELSE
      hb_run( hb_StrFormat( "hbmk2 -hbraw -q0 %1$s -j%2$s -s", hPar[ "entry" ], cTempContent ) )

      POT_Sort( cTempContent )

      cContent := hb_MemoRead( cTempContent )
   ENDIF

   IF Empty( hPar[ "po" ] )
      #ifdef DEBUG
         hb_MemoWrit( hPar[ "baselang" ] + ".po", cContent )
      #endif
   ELSE
      cContent := hb_StrFormat( ;
         "#, c-format" + hb_eol() + ;
         'msgid ""' + hb_eol() + ;
         'msgstr ""' + hb_eol() + ;
         '"Project-Id-Version: %1$s\n"' + hb_eol() + ;
         '"Language: %2$s\n"' + hb_eol() + ;
         '"MIME-Version: 1.0\n"' + hb_eol() + ;
         '"Content-Type: text/plain; charset=UTF-8\n"' + hb_eol() + ;
         '"Content-Transfer-Encoding: 8bit\n"', hb_FNameName( hPar[ "entry" ] ), hPar[ "baselang" ] ) + hb_eol() + ;
         hb_eol() + ;
         cContent
      ? hPar[ "name" ], "saving locally"
      hb_MemoWrit( hPar[ "po" ] + hb_FNameName( hPar[ "entry" ] ) + "." + hPar[ "baselang" ] + ".po", cContent )
   ENDIF

   ? hPar[ "name" ], "uploading", "size", hb_ntos( Len( cContent ) )

   hb_MemoWrit( cTempContent, hb_jsonEncode( { "content" => StrTran( cContent, hb_eol(), e"\n" ) } ) )

   hb_run( hb_StrFormat( 'curl -s -i -L --user %1$s -X ' + ;
      'PUT -d @%2$s -H "Content-Type: application/json" ' + ;
      'https://www.transifex.com/api/2/project/%3$s/resource/%4$s/content/' + ;
      ' -o %5$s', ;
      hPar[ "login" ], cTempContent, hPar[ "project" ], ;
      hb_FNameName( hPar[ "entry" ] ), cTempResult ) )

   IF hb_jsonDecode( GetJSON( hb_MemoRead( cTempResult ) ), @json ) > 0
      ? hb_ValToExp( json )
   ELSE
      ? "API error"
   ENDIF

   FErase( cTempContent )
   FErase( cTempResult )

   RETURN

STATIC FUNCTION POT_Sort( cFileName )

   LOCAL aTrans
   LOCAL cErrorMsg

   IF ( aTrans := __i18n_potArrayLoad( cFileName, @cErrorMsg ) ) != NIL .AND. ;
      __i18n_potArraySave( cFileName, __i18n_potArraySort( aTrans ), @cErrorMsg )
      RETURN .T.
   ENDIF

   ? "i18n error", cErrorMsg

   RETURN .F.

/* --- */

STATIC PROCEDURE trs_pull( cMain )

   LOCAL hPar := LoadPar( cMain )

   LOCAL json
   LOCAL cLang
   LOCAL cTempResult

   FClose( hb_FTempCreateEx( @cTempResult ) )

   ? hPar[ "name" ], "pulling translations:"

   FOR EACH cLang IN hPar[ "langs" ]

      ?? "", cLang

      hb_run( hb_StrFormat( "curl -s -i -L --user %1$s -X " + ;
         "GET https://www.transifex.com/api/2/project/%2$s/resource/%3$s/translation/%4$s/" + ;
         " -o %5$s", ;
         hPar[ "login" ], hPar[ "project" ], ;
         hb_FNameName( hPar[ "entry" ] ), cLang, cTempResult ) )

      IF hb_jsonDecode( GetJSON( hb_MemoRead( cTempResult ) ), @json ) > 0
         hb_MemoWrit( cTempResult, json[ "content" ] )
         IF ! Empty( hPar[ "po" ] )
            POT_Sort( cTempResult )
            /* should only do this if the translation is primarily done
               on Transifex website. This encouraged and probably the case
               in practice. Delete source information, delete empty
               translations and apply some automatic transformation for
               common translation mistakes. */
            PO_Clean( cTempResult, hPar[ "po" ] + hb_FNameName( hPar[ "entry" ] ) + "." + cLang + ".po", ;
               .F., .F., @DoctorTranslation() )
         ELSE
            PO_Clean( cTempResult, cTempResult, ;
               .F., .T., @DoctorTranslation() )
            POToLang( ;
               cTempResult, ;
               hb_DirBase() + hb_DirSepToOS( "../src/lang/" ) + "l_" + LangToCoreLang( cLang ) + ".c", ;
               LangToCoreLang( cLang ) )
            #ifdef DEBUG
               hb_MemoWrit( cLang + ".po", json[ "content" ] )
            #endif
         ENDIF
      ELSE
         ? "API error"
      ENDIF
   NEXT

   FErase( cTempResult )

   RETURN

STATIC FUNCTION DoctorTranslation( cString, cOri )

   LOCAL lRightToLeft := IsRightToLeft( cString )

   LOCAL regex, hit

   IF lRightToLeft
      IF ! Empty( Left( cOri, 1 ) )
         cString := RTrim( cString )
      ENDIF
      IF ! Empty( Right( cOri, 1 ) )
         cString := LTrim( cString )
      ENDIF
   ELSE
      IF ! Empty( Left( cOri, 1 ) )
         cString := LTrim( cString )
      ENDIF
      IF ! Empty( Right( cOri, 1 ) )
         cString := RTrim( cString )
      ENDIF
   ENDIF

   /* Only if original doesn't have elongated spaces */
   IF cOri == StrUnspace( cOri )
      cString := StrUnspace( cString )
   ENDIF

   /* For Transifex: RETURN SYMBOL to real new line */
   cString := StrTran( cString, hb_UChar( 0x23CE ), e"\n" )

   IF lRightToLeft
      /* Common typos: extra space or punctuation */
      cString := hb_StrReplace( cString, { ;
         e"\n "  => e"\n"  , ;
         e". \n" => e".\n" , ;
         "( "    => "("    , ;
         " )"    => ")"    , ;
         ": "    => ":"    , ;
         ", "    => ","    , ;
         "; "    => ";"    , ;
         " .:"   => ": "   , ;
         " . :"  => ": "   , ;
         " ,:"   => ": "   , ;
         " , :"  => ": "   } )

      /* Common typos: missing space */
      FOR EACH regex IN { "([A-Za-z]):", "(\S)[,;]", "(\w)\(" }
         FOR EACH hit IN hb_regexAll( regex, cString,,,,, .T. )
            IF ! hit[ 1 ] $ cOri
               cString := StrTran( cString, hit[ 1 ], StrTran( hit[ 1 ], hit[ 2 ], hit[ 2 ] + " " ) )
            ENDIF
         NEXT
      NEXT
   ELSE
      /* Common typos: extra space or punctuation */
      cString := hb_StrReplace( cString, { ;
         e"\n "  => e"\n"  , ;
         e" .\n" => e".\n" , ;
         "( "    => "("    , ;
         " )"    => ")"    , ;
         " :"    => ":"    , ;
         " ,"    => ","    , ;
         " ;"    => ";"    , ;
         ":. "   => ": "   , ;
         ": . "  => ": "   , ;
         ":, "   => ": "   , ;
         ": , "  => ": "   } )

      /* Common typos: missing space */
      FOR EACH regex IN { ":([A-Za-z])", "[,;](\S)", "\)(\w)" }
         FOR EACH hit IN hb_regexAll( regex, cString,,,,, .T. )
            IF ! hit[ 1 ] $ cOri
               cString := StrTran( cString, hit[ 1 ], StrTran( hit[ 1 ], hit[ 2 ], " " + hit[ 2 ] ) )
            ENDIF
         NEXT
      NEXT
   ENDIF

   RETURN cString

STATIC FUNCTION IsRightToLeft( cString )

   LOCAL nChar
   LOCAL tmp

   FOR tmp := 1 TO Len( cString )
      nChar := Asc( SubStr( cString, tmp, 1 ) )
      IF ( nChar >= 0x0590 .AND. nChar <= 0x05FF ) .OR. ;  /* hebrew */
         ( nChar >= 0x0600 .AND. nChar <= 0x06FF )         /* arabic */
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

/* Converts multiple spaces to just one */
STATIC FUNCTION StrUnspace( cString )

   LOCAL cResult := ""
   LOCAL cChar, cCharPrev
   LOCAL tmp

   FOR tmp := 1 TO Len( cString )

      cChar := SubStr( cString, tmp, 1 )

      IF ! cChar == " " .OR. ! cCharPrev == " "
         cResult += cChar
      ENDIF

      cCharPrev := cChar
   NEXT

   RETURN cResult

STATIC FUNCTION PO_Clean( cFNSource, cFNTarget, ... )

   LOCAL aTrans
   LOCAL cErrorMsg

   IF ( aTrans := __i18n_potArrayLoad( cFNSource, @cErrorMsg ) ) != NIL .AND. ;
      __i18n_potArraySave( cFNTarget, __i18n_potArrayClean( aTrans, ... ), @cErrorMsg )
      RETURN .T.
   ENDIF

   ? "i18n error", cErrorMsg

   RETURN .F.

/* --- */

STATIC PROCEDURE trs_push( cMain )

   LOCAL hPar := LoadPar( cMain )

   LOCAL json
   LOCAL cLang
   LOCAL cTempContent
   LOCAL cTempResult
   LOCAL cContent

   FClose( hb_FTempCreateEx( @cTempContent ) )
   FClose( hb_FTempCreateEx( @cTempResult ) )

   ? hPar[ "name" ], "uploading translation:"

   FOR EACH cLang IN hPar[ "langs" ]

      ?? "", cLang

      IF Empty( hPar[ "po" ] )
         cContent := LangToPO( LangToCoreLang( cLang ) )
         #ifdef DEBUG
            hb_MemoWrit( cLang + ".po", cContent )
         #endif
      ELSE
         cContent := hb_MemoRead( hPar[ "po" ] + hb_FNameName( hPar[ "entry" ] ) + "." + cLang + ".po" )
      ENDIF

      ?? "", hb_ntos( Len( cContent ) )

      hb_MemoWrit( cTempContent, hb_jsonEncode( { "content" => StrTran( cContent, hb_eol(), e"\n" ) } ) )

      hb_run( hb_StrFormat( 'curl -s -i -L --user %1$s -X ' + ;
         'PUT -d @%2$s -H "Content-Type: application/json" ' + ;
         'https://www.transifex.com/api/2/project/%3$s/resource/%4$s/translation/%5$s/' + ;
         ' -o %6$s', ;
         hPar[ "login" ], cTempContent, hPar[ "project" ], ;
         hb_FNameName( hPar[ "entry" ] ), cLang, cTempResult ) )

      IF hb_jsonDecode( GetJSON( hb_MemoRead( cTempResult ) ), @json ) > 0
         ? hb_ValToExp( json )
      ELSE
         ? "API error"
      ENDIF
   NEXT

   FErase( cTempContent )
   FErase( cTempResult )

   RETURN

/* --- */

STATIC FUNCTION GetJSON( cString )

   cString := SubStr( cString, At( "{", cString ) )
   cString := Left( cString, RAt( "}", cString ) )

   RETURN cString

STATIC FUNCTION ArrayToList( array )

   LOCAL cString := ""
   LOCAL tmp

   FOR EACH tmp IN array
      cString += tmp
      IF ! tmp:__enumIsLast()
         cString += " "
      ENDIF
   NEXT

   RETURN cString

STATIC FUNCTION _HAGetDef( xContainer, xDefault, ... )

   LOCAL item

   IF PCount() > 2
      FOR EACH item IN { ... }
         IF ( HB_ISHASHKEY( item ) .AND. HB_ISHASH( xContainer ) .AND. item $ xContainer ) .OR. ;
            ( HB_ISNUMERIC( item ) .AND. HB_ISARRAY( xContainer ) .AND. item >= 1 .AND. item <= Len( xContainer ) )
            xContainer := xContainer[ item ]
         ELSE
            RETURN xDefault
         ENDIF
      NEXT
      RETURN xContainer
   ENDIF

   RETURN xDefault

STATIC FUNCTION LangToCoreLang( cLang )
   RETURN hb_HGetDef( sc_hLangMapping, cLang, cLang )

STATIC FUNCTION LoadPar( cMain )

   LOCAL hPar := { => }

   LOCAL cConfig
   LOCAL item
   LOCAL tmp

   hPar[ "project" ]   := "harbour"
   hPar[ "entry" ]     := cMain
   hPar[ "login" ]     := GetEnv( "HB_TRANSIFEX_LOGIN" )  /* Format: username:password */
   hPar[ "name" ]      := hb_FNameName( hPar[ "entry" ] )

   IF hPar[ "entry" ] == "core-lang"
      hPar[ "baselang" ]  := "en"
      hPar[ "po" ]        := NIL
      hPar[ "langs" ]     := CoreLangList()
      FOR EACH item IN hPar[ "langs" ] DESCEND
         IF Lower( item ) == hPar[ "baselang" ]
            hb_ADel( item:__enumBase(), item:__enumIndex(), .T. )
            LOOP
         ENDIF
         FOR EACH tmp IN sc_hLangMapping
            IF Lower( item ) == tmp
               item := tmp:__enumKey()
               EXIT
            ENDIF
         NEXT
      NEXT
   ELSE
      cConfig := hb_MemoRead( hb_FNameExtSet( hPar[ "entry" ], ".hbp" ) )

      hPar[ "doc" ]       := hb_FNameDir( hPar[ "entry" ] ) + hb_DirSepToOS( "doc/" )
      hPar[ "docext" ]    := _HAGetDef( hb_regexAll( "-3rd=_langhb_docext=([\S]*)", cConfig,,,,, .T. ), ".txt", 1, 2 )
      hPar[ "docoption" ] := {}
      FOR EACH item IN hb_regexAll( "-3rd=_langhb_docoption=([\S]*)", cConfig,,,,, .T. )
         AAdd( hPar[ "docoption" ], item[ 2 ] )
      NEXT

      item := _HAGetDef( hb_regexAll( "-3rd=_langhb_entry=([\S]*)", cConfig,,,,, .T. ), NIL, 1, 2 )
      IF item != NIL
         item := hb_FNameDir( hPar[ "entry" ] ) + hb_DirSepToOS( item )
         hPar[ "entry" ] := iif( Empty( hb_FNameName( item ) ), item + hb_FNameName( hb_DirSepDel( item ) ) + ".prg", item )
      ENDIF

      cConfig := hb_MemoRead( hb_FNameExtSet( hPar[ "entry" ], ".hbp" ) )

      hPar[ "langs" ]     := hb_ATokens( _HAGetDef( hb_regexAll( "-lng=([\w,]*)", cConfig,,,,, .T. ), "", 1, 2 ), "," )
      hPar[ "baselang" ]  := _HAGetDef( hb_regexAll( "-3rd=_langhb_base=([\w]*)", cConfig,,,,, .T. ), "en", 1, 2 )

      hPar[ "po" ]        := hb_FNameDir( hPar[ "entry" ] ) + hb_DirSepToOS( "po/" )
   ENDIF

   RETURN hPar

/* --- */

#include "lang2po.hb"
#include "po2lang.hb"