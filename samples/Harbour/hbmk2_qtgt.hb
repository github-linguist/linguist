/*
 * hbmk2 plugin script, implementing support for QT specific features
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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

#include "directry.ch"
#include "hbclass.ch"
#include "error.ch"

#define I_( x )                 hb_i18n_gettext( x )

FUNCTION hbmk_plugin_qt( hbmk )

   LOCAL cRetVal := ""

   LOCAL cSrc
   LOCAL cDst
   LOCAL tSrc
   LOCAL tDst

   LOCAL cCommand
   LOCAL nError
   LOCAL lBuildIt

   SWITCH hbmk[ "cSTATE" ]
   CASE "init"

      hbmk_Register_Input_File_Extension( hbmk, ".h" )

      EXIT

   CASE "pre_all"

      /* Gather input parameters */

      hbmk[ "vars" ][ "aMOC_Src" ] := {}

      FOR EACH cSrc IN hbmk[ "params" ]
         IF ! Left( cSrc, 1 ) == "-" .AND. ;
            Lower( hb_FNameExt( cSrc ) ) == ".h"

            AAdd( hbmk[ "vars" ][ "aMOC_Src" ], cSrc )
         ENDIF
      NEXT

      /* Create output file lists */

      hbmk[ "vars" ][ "aMOC_Dst" ] := {}
      FOR EACH cSrc IN hbmk[ "vars" ][ "aMOC_Src" ]
         cDst := hbmk_FNameDirExtSet( "moc_" + hb_FNameName( cSrc ), hbmk[ "cWorkDir" ], ".cpp" )
         AAdd( hbmk[ "vars" ][ "aMOC_Dst" ], cDst )
         hbmk_AddInput_CPP( hbmk, cDst )
      NEXT

      /* Detect tool locations */

      IF ! hbmk[ "lCLEAN" ]
         IF ! Empty( hbmk[ "vars" ][ "aMOC_Src" ] )
            hbmk[ "vars" ][ "cMOC_BIN" ] := qt_tool_detect( hbmk, "moc", "MOC_BIN", .T. )
            IF Empty( hbmk[ "vars" ][ "cMOC_BIN" ] )
               cRetVal := I_( "Required QT tool not found" )
            ENDIF
         ENDIF
      ENDIF

      EXIT

   CASE "pre_c"

      IF ! hbmk[ "lCLEAN" ] .AND. ;
         ! Empty( hbmk[ "vars" ][ "aMOC_Src" ] )

         IF ! Empty( hbmk[ "vars" ][ "cMOC_BIN" ] )

            /* Execute 'moc' commands on input files */

            FOR EACH cSrc, cDst IN hbmk[ "vars" ][ "aMOC_Src" ], hbmk[ "vars" ][ "aMOC_Dst" ]

               IF hbmk[ "lINC" ] .AND. ! hbmk[ "lREBUILD" ]
                  lBuildIt := ! hb_FGetDateTime( cDst, @tDst ) .OR. ;
                              ! hb_FGetDateTime( cSrc, @tSrc ) .OR. ;
                              tSrc > tDst
               ELSE
                  lBuildIt := .T.
               ENDIF

               IF lBuildIt

                  cCommand := hbmk[ "vars" ][ "cMOC_BIN" ] +;
                     " " + hbmk_FNameEscape( hbmk, hbmk_PathSepToTarget( hbmk, cSrc ) ) +;
                     " -o " + hbmk_FNameEscape( hbmk, hbmk_PathSepToTarget( hbmk, cDst ) )

                  IF hbmk[ "lTRACE" ]
                     IF ! hbmk[ "lQUIET" ]
                        hbmk_OutStd( hbmk, I_( "'moc' command:" ) )
                     ENDIF
                     hbmk_OutStdRaw( hbmk, cCommand )
                  ENDIF

                  IF ! hbmk[ "lDONTEXEC" ] .AND. ( nError := hb_processRun( cCommand ) ) != 0
                     hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running 'moc' executable. %1$d" ), nError ) )
                     IF ! hbmk[ "lQUIET" ]
                        hbmk_OutErrRaw( hbmk, cCommand )
                     ENDIF
                     IF ! hbmk[ "lIGNOREERROR" ]
                        cRetVal := "error"
                        EXIT
                     ENDIF
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      ENDIF

      EXIT

   CASE "post_all"

      IF ! hbmk[ "lINC" ] .OR. hbmk[ "lCLEAN" ]
         AEval( hbmk[ "vars" ][ "aMOC_Dst" ], {| tmp | FErase( tmp ) } )
      ENDIF

      EXIT

   ENDSWITCH

   RETURN cRetVal

STATIC FUNCTION qt_tool_detect( hbmk, cName, cEnvQT, lSuffix )
   LOCAL cBIN
   LOCAL cEnv
   LOCAL aEnvList
   LOCAL cStdErr

   cBIN := GetEnv( cEnvQT )
   IF Empty( cBIN )

      IF lSuffix
         IF ! ( cEnv := GetEnv( "HB_QTPOSTFIX" ) ) == ""  /* Compatibility */
            hb_SetEnv( "HB_QTSUFFIX", cEnv )
         ENDIF
         cName += GetEnv( "HB_QTSUFFIX" )
         aEnvList := { "HB_QTPATH", "HB_QTSUFFIX" }
      ELSE
         aEnvList := { "HB_QTPATH" }
      ENDIF
      cName += hbmk[ "cCCEXT" ]

      IF Empty( cEnv := GetEnv( "HB_QTPATH" ) ) .OR. ;
         ! hb_FileExists( cBIN := hb_DirSepAdd( cEnv ) + cName )

         #if ! defined( __PLATFORM__UNIX )

            hb_AIns( aEnvList, 1, "HB_WITH_QT", .T. )

            IF ! Empty( cEnv := GetEnv( "HB_WITH_QT" ) )

               IF cEnv == "no"
                  /* Return silently. It shall fail at dependency detection inside hbmk2 */
                  RETURN NIL
               ELSE
                  IF ! hb_FileExists( cBIN := hb_PathNormalize( hb_DirSepAdd( cEnv ) + "..\bin\" + cName ) )
                     hbmk_OutErr( hbmk, hb_StrFormat( "Warning: HB_WITH_QT points to incomplete QT installation. '%1$s' executable not found.", cName ) )
                     cBIN := ""
                  ENDIF
               ENDIF
            ELSE
               cBIN := hb_DirSepAdd( hb_DirBase() ) + cName
               IF ! hb_FileExists( cBIN )
                  cBIN := ""
               ENDIF
            ENDIF
         #else
            cBIN := ""
         #endif

         IF Empty( cBIN )
            cBIN := hbmk_FindInPath( cName, GetEnv( "PATH" ) + hb_osPathListSeparator() + "/opt/qtsdk/qt/bin" )
            IF Empty( cBIN )
               hbmk_OutErr( hbmk, hb_StrFormat( "%1$s not set, could not autodetect '%2$s' executable", hbmk_ArrayToList( aEnvList, ", " ), cName ) )
               RETURN NIL
            ENDIF
         ENDIF
      ENDIF
      IF hbmk[ "lINFO" ]
         cStdErr := ""
         IF ! hbmk[ "lDONTEXEC" ]
            hb_processRun( cBIN + " -v",,, @cStdErr )
            IF ! Empty( cStdErr )
               cStdErr := " [" + StrTran( StrTran( cStdErr, Chr( 13 ) ), Chr( 10 ) ) + "]"
            ENDIF
         ENDIF
         hbmk_OutStd( hbmk, hb_StrFormat( "Using QT '%1$s' executable: %2$s%3$s (autodetected)", cName, cBIN, cStdErr ) )
      ENDIF
   ENDIF

   RETURN cBIN