/*
 * Harbour NETIO server management client engine
 *
 * Copyright 2009-2013 Viktor Szakats (vszakats.net/harbour)
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

#include "hbver.ch"

#define _NETIOMGM_IPV4_DEF  "127.0.0.1"
#define _NETIOMGM_PORT_DEF  2940

#define _NETIOCLI_cIP                    1
#define _NETIOCLI_nPort                  2
#define _NETIOCLI_pConnection            3
#define _NETIOCLI_nStreamID              4
#define _NETIOCLI_hCommands              5
#define _NETIOCLI_lWaitStream            6
#define _NETIOCLI_hWaitThread            7
#define _NETIOCLI_hConIO                 8
#define _NETIOCLI_MAX_                   8

FUNCTION __hbshell_plugin()
   RETURN { ;
      "id"   => "netio", ;
      "init" => {| hConIO, ... | hbnetiocon_init( hConIO, ... ) }, ;
      "exit" => {| context | hbnetiocon_exit( context ) }, ;
      "cmd"  => {| context, cCommand | hbnetiocon_command( context, cCommand ) } }

STATIC PROCEDURE hbnetiocon_dispevent( netiocli, cText )

   LOCAL cPrompt

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      cPrompt := "hbnetiosrv$ "
   ELSE
      cPrompt := "hbnetiosrv://" + netiocli[ _NETIOCLI_cIP ] + ":" + hb_ntos( netiocli[ _NETIOCLI_nPort ] ) + "$ "
   ENDIF

   Eval( netiocli[ _NETIOCLI_hConIO ][ "displine" ], cPrompt + cText )

   RETURN

STATIC FUNCTION hbnetiocon_init( hConIO, aParam )

   LOCAL netiocli[ _NETIOCLI_MAX_ ]

   LOCAL cParam
   LOCAL cIP := _NETIOMGM_IPV4_DEF
   LOCAL nPort := _NETIOMGM_PORT_DEF
   LOCAL cPassword := ""

   FOR EACH cParam IN aParam
      DO CASE
      CASE Lower( Left( cParam, Len( "--netio.addr=" ) ) ) == "--netio.addr="
         hbnetiocon_IPPortSplit( SubStr( cParam, Len( "--netio.addr=" ) + 1 ), @cIP, @nPort )
         IF Empty( nPort )
            nPort := _NETIOMGM_PORT_DEF
         ENDIF
      CASE Lower( Left( cParam, Len( "--netio.pass=" ) ) ) == "--netio.pass="
         cPassword := SubStr( cParam, Len( "--netio.pass=" ) + 1 )
         hb_StrClear( @cParam )
      ENDCASE
   NEXT

   netiocli[ _NETIOCLI_cIP ]         := ""
   netiocli[ _NETIOCLI_nPort ]       := 0
   netiocli[ _NETIOCLI_pConnection ] := NIL
   netiocli[ _NETIOCLI_nStreamID ]   := NIL
   netiocli[ _NETIOCLI_hConIO ]      := hConIO
   netiocli[ _NETIOCLI_hCommands ]   := { ;
      "?"             => { ""               , "Synonym for 'help'."                            , {| netiocli | cmdHelp( netiocli ) } },;
      "connect"       => { "[<ip[:port>]]"  , "Connect."                                       , {| netiocli, cCommand | cmdConnect( netiocli, cCommand ) } },;
      "about"         => { ""               , "About."                                         , {| netiocli | cmdAbout( netiocli ) } },;
      "disconnect"    => { ""               , "Disconnect."                                    , {| netiocli | cmdDisconnect( netiocli ) } },;
      "sysinfo"       => { ""               , "Show server system/build information."          , {| netiocli | cmdSysInfo( netiocli ) } },;
      "showconf"      => { ""               , "Show server configuration."                     , {| netiocli | cmdServerConfig( netiocli ) } },;
      "show"          => { ""               , "Show list of connections."                      , {| netiocli | cmdConnInfo( netiocli, .F. ) } },;
      "showadmin"     => { ""               , "Show list of management connections."           , {| netiocli | cmdConnInfo( netiocli, .T. ) } },;
      "noconn"        => { ""               , "Disable incoming connections."                  , {| netiocli | cmdConnEnable( netiocli, .F. ) } },;
      "conn"          => { ""               , "Enable incoming connections."                   , {| netiocli | cmdConnEnable( netiocli, .T. ) } },;
      "nologconn"     => { ""               , "Disable logging incoming connections."          , {| netiocli | cmdConnLogEnable( netiocli, .F. ) } },;
      "logconn"       => { ""               , "Enable logging incoming connections."           , {| netiocli | cmdConnLogEnable( netiocli, .T. ) } },;
      "filt"          => { ""               , "Show filters."                                  , {| netiocli | cmdConnFilters( netiocli, .F. ) } },;
      "filtadmin"     => { ""               , "Show filters for management connections."       , {| netiocli | cmdConnFilters( netiocli, .T. ) } },;
      "allowadd"      => { "<ip>"           , "Add allow filter"                               , {| netiocli, cCommand | cmdConnFilterMod( netiocli, cCommand, "hbnetiomgm_allowadd" ) } },;
      "allowdel"      => { "<ip>"           , "Remove allow filter"                            , {| netiocli, cCommand | cmdConnFilterMod( netiocli, cCommand, "hbnetiomgm_allowdel" ) } },;
      "blockadd"      => { "<ip>"           , "Add block filter"                               , {| netiocli, cCommand | cmdConnFilterMod( netiocli, cCommand, "hbnetiomgm_blockadd" ) } },;
      "blockdel"      => { "<ip>"           , "Remove block filter"                            , {| netiocli, cCommand | cmdConnFilterMod( netiocli, cCommand, "hbnetiomgm_blockdel" ) } },;
      "allowaddadmin" => { "<ip>"           , "Add allow filter for management connections"    , {| netiocli, cCommand | cmdConnFilterMod( netiocli, cCommand, "hbnetiomgm_allowaddadmin" ) } },;
      "allowdeladmin" => { "<ip>"           , "Remove allow filter for management connections" , {| netiocli, cCommand | cmdConnFilterMod( netiocli, cCommand, "hbnetiomgm_allowdeladmin" ) } },;
      "blockaddadmin" => { "<ip>"           , "Add block filter for management connections"    , {| netiocli, cCommand | cmdConnFilterMod( netiocli, cCommand, "hbnetiomgm_blockaddadmin" ) } },;
      "blockdeladmin" => { "<ip>"           , "Remove block filter for management connections" , {| netiocli, cCommand | cmdConnFilterMod( netiocli, cCommand, "hbnetiomgm_blockdeladmin" ) } },;
      "filtsave"      => { ""               , "Save filters to disk."                          , {| netiocli | cmdConnFilterSave( netiocli ) } },;
      "stop"          => { "[<ip:port>|all]", "Stop specified connection(s)."                  , {| netiocli, cCommand | cmdConnStop( netiocli, cCommand ) } },;
      "clientinfo"    => { "[<ip:port>"     , "Show client details."                           , {| netiocli, cCommand | cmdConnClientInfo( netiocli, cCommand ) } },;
      "shutdown"      => { ""               , "Stop server."                                   , {| netiocli | cmdShutdown( netiocli ) } },;
      "help"          => { ""               , "Display this help."                             , {| netiocli | cmdHelp( netiocli ) } } }

   IF ! Empty( cPassword )
      ConnectLow( netiocli, cIP, nPort, cPassword )
   ENDIF

   RETURN netiocli

STATIC PROCEDURE hbnetiocon_exit( netiocli )

   IF ! Empty( netiocli )
      IF ! Empty( netiocli[ _NETIOCLI_pConnection ] )
         DisconnectLow( netiocli )
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION hbnetiocon_command( netiocli, cCommand )

   LOCAL aCommand
   LOCAL nPos

   IF ! Empty( netiocli )
      aCommand := hb_ATokens( cCommand, " " )
      IF ! Empty( aCommand ) .AND. ( nPos := hb_HPos( netiocli[ _NETIOCLI_hCommands ], Lower( aCommand[ 1 ] ) ) ) > 0
         Eval( hb_HValueAt( netiocli[ _NETIOCLI_hCommands ], nPos )[ 3 ], netiocli, cCommand )
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

STATIC PROCEDURE hbnetiocon_IPPortSplit( cAddr, /* @ */ cIP, /* @ */ nPort )

   LOCAL tmp

   IF ! Empty( cAddr )
      cIP := cAddr
      IF ( tmp := At( ":", cIP ) ) > 0
         nPort := Val( SubStr( cIP, tmp + Len( ":" ) ) )
         cIP := Left( cIP, tmp - 1 )
      ELSE
         nPort := NIL
      ENDIF
   ENDIF

   RETURN

/* TODO: To display event in separate screen area than cmd prompt. */
STATIC FUNCTION hbnetiocon_acceptStreamData( netiocli, xItem )

   IF HB_ISSTRING( xItem )
      IF xItem == "__SHUTDOWN__"
         hbnetiocon_dispevent( netiocli, "> message from server: Shutting down..." )
         RETURN .F.
      ELSE
         hbnetiocon_dispevent( netiocli, hb_StrFormat( "> message from server: %1$s", xItem ) )
      ENDIF
   ENDIF

   RETURN .T.

STATIC PROCEDURE hbnetiocon_waitStream( netiocli, bBlock ) /* in separate thread */

   LOCAL lExit := .F.
   LOCAL xList
   LOCAL xItem
   LOCAL xRetVal

   LOCAL nLastPing := hb_MilliSeconds()

   DO WHILE netiocli[ _NETIOCLI_lWaitStream ]

      IF ! Empty( netiocli[ _NETIOCLI_pConnection ] )
         IF hb_MilliSeconds() > nLastPing + 5000
            /* Is connection alive? */
            BEGIN SEQUENCE WITH __BreakBlock()
               netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_ping" )
            RECOVER
               hbnetiocon_dispevent( netiocli, "Connection lost." )
               EXIT
            END SEQUENCE
            nLastPing := hb_MilliSeconds()
         ENDIF
      ENDIF

      IF netiocli[ _NETIOCLI_nStreamID ] != NIL
         IF ( xList := netio_GetData( netiocli[ _NETIOCLI_nStreamID ] ) ) != NIL
            IF HB_ISARRAY( xList )
               FOR EACH xItem IN xList
                  xRetVal := Eval( bBlock, netiocli[ _NETIOCLI_nStreamID ], xItem )
                  IF HB_ISLOGICAL( xRetVal ) .AND. ! xRetVal
                     lExit := .T.
                     EXIT
                  ENDIF
               NEXT
               IF lExit
                  EXIT
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      hb_idleSleep( 0.2 )
   ENDDO

   RETURN

/* connect to server */
STATIC PROCEDURE ConnectLow( netiocli, cIP, nPort, cPassword )

   hbnetiocon_dispevent( netiocli, hb_StrFormat( "Connecting to hbnetio server management at %1$s:%2$d...", cIP, nPort ) )

   netiocli[ _NETIOCLI_pConnection ] := netio_GetConnection( cIP, nPort,, cPassword )
   cPassword := NIL

   IF ! Empty( netiocli[ _NETIOCLI_pConnection ] )

      netiocli[ _NETIOCLI_cIP ] := cIP
      netiocli[ _NETIOCLI_nPort ] := nPort
      netiocli[ _NETIOCLI_nStreamID ] := NIL

      netiocli[ _NETIOCLI_lWaitStream ] := .T.
      netiocli[ _NETIOCLI_hWaitThread ] := hb_threadStart( {| ... | hbnetiocon_waitStream( netiocli, {| nStreamID, xItem | HB_SYMBOL_UNUSED( nStreamID ), hbnetiocon_acceptStreamData( netiocli, xItem ) } ) } )

      netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_setclientinfo", MyClientInfo() )
      netiocli[ _NETIOCLI_nStreamID ] := netio_OpenItemStream( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_regnotif", .T. )

      hbnetiocon_dispevent( netiocli, "Connected." )
   ELSE
      hbnetiocon_dispevent( netiocli, "Error connecting server." )
   ENDIF

   RETURN

STATIC PROCEDURE DisconnectLow( netiocli )

   IF ! Empty( netiocli[ _NETIOCLI_pConnection ] )

      netio_OpenItemStream( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_regnotif", .F. )

      netiocli[ _NETIOCLI_lWaitStream ] := .F.
      hb_threadJoin( netiocli[ _NETIOCLI_hWaitThread ] )

      netiocli[ _NETIOCLI_pConnection ] := NIL

      hbnetiocon_dispevent( netiocli, "Connection closed." )
   ENDIF

   RETURN

STATIC FUNCTION MyClientInfo()
   RETURN { ;
      "OS()"          => OS()          , ;
      "Version()"     => Version()     , ;
      "hb_Compiler()" => hb_Compiler() , ;
      "NetName()"     => NetName()     , ;
      "hb_UserName()" => hb_UserName() }

STATIC FUNCTION XToStrX( xValue )

   LOCAL cType := ValType( xValue )

   LOCAL tmp
   LOCAL cRetVal

   SWITCH cType
   CASE "C"

      xValue := StrTran( xValue, Chr(  0 ), '" + Chr(  0 ) + "' )
      xValue := StrTran( xValue, Chr(  9 ), '" + Chr(  9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN xValue

   CASE "N" ; RETURN hb_ntos( xValue )
   CASE "D" ; RETURN DToC( xValue )
   CASE "T" ; RETURN hb_TToC( xValue )
   CASE "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE "O" ; RETURN xValue:className() + " Object"
   CASE "U" ; RETURN "NIL"
   CASE "B" ; RETURN '{||...} -> ' + XToStrX( Eval( xValue ) )
   CASE "A"

      cRetVal := '{ '

      FOR EACH tmp IN xValue
         cRetVal += XToStrX( tmp )
         IF ! tmp:__enumIsLast()
            cRetVal += ", "
         ENDIF
      NEXT

      RETURN cRetVal + ' }'

   CASE "H"

      cRetVal := '{ '

      FOR EACH tmp IN xValue
         cRetVal += tmp:__enumKey() + " => " + XToStrX( tmp )
         IF ! tmp:__enumIsLast()
            cRetVal += ", "
         ENDIF
      NEXT

      RETURN cRetVal + ' }'

   CASE "M" ; RETURN 'M:' + xValue
   ENDSWITCH

   RETURN ""

/* Commands */

STATIC PROCEDURE cmdAbout( netiocli )

   hbnetiocon_dispevent( netiocli, "Harbour NETIO Server Management Console " + StrTran( Version(), "Harbour " ) )
   hbnetiocon_dispevent( netiocli, "Copyright (c) 2009-" + ;
      "2015" + ", " + ;
      "Viktor Szakats" )
   hbnetiocon_dispevent( netiocli, hb_Version( HB_VERSION_URL_BASE ) )

   RETURN

STATIC PROCEDURE cmdHelp( netiocli )

   LOCAL hCommands := netiocli[ _NETIOCLI_hCommands ]
   LOCAL aTexts := {}
   LOCAL n, m

   m := 0
   hb_HEval( hCommands, {| k, l | m := Max( m, Len( k + iif( Empty( l[ 1 ] ), "", " " + l[ 1 ] ) ) ) } )

   AAdd( aTexts, "Commands:" )

   /* Processing commands */
   FOR EACH n IN hCommands
      AAdd( aTexts, " " + PadR( n:__enumKey() + iif( Empty( n[ 1 ] ), "", " " + n[ 1 ] ), m ) + " - " + n[ 2 ] )
   NEXT

   ASort( aTexts, 2 )

   FOR EACH n IN aTexts
      hbnetiocon_dispevent( netiocli, n )
   NEXT

   RETURN

STATIC PROCEDURE cmdConnect( netiocli, cCommand )

   LOCAL cIP
   LOCAL nPort
   LOCAL aToken
   LOCAL cPassword
   LOCAL nPortOld

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )

      aToken := hb_ATokens( cCommand, " " )

      IF Len( aToken ) >= 2
         nPortOld := nPort
         hbnetiocon_IPPortSplit( aToken[ 2 ], @cIP, @nPort )
         IF Empty( nPort )
            nPort := nPortOld
         ENDIF
      ENDIF
      IF Len( aToken ) >= 3
         cPassword := aToken[ 3 ]
      ELSE
         cPassword := Eval( netiocli[ _NETIOCLI_hConIO ][ "gethidden" ] )
      ENDIF

      ConnectLow( netiocli, cIP, nPort, cPassword )
   ELSE
      hbnetiocon_dispevent( netiocli, "Already connected. Disconnect first." )
   ENDIF

   RETURN

STATIC PROCEDURE cmdDisconnect( netiocli )

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      DisconnectLow( netiocli )
   ENDIF

   RETURN

STATIC PROCEDURE cmdSysInfo( netiocli )

   LOCAL aArray
   LOCAL cLine

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      aArray := netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_sysinfo" )
      IF HB_ISARRAY( aArray )
         FOR EACH cLine IN aArray
            hbnetiocon_dispevent( netiocli, cLine )
         NEXT
      ELSE
         hbnetiocon_dispevent( netiocli, "Error: RPC call failed." )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdServerConfig( netiocli )

   LOCAL aArray
   LOCAL cLine

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      aArray := netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_serverconfig" )
      IF HB_ISARRAY( aArray )
         FOR EACH cLine IN aArray
            hbnetiocon_dispevent( netiocli, cLine )
         NEXT
      ELSE
         hbnetiocon_dispevent( netiocli, "Error: RPC call failed." )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnStop( netiocli, cCommand )

   LOCAL aToken

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      aToken := hb_ATokens( cCommand, " " )
      IF Len( aToken ) > 1
         netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_stop", aToken[ 2 ] )
      ELSE
         hbnetiocon_dispevent( netiocli, "Error: Invalid syntax." )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnClientInfo( netiocli, cCommand )

   LOCAL aToken
   LOCAL xCargo

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      aToken := hb_ATokens( cCommand, " " )
      IF Len( aToken ) > 1
         xCargo := netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_clientinfo", aToken[ 2 ] )
         IF xCargo == NIL
            hbnetiocon_dispevent( netiocli, "No information" )
         ELSE
            hbnetiocon_dispevent( netiocli, hb_StrFormat( "%1$s", XToStrX( xCargo ) ) )
         ENDIF
      ELSE
         hbnetiocon_dispevent( netiocli, "Error: Invalid syntax." )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnInfo( netiocli, lManagement )

   LOCAL aArray
   LOCAL hConn

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      aArray := netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], iif( lManagement, "hbnetiomgm_adminfo", "hbnetiomgm_conninfo" ) )
      IF HB_ISARRAY( aArray )
         hbnetiocon_dispevent( netiocli, hb_StrFormat( "Number of connections: %1$d", Len( aArray ) ) )

         FOR EACH hConn IN aArray
            hbnetiocon_dispevent( netiocli, "#" + PadR( hb_ntos( hConn[ "nThreadID" ] ), Len( Str( hConn[ "nThreadID" ] ) ) ) + " " + ;
               hb_TToC( hConn[ "tStart" ] ) + " " + ;
               PadR( hConn[ "cStatus" ], 12 ) + " " + ;
               "fcnt: " + Str( hConn[ "nFilesCount" ] ) + " " + ;
               "send: " + Str( hConn[ "nBytesSent" ] ) + " " + ;
               "recv: " + Str( hConn[ "nBytesReceived" ] ) + " " + ;
               hConn[ "cAddressPeer" ] + " " + ;
               iif( "xCargo" $ hconn, hb_ValToStr( hConn[ "xCargo" ] ), "" ) )
         NEXT
      ELSE
         hbnetiocon_dispevent( netiocli, "Error: RPC call failed." )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdShutdown( netiocli )

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_shutdown" )
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnEnable( netiocli, lValue )

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_conn", lValue )
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnLogEnable( netiocli, lValue )

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_logconn", lValue )
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnFilterMod( netiocli, cCommand, cRPC )

   LOCAL lResult
   LOCAL aToken

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      aToken := hb_ATokens( cCommand, " " )
      IF Len( aToken ) > 1
         lResult := netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], cRPC, aToken[ 2 ] )
         IF HB_ISLOGICAL( lResult )
            IF lResult
               hbnetiocon_dispevent( netiocli, "Done" )
            ELSE
               hbnetiocon_dispevent( netiocli, "Failed" )
            ENDIF
         ELSE
            hbnetiocon_dispevent( netiocli, "Error: RPC call failed." )
         ENDIF
      ELSE
         hbnetiocon_dispevent( netiocli, "Error: Invalid syntax." )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnFilters( netiocli, lManagement )

   LOCAL aArray
   LOCAL hFilter

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      aArray := netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], iif( lManagement, "hbnetiomgm_filtersadmin", "hbnetiomgm_filters" ) )
      IF HB_ISARRAY( aArray )
         FOR EACH hFilter IN aArray
            hbnetiocon_dispevent( netiocli, hFilter[ "cType" ], ;
               hFilter[ "cAddress" ] )
         NEXT
      ELSE
         hbnetiocon_dispevent( netiocli, "Error: RPC call failed." )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnFilterSave( netiocli )

   IF Empty( netiocli[ _NETIOCLI_pConnection ] )
      hbnetiocon_dispevent( netiocli, "Not connected." )
   ELSE
      netio_FuncExec( netiocli[ _NETIOCLI_pConnection ], "hbnetiomgm_filtersave" )
   ENDIF

   RETURN