// The MIT License (MIT)

// Copyright (c) 2016 dario ureña

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Source: https://github.com/darioaxel/PowerScriptToKDMTransformer/blob/07bf527194b73e133943221dc2199a9442d463eb/resources/inventoryModel/nivel0/ginpix7.sra

HA$PBExportHeader$ginpix7.sra
$PBExportComments$Aplicaci$$HEX1$$f300$$ENDHEX$$n Ginpix7
forward
global type ginpix7 from application
end type
global n_tr_apli sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
n_appmanager  gnv_app
//------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// Satxa - 02-Jun-2009.27760 No tenemos constancia del uso de esta ayuda, asi que la eliminamos.
//n_help     gn_help
//------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------
// Satxa - 17-Jun-2010.42351 No se usa.
//n_help2  gn_help2
//----------------------------------------------------------

//-----------------------------------------------------------------------------------------------------------------------------------------------------------
// Satxa - 15-Jun-2009.27760
//Para el modulo de contratos.
String	gs_enlacemacros, gs_office2000 // Posibles de Pulir.(en estudio).
//-----------------------------------------------------------------------------------------------------------------------------------------------------------
end variables
global type ginpix7 from application
string appname = "ginpix7"
string toolbarframetitle = "ginpix7"
boolean toolbarusercontrol = false
end type
global ginpix7 ginpix7

type prototypes
//------------------------------------------------------------------------------------------------------------------------------------------------------
// Satxa - 15-Jun-2009.27760 Copio la llamada a la API del objeto aplicaci$$HEX1$$f300$$ENDHEX$$n de LCS.
// Satxa - 25-Oct-2005.25577 Viene de LCS y RCS, abre un archivo independientemente del tipo ejecutando el programa asociado en WINDOWS
Public Function long ShellExecuteA(long hwnd, string lpOperation, string lpFile, string lpParameters, string lpDirectory, long nShowCmd) Library "SHELL32.DLL" alias for "ShellExecuteA;Ansi"
//------------------------------------------------------------------------------------------------------------------------------------------------------

//
// Fernando Baldellou.14/05/2009. Ginpix7.// Funciones Externas Globales para el Proyecto de Mensajer$$HEX1$$ed00$$ENDHEX$$a.
//
Public Function Long GetWindowRect (Long hWnd, Ref mensajeria lpRect) Library "user32" alias for "GetWindowRect;Ansi"
Public Function Long GetClientRect (Long hWnd, Ref mensajeria lpRect) Library "user32" alias for "GetClientRect;Ansi"
Public Function Long SetWindowPos (Long hWnd, Long hWndInsertAfter, Long x, Long y, Long cx, Long cy, Long wFlags) Library "user32"
Public Function Boolean sndPlaySoundA (String SoundName, uint Flags)Library "winmm.dll" alias for "sndPlaySoundA;Ansi" 
// Fin de la anotaci$$HEX1$$f300$$ENDHEX$$n anterior.


end prototypes

type variables

end variables

on ginpix7.create
appname="ginpix7"
message=create message
sqlca=create n_tr_apli
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on ginpix7.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;// Inicializa propiedades del objeto aplicaci$$HEX1$$f300$$ENDHEX$$n:
This.MicroHelpDefault = "SAVIA"		

//----------------------------------------------------------
// Satxa - 17-Jun-2010.42351 No se usa.
// Crea el servicio de ayuda nivel2:
//IF NOT IsValid(gn_help2) THEN
//   gn_help2 = CREATE n_help2
//END IF
//----------------------------------------------------------

// Presenta la aplicaci$$HEX1$$f300$$ENDHEX$$n:
OpenWithParm(w_ini_ginpix7,commandline)
//OpenWithParm(w_logontab,commandline)


// PORQU$$HEX2$$c9002000$$ENDHEX$$RETRASAMOS LA INSTANCIACI$$HEX1$$d300$$ENDHEX$$N DEL MANEJADOR DE LA APLICACI$$HEX1$$d300$$ENDHEX$$N "n_appmanager" ???
//
// El CNVUO n_appmanager conlleva consigo el proceso de apertura de la ventana w_logon de
// conexi$$HEX1$$f300$$ENDHEX$$n a la base de datos. Nosotros sin embargo, deseamos presentar previamente 
// nuestra ventana de presentaci$$HEX1$$f300$$ENDHEX$$n "w_ini".
//
// Si instanciamos n_appmanager aqu$$HEX1$$ed00$$ENDHEX$$, abre "w_ini" pero pasa de largo y no se "queda" en la
// ventana esperando que pulsemos <CONTINUAR> o bien <SALIR>, ll$$HEX1$$e900$$ENDHEX$$ndose directamente a la 
// apertura de "w_logon" y requirimientos de datos para la conexi$$HEX1$$f300$$ENDHEX$$n a la base de datos.
//
// Por eso instanciamos el CNVUO n_appmanager en el evento w_ini::cb_ini_con::clicked, para
// retrasar este proceso de conexi$$HEX1$$f300$$ENDHEX$$n a base de datos.
//
//gnv_app = CREATE n_appmanager
//gnv_app.Event pfc_Open(commandline)


end event

event close;//----------------------------------------------------------
// Satxa - 17-Jun-2010.42351 No se usa.
// Destruye el servicio de ayuda nivel2:
//IF IsValid(gn_help2) THEN
//   DESTROY n_help2
//END IF
//----------------------------------------------------------

// Destruye el manejador de aplicaci$$HEX1$$f300$$ENDHEX$$n:
IF IsValid(gnv_app) THEN
   DESTROY gnv_app
END IF

// Redirige el evento al manejador de aplicaci$$HEX1$$f300$$ENDHEX$$n:
IF IsValid(gnv_app) THEN
   gnv_app.Event pfc_Close()
END IF

end event

event connectionbegin;// Redirige el evento al manejador de aplicaci$$HEX1$$f300$$ENDHEX$$n:
RETURN gnv_app.Event pfc_ConnectionBegin(userid,password,connectstring)

end event

event connectionend;// Redirige el evento al manejador de aplicaci$$HEX1$$f300$$ENDHEX$$n:
gnv_app.Event pfc_ConnectionEnd()


end event

event idle;// Redirige el evento al manejador de aplicaci$$HEX1$$f300$$ENDHEX$$n:
gnv_app.Event pfc_Idle()
end event

event systemerror;// Redirige el evento al manejador de aplicaci$$HEX1$$f300$$ENDHEX$$n:
gnv_app.Event pfc_SystemError()
end event

