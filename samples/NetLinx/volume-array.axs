(***********************************************************
    AMX VOLUME CONTROL
    VOLUME ARRAY EXAMPLE
    
    Website: https://sourceforge.net/projects/amx-lib-volume/
    
    
    This application demonstrates the use of volume control
    arrays using the amx-lib-volume library.
    
    Volume control operation can be viewed by watching the
    master's internal diagnostic output.
    
    I/O PORT CONNECTIONS:
    Ch 1: Volume Up Button
    Ch 2: Volume Down Button
************************************************************
Copyright 2011, 2012, 2014 Alex McLain

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************)

PROGRAM_NAME='volume array'
(***********************************************************)
(***********************************************************)
(* System Type : NetLinx                                   *)
(***********************************************************)
(* REV HISTORY:                                            *)
(***********************************************************)
(*
    $History: See version control repository.
*)
(***********************************************************)
(*                   INCLUDES GO BELOW                     *)
(***********************************************************)

// Include the volume control library.
#include 'amx-lib-volume'

(***********************************************************)
(*          DEVICE NUMBER DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_DEVICE

dvDebug = 0:0:0;        // For debug output.

dvIO    = 36000:1:0;    // Volume up/down button connections.

(***********************************************************)
(*               CONSTANT DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_CONSTANT

// Volume control indexes.
MIC1    = 1;    // Microphone 1.
MIC2    = 2;    // Microphone 2.
MIC3    = 3;    // Microphone 3.
MIC4    = 4;    // Microphone 4.
WLS1    = 5;    // Wireless mic 1.
WLS2    = 6;    // Wireless mic 2.
IPOD    = 7;    // iPod input.
CD      = 8;    // CD player input.

(***********************************************************)
(*              DATA TYPE DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_TYPE

(***********************************************************)
(*               VARIABLE DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_VARIABLE

// Define a volume control array for the input devices.
volume inputs[8];

(***********************************************************)
(*               LATCHING DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_LATCHING

(***********************************************************)
(*       MUTUALLY EXCLUSIVE DEFINITIONS GO BELOW           *)
(***********************************************************)
DEFINE_MUTUALLY_EXCLUSIVE

(***********************************************************)
(*        SUBROUTINE/FUNCTION DEFINITIONS GO BELOW         *)
(***********************************************************)
(* EXAMPLE: DEFINE_FUNCTION <RETURN_TYPE> <NAME> (<PARAMETERS>) *)
(* EXAMPLE: DEFINE_CALL '<NAME>' (<PARAMETERS>) *)

(***********************************************************)
(*                STARTUP CODE GOES BELOW                  *)
(***********************************************************)
DEFINE_START

// Initialize the array of volume controls.
volArrayInit(inputs, 0, VOL_UNMUTED, 10000, 20000, 5);

(***********************************************************)
(*                THE EVENTS GO BELOW                      *)
(***********************************************************)
DEFINE_EVENT

// Volume Up
button_event[dvIO, 1]
{
    PUSH:
    {
        volArrayIncrement(inputs); // Increment the volume up a step.
        send_string dvDebug, "'Volume Up MIC1: ', itoa(volGetLevel(inputs[MIC1]))";
        send_string dvDebug, "'Volume Up MIC2: ', itoa(volGetLevel(inputs[MIC2]))";
        send_string dvDebug, "'Volume Up MIC3: ', itoa(volGetLevel(inputs[MIC3]))";
        send_string dvDebug, "'Volume Up MIC4: ', itoa(volGetLevel(inputs[MIC4]))";
        send_string dvDebug, "'Volume Up WLS1: ', itoa(volGetLevel(inputs[WLS1]))";
        send_string dvDebug, "'Volume Up WLS2: ', itoa(volGetLevel(inputs[WLS2]))";
        send_string dvDebug, "'Volume Up IPOD: ', itoa(volGetLevel(inputs[IPOD]))";
        send_string dvDebug, "'Volume Up   CD: ', itoa(volGetLevel(inputs[CD]))";
    }
}

// Volume Down
button_event[dvIO, 2]
{
    PUSH:
    {
        volArrayDecrement(inputs); // Decrement the volume down a step.
        send_string dvDebug, "'Volume Dn MIC1: ', itoa(volGetLevel(inputs[MIC1]))";
        send_string dvDebug, "'Volume Dn MIC2: ', itoa(volGetLevel(inputs[MIC2]))";
        send_string dvDebug, "'Volume Dn MIC3: ', itoa(volGetLevel(inputs[MIC3]))";
        send_string dvDebug, "'Volume Dn MIC4: ', itoa(volGetLevel(inputs[MIC4]))";
        send_string dvDebug, "'Volume Dn WLS1: ', itoa(volGetLevel(inputs[WLS1]))";
        send_string dvDebug, "'Volume Dn WLS2: ', itoa(volGetLevel(inputs[WLS2]))";
        send_string dvDebug, "'Volume Dn IPOD: ', itoa(volGetLevel(inputs[IPOD]))";
        send_string dvDebug, "'Volume Dn   CD: ', itoa(volGetLevel(inputs[CD]))";
    }
}

(***********************************************************)
(*            THE ACTUAL PROGRAM GOES BELOW                *)
(***********************************************************)
DEFINE_PROGRAM

(***********************************************************)
(*                     END OF PROGRAM                      *)
(*        DO NOT PUT ANY CODE BELOW THIS COMMENT           *)
(***********************************************************)
