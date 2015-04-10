(***********************************************************
    Mock Projector
    
    For testing syntax highlighting
************************************************************)

#if_not_defined MOCK_PROJECTOR
#define MOCK_PROJECTOR 1
(***********************************************************)
(* System Type : NetLinx                                   *)
(***********************************************************)
(*           DEVICE NUMBER DEFINITIONS GO BELOW            *)
(***********************************************************)
DEFINE_DEVICE

dvPROJECTOR = 5001:1:0;

(***********************************************************)
(*              CONSTANT DEFINITIONS GO BELOW              *)
(***********************************************************)
DEFINE_CONSTANT

// Power States
POWER_STATE_ON      = 0;
POWER_STATE_OFF     = 1;
POWER_STATE_WARMING = 2;
POWER_STATE_COOLING = 3;

// Inputs
INPUT_HDMI          = 0;
INPUT_VGA           = 1;
INPUT_COMPOSITE     = 2;
INPUT_SVIDEO        = 3;

(***********************************************************)
(*                    INCLUDES GO BELOW                    *)
(***********************************************************)

#include 'amx-lib-log'

(***********************************************************)
(*              DATA TYPE DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_TYPE

struct projector_t
{
    integer power_state;
    integer input;
    integer lamp_hours;
}

(***********************************************************)
(*              VARIABLE DEFINITIONS GO BELOW              *)
(***********************************************************)
DEFINE_VARIABLE

volatile projector_t proj_1;

(***********************************************************)
(*         SUBROUTINE/FUNCTION DEFINITIONS GO BELOW        *)
(***********************************************************)

define_function initialize(projector_t self)
{
    self.power_state = POWER_STATE_OFF;
    self.input = INPUT_HDMI;
    self.lamp_hours = 0;
}

define_function switch_input(projector_t self, integer input)
{
    self.input = input;
    print(LOG_LEVEL_INFO, "'Projector set to input: ', itoa(input)");
}

(***********************************************************)
(*                 STARTUP CODE GOES BELOW                 *)
(***********************************************************)
DEFINE_START

initialize(proj_1);

(***********************************************************)
(*                   THE EVENTS GO BELOW                   *)
(***********************************************************)
DEFINE_EVENT

data_event[dvPROJECTOR]
{
    string:
    {
        parse_message(data.text);
    }
    
    command: {}
    online:  {}
    offline: {}
}

button_event[dvTP, BTN_HDMI]
button_event[dvTP, BTN_VGA]
button_event[dvTP, BTN_COMPOSITE]
button_event[dvTP, BTN_SVIDEO]
{
    push:
    {
        switch (button.input.channel)
        {
            case BTN_HDMI:      switch_input(proj_1, INPUT_HDMI);
            case BTN_VGA:       switch_input(proj_1, INPUT_VGA);
            case BTN_COMPOSITE: switch_input(proj_1, INPUT_COMPOSITE);
            case BTN_SVIDEO:    switch_input(proj_1, INPUT_SVIDEO);
        }
    }
    
    release: {}
}

(***********************************************************)
(*                 THE MAINLINE GOES BELOW                 *)
(***********************************************************)
DEFINE_PROGRAM

[dvTP, BTN_POWER_ON]  = (proj_1.power_state == POWER_STATE_ON);
[dvTP, BTN_POWER_OFF] = (proj_1.power_state == POWER_STATE_OFF);

(***********************************************************)
(*                     END OF PROGRAM                      *)
(*          DO NOT PUT ANY CODE BELOW THIS COMMENT         *)
(***********************************************************)
#end_if
