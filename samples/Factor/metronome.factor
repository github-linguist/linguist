USING: accessors calendar circular colors.constants colors.hsv
command-line continuations io kernel math math.parser namespaces
openal.example sequences system timers ui ui.gadgets
ui.pens.solid ;
IN: rosetta-code.metronome

: bpm>duration ( bpm -- duration ) 60 swap / seconds ;

: blink-gadget ( gadget freq -- )
    1.0 1.0 1.0 <hsva>  <solid> >>interior relayout-1 ;

: blank-gadget ( gadget -- )
    COLOR: white <solid> >>interior relayout-1 ;

: play-note ( gadget freq -- )
    [ blink-gadget ] [ 0.3 play-sine blank-gadget ] 2bi ;

: metronome-iteration ( gadget circular -- )
    [ first play-note ] [ rotate-circular ] bi ;

TUPLE: metronome-gadget < gadget bpm notes timer ;

: <metronome-gadget> ( bpm notes -- gadget )
    \ metronome-gadget new swap >>notes swap >>bpm ;

: metronome-quot ( gadget -- quot )
    dup notes>> <circular> [ metronome-iteration ] 2curry ;

: metronome-timer ( gadget -- timer )
    [ metronome-quot ] [ bpm>> bpm>duration ] bi every ;

M: metronome-gadget graft* ( gadget -- )
    [ metronome-timer ] keep timer<< ;

M: metronome-gadget ungraft*
    timer>> stop-timer ;

M: metronome-gadget pref-dim* drop { 200 200 } ;

: metronome-defaults ( -- bpm notes ) 60 { 440 220 330 } ;

: metronome-ui ( bpm notes -- ) <metronome-gadget> "Metronome" open-window ;

: metronome-example ( -- ) metronome-defaults metronome-ui ;

: validate-args ( int-args -- )
    [ length 2 < ] [ [ 0 <= ] any? ] bi or [ "args error" throw ] when ;

: (metronome-cmdline) ( args -- bpm notes )
    [ string>number ] map dup validate-args
    unclip swap ;

: metronome-cmdline ( -- bpm notes )
    command-line get [ metronome-defaults ] [ (metronome-cmdline) ] if-empty ;

: print-defaults ( -- )
  metronome-defaults swap prefix
  [ " " write ] [ number>string write ] interleave nl ;

: metronome-usage ( -- )
    "Usage: metronome [BPM FREQUENCIES...]" print
    "Arguments must be non-zero" print
    "Example: metronome " write print-defaults flush ;

: metronome-main ( -- )
     [ [ metronome-cmdline metronome-ui ] [ drop metronome-usage 1 exit ] recover ] with-ui ;

MAIN: metronome-main
