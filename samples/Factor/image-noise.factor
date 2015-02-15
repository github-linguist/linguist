USING: accessors calendar images images.viewer kernel math
math.parser models models.arrow random sequences threads timers
ui.gadgets ui.gadgets.labels ui.gadgets.packs ;
IN: bw-noise

CONSTANT: pixels { B{ 0 0 0 } B{ 255 255 255 } }

: <random-images-bytes> ( dim -- bytes )
    product [ pixels random ] { } replicate-as concat ;

: <random-bw-image> ( -- image )
 <image>
  { 320 240 } [ >>dim ] [ <random-images-bytes> >>bitmap ] bi
  RGB >>component-order
  ubyte-components >>component-type ;

TUPLE: bw-noise-gadget < image-control timers cnt old-cnt fps-model ;

: animate-image ( control -- )
    [ 1 + ] change-cnt
    model>> <random-bw-image> swap set-model ;

: update-cnt ( gadget -- )
    [ cnt>> ] [ old-cnt<< ] bi ;
: fps ( gadget -- fps )
    [ cnt>> ] [ old-cnt>> ] bi - ;
: fps-monitor ( gadget -- )
    [ fps ] [ update-cnt ] [ fps-model>> set-model ] tri ;

: start-animation ( gadget -- )
    [ [ animate-image ] curry 1 nanoseconds every ] [ timers>> push ] bi ;
: start-fps ( gadget -- )
    [ [ fps-monitor ] curry 1 seconds every ] [ timers>> push ] bi ;
: setup-timers ( gadget -- )
    [ start-animation ] [ start-fps ] bi ;
: stop-animation ( gadget -- )
    timers>> [ [ stop-timer ] each ] [ 0 swap set-length ] bi ;

M: bw-noise-gadget graft* [ call-next-method ] [ setup-timers ] bi ;
M: bw-noise-gadget ungraft* [ stop-animation ] [ call-next-method ] bi ;

: <bw-noise-gadget> ( -- gadget )
    <random-bw-image> <model> bw-noise-gadget new-image-gadget*
    0 >>cnt 0 >>old-cnt 0 <model> >>fps-model V{ } clone >>timers ;
: fps-gadget ( model -- gadget )
    [ number>string ] <arrow> <label-control>
    "FPS: " <label>
    <shelf> swap add-gadget swap add-gadget ;

: with-fps ( gadget -- gadget' )
    [ fps-model>> fps-gadget ]
    [ <pile> swap add-gadget swap add-gadget ] bi ;

: open-noise-window ( -- ) [ <bw-noise-gadget> with-fps "Black and White noise" open-window ] with-ui ;
