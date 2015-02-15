USING: accessors alarms calendar combinators kernel locals math
math.constants math.functions prettyprint system threads ;
IN: rosettacode.active

TUPLE: active-object alarm function state previous-time ;

: apply-stack-effect ( quot -- quot' )
    [ call( x -- x ) ] curry ; inline

: nano-to-seconds ( -- seconds ) nano-count 9 10^ / ;

: object-times ( active-object -- t1 t2 )
    [ previous-time>> ]
    [ nano-to-seconds [ >>previous-time drop ] keep ] bi ;
:: adding-function ( t1 t2 active-object -- function )
    t2 t1 active-object function>> apply-stack-effect bi@ +
    t2 t1 - * 2 / [ + ] curry ;
: integrate ( active-object -- )
    [ object-times ]
    [ adding-function ]
    [ swap apply-stack-effect change-state drop ] tri ;

: <active-object> ( -- object )
    active-object new
    0 >>state
    nano-to-seconds >>previous-time
    [ drop 0 ] >>function
    dup [ integrate ] curry 1 nanoseconds every >>alarm ;
: destroy ( active-object -- ) alarm>> cancel-alarm ;

: input ( object quot -- object ) >>function ;
: output ( object -- val ) state>> ;

: active-test ( -- )
    <active-object>
    [ 2 pi 0.5 * * * sin ] input
    2 seconds sleep
    [ drop 0 ] input
    0.5 seconds sleep
    [ output . ] [ destroy ] bi ;
MAIN: active-test
