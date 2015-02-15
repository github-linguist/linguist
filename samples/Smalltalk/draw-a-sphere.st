Point3D :=
   Point subclass:#Point3D
        instanceVariableNames:'z'
        classVariableNames:''
        poolDictionaries:''
        category:''
        inEnvironment:nil.

Point3D compile:'z ^ z'.
Point3D compile:'z:v z := v'.

normalize := [:v | |invLen|
    invLen := 1 / (dot value:v value:v) sqrt.
    v x: v x * invLen.
    v y: v y * invLen.
    v z: v z * invLen.
].

dot := [:a :b |
    (a x * b x) + (a y * b y) + (a z * b z)
].

drawSphere := [:r :k :amb :dir |
    |w h imh vec img|

    w := r*4. h := r*3.
    img := Image width:w height:h depth:8.
    img photometric:#blackIs0; createPixelStore.
    vec := Point3D new.
    0-r to:r do:[:x |
        0-r to:r do:[:y |
            |z s lum|
            (z := (r*r) - (x*x) - (y*y)) >= 0 ifTrue:[
                vec x: x.
                vec y: y.
                vec z: z sqrt.
                normalize value:vec.
                s := dot value:dir value:vec.
                s < 0 ifTrue:[ s := 0 ].
                lum := 255 * ((s raisedTo: k) + amb) / (1 + amb).
                lum < 0 ifTrue:[
                    lum := 0
                ] ifFalse:[ lum > 255 ifTrue:[
                    lum := 255
                ]].
                img atX:(x+(w//2)) y:(y+(h//2)) put:(Color greyByte:lum).
            ]
        ]
    ].
    img
].

main := [
    |dir img|

    dir := Point3D new x:-30; y:-30; z:50; yourself.
    normalize value:dir.
    img := drawSphere value: 100 value: 1.5 value: 0.2 value: dir.
    img displayOn:(View new extent:400@400; openAndWait).
    img saveOn:'sphere.png'.
].

main value.
