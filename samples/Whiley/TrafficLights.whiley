// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//    * Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above copyright
//      notice, this list of conditions and the following disclaimer in the
//      documentation and/or other materials provided with the distribution.
//    * Neither the name of the <organization> nor the
//      names of its contributors may be used to endorse or promote products
//      derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL DAVID J. PEARCE BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// the British interpretation of traffic lights!
type TrafficLights is {
    bool red,
    bool amber,
    bool green    
} where (!red && !amber && green) ||
        (!red && amber && !green) || // ignoring flashing
        (red && !amber && !green) ||
        (red && amber && !green)

function TrafficLights() => TrafficLights:
    return {
        red: true,
        amber: false,
        green: false
    }

function change(TrafficLights ls) => TrafficLights:
    if ls.green:
        // -> !red && !amber && green
        return { red: false, amber: true, green: false }
    else if ls.red:
        // -> red && ~amber && !green
        if ls.amber:
            // -> red && amber && !green
            return { red: false, amber: false, green: true }
        else:
            return { red: true, amber: true, green: false }
    else:
        // -> !red && amber && !green
        return { red: true, amber: false, green: false }

function toString(TrafficLights ls) => string:
    string r
    //
    if ls.red:
        r = "RED "
    else:
        r = "    "
    if ls.amber:
        r = r ++ "AMBER "
    else:
        r = r ++ "       "
    if ls.green:
        r = r ++ "GREEN "
    else:
        r = r ++ "      "
    return r

public method main(System.Console console):
    TrafficLights lights = TrafficLights()
    console.out.println(toString(lights))
    lights = change(lights)
    console.out.println(toString(lights))
    lights = change(lights)
    console.out.println(toString(lights))
    lights = change(lights)
    console.out.println(toString(lights))
    lights = change(lights)
    console.out.println(toString(lights))
