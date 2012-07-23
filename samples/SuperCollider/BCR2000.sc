BCR2000 {
  var controls,
      controlBuses,
      rangedControlBuses,
      responders
  ;

  *new {
    ^super.new.init;
  }
    
  init {
    controls = Dictionary.new(108);
    controlBuses = Dictionary.new(108);
    rangedControlBuses = Dictionary.new(108);

    this.createCCResponders;
  }

  createCCResponders {
    responders = Array.fill(108, {|i|
      CCResponder({|src, chan, num, val|
        [src, chan, num, val].postln;

        // Write to controls 
        controls.put(i + 1, val);

        // Write to bus (converted to scalar 0..1)
        controlBuses.put(i + 1, Bus.control(Server.default));
        controlBuses.at(i + 1).value = val / 127;
      },
        // Adjust values as/if needed
        nil, // src
        nil, // chan
        nil, // num
        nil  // value    
      )
    });
  }

  // Value from BCR
  at {arg controlNum;
    ^controls.at(controlNum)
  }

  // Convert to 0..1
  scalarAt {arg controlNum; 
    ^controls.at(controlNum) / 127
  }

  // Get a bus
  busAt {arg controlNum;
    ^controlBuses.at(controlNum)
  }

  /*
  busRangeAt(arg controlNum, lo, hi;
    if (rangedControlBuses.at(controlNum).isNil, {
      rangedControlBuses.put(controlNum, Bus.control(Server.default))
    }); 
   
    // Left to right order of operations
    //rangedControlBuses.put(
    bus.value = hi - lo * controls.at(controlNum) + lo;

    ^bus
  }
  */
}

/* Scratch
Dictionary
b = BCR2000();
b.at(4);
b.scalarAt(4);
b.controls[5].get;
throw
z = Dictionary.new(2);
z.at(\1);
Array.fill(10, {|i| i.postln;})
(2 + 3).asSymbol;


SynthDef(\x, {
  arg amp = 0.01,
      freq = 1200,
      modDepth = 0.7,
      modFreq = 2
  ;

  var
    carrier,
    modulator   
  ;

  modulator = SinOsc.ar(modFreq, mul: modDepth);
  carrier = Saw.ar(freq, add: modulator, mul: amp);

  Out.ar([0,1], carrier)
}).store; 


x = Synth(\x);
x.set(\modDepth, 1);
x.set(\modFreq, 64); 

x.map(\modFreq, b.busAt(       



ControlSpec
*/


