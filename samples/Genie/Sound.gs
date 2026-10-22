/*
Source: https://github.com/darkoverlordofdata/libbosco/blob/996aad047203c60a2118250095f59d238769bbbe/src/Sound.gs
License: https://github.com/darkoverlordofdata/libbosco/blob/996aad047203c60a2118250095f59d238769bbbe/license.md

# MIT License

Copyright (c) 2015-2016 Bruce Davidson &lt;darkoverlordofdata@gmail.com&gt;

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Sound.gs
 *
 * Wrapper for Mix_Chunk.WAV sound effect
 *
 * Copyright 2016 Dark Overlord of Data
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the he MIT License (MIT).
 *
 * Author:
 *      bruce davidson
 */
[indent=4]

uses SDL
uses SDLMixer
uses GLib

namespace Bosco

    class Sound : Object

        chunk : Chunk

        /**
         *  load a sound effect from a file or resource
         *
         * @param path of the sprite file or resource
         * @return new Sound
         */
        def static fromFile(path: string) : Sound

            var fx = new Sound()

            if path.index_of("resource:///") == 0
                try
                    var ptr  = GLib.resources_lookup_data(path.substring(11), 0)
                    var rw = new RWops.from_mem((void*)ptr.get_data(), (int)ptr.get_size())
                    fx.chunk = new Chunk.WAV_RW(rw)
                except e: Error
                    print "Error loading resource: %s\n", e.message

            else
                fx.chunk = new Chunk.WAV(path)

            return fx

        
        /**
         *  Render (Play) the sound
         *
         * @param loops to repeat
         */
        def play(loops: int = 0)
            SDLMixer.play(-1, chunk, loops)
