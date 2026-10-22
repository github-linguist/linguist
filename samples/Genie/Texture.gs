/*
Source: https://github.com/darkoverlordofdata/libbosco/blob/996aad047203c60a2118250095f59d238769bbbe/src/Atlas/Texture.gs
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

[indent=4]
/**
 * Texture.gs
 *
 * Copyright 2016 Dark Overlord of Data
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the he MIT License (MIT).
 *
 * Author: 
 *      bruce davidson
 */
uses Gee
uses SDL
uses SDLImage
uses SDLTTF
uses GLib

namespace Bosco

    class Texture : Object
        data: Video.Surface
        prop path: string
        prop width: int
            get
                return data.w
        prop height: int
            get
                return data.h
        construct(path: string)
            _path = path
            if _path.index_of("resource:///") == 0
                try
                    var ptr  = GLib.resources_lookup_data(_path.substring(11), 0)
                    var rw = new RWops.from_mem((void*)ptr.get_data(), (int)ptr.get_size())
                    data = new Video.Surface.from_bmp_rw(rw)

                except e: Error
                    data = null
                    print "Error loading resource: %s\n", e.message

            else
                data = SDLImage.load(_path)

        def setFilter(minFilter: int, magFilter: int)
            pass

        def setWrap(u: int, v: int)
            pass
