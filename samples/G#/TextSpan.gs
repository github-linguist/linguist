/*
Source: https://github.com/DavidObando/gsharp/blob/a1190b9b77cd8c8aecf5b90cbd6774f26489989e/src/Core/CodeAnalysis/Text/TextSpan.gs
License: https://github.com/DavidObando/gsharp/blob/a1190b9b77cd8c8aecf5b90cbd6774f26489989e/LICENSE

MIT License

Copyright (c) 2019 David Obando

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

// <copyright file="TextSpan.cs" company="GSharp">
// Copyright (C) GSharp Authors. All rights reserved.
// </copyright>

package GSharp.Core.CodeAnalysis.Text

import System

/// An abstract representation of a span with a start and an end index.
struct TextSpan : IComparable[TextSpan] {
    /// Initializes a new instance of the (cref:TextSpan) struct.
    /// @param start Start index.
    /// @param length Length.
    init(start int32, length int32) {
        Start = start
        Length = length
    }

    /// Gets the start index of the span.
    prop Start int32 {
        get;
    }

    /// Gets the length of the span.
    prop Length int32 {
        get;
    }

    /// Gets the end of the span.
    prop End int32 -> Start + Length

    /// Compares two text spans, useful for sorting sets of text spans.
    /// @param other The text span to compare to.
    /// @returns A value indicating the relation of the first text span to the second one.
    func CompareTo(other TextSpan) int32 {
        var cmp = Start - other.Start
        if cmp == 0 {
            cmp = Length - other.Length
        }
        return cmp
    }

    /// A string representation of the span.
    /// @returns A string containing the start and end index of the span.
    override func ToString() string -> "$Start..$End"

    shared {
        /// Creates a new span given the specified start and end boundaries.
        /// @param start The start boundary.
        /// @param end The end boundary.
        /// @returns A materialized span.
        func FromBounds(start int32, end int32) TextSpan {
            let length = end - start
            return TextSpan(start, length)
        }
    }
}
