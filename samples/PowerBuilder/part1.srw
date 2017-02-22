// The MIT License (MIT)

// Copyright (c) 2016 dario ureña

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Source - https://github.com/darioaxel/PowerScriptToKDMTransformer/blob/62083e4cd4f9ead9975427ee9f558d6adef67bbe/resources/advanced/w_mant_seg/part1.srw

forward
global type w_mant_seg_scs from w_mant
end type
type sle_anio from singlelineedit within w_mant_seg_scs
end type
type st_1 from statictext within w_mant_seg_scs
end type
end forward

global type w_mant_seg_scs from w_mant
integer height = 952
sle_anio sle_anio
st_1 st_1
end type
global w_mant_seg_scs w_mant_seg_scs

type variables
//------------------------------------------------------------------------------------------
// Satxa - 26-Ene-2005.19417 
String	is_anio
//------------------------------------------------------------------------------------------
end variables
