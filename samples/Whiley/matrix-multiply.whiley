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

import whiley.lang.*
import * from whiley.lang.System
import * from whiley.io.File
import * from whiley.lang.Errors

// Author: David J. Pearce

// ========================================================
// Description
// ========================================================

// This is a very naive implementation of matrix multiplication.  It does
// not perform any optimisations, and does not represent matrices in any
// special manner (e.g. sparse representations, etc).
//
// In the future, it would be interesting to consider chain
// multiplication problem:
//
// http://en.wikipedia.org/wiki/Matrix_chain_multiplication
//

// ========================================================
// Benchmark Code
// ========================================================

type nat is (int x) where x >= 0

type Matrix is {
    int width,
    int height,
    [[int]] data
} where |data| == height && no { i in data | |i| != width }

function Matrix(nat width, nat height, [[int]] data) => (Matrix r)
// Input array must match matrix height
requires |data| == height
// Elements of input array must match matrix width
requires no { i in data | |i| != width }
// 
ensures r.width == width && r.height == height && r.data == data:
    //
    return {
        width: width,
        height: height,
        data: data
    }

function multiply(Matrix A, Matrix B) => (Matrix C) 
// Must be possible to multiply matrices
requires A.width == B.height
// Specify dimensions of result
ensures C.width == B.width && C.height == A.height:
    //
    [[int]] C_data = []
    nat i = 0
    //
    // NOTE: the following loops can be more elegantly written using
    // "for" statements.  However, for the moment I use "while"
    // statements as these work better with verification.
    //
    while i < A.height:
        [int] row = []
        nat j = 0
        while j < B.width:
            int r = 0
            nat k = 0
            while k < A.width:
                r = r + (A.data[i][k] * B.data[k][j])
                k = k + 1
            row = row ++ [r]
            j = j + 1
        C_data = C_data ++ [row]
        i = i + 1
    //
    return Matrix(B.width,A.height,C_data)

// ========================================================
// Parser Code
// ========================================================

function parseFile(string input) => (Matrix,Matrix)
throws SyntaxError:
    Matrix A // 1st result
    Matrix B // 2nd result
    [int] data, int pos = parseLine(2,0,input)
    int nrows = data[0]
    int ncols = data[1]
    pos = skipBreak(pos,input)
    A,pos = parseMatrix(nrows,ncols,pos,input)
    pos = skipBreak(pos,input)
    B,pos = parseMatrix(nrows,ncols,pos,input)
    return A,B

function parseMatrix(nat height, nat width, int pos, string input) => (Matrix,int)
throws SyntaxError:
    //
    [[int]] rows = []
    [int] row
    //
    for i in 0 .. height:
        row,pos = parseLine(width,pos,input)
        rows = rows ++ [row]
    //
    return Matrix(width,height,rows),pos

function parseLine(int count, int pos, string input) => ([int],int) 
throws SyntaxError:
    //
    pos = skipWhiteSpace(pos,input)
    [int] ints = []
    int i
    //
    while pos < |input| && |ints| != count:
        i,pos = parseInt(pos,input)
        ints = ints ++ [i]
        pos = skipWhiteSpace(pos,input)
    //
    if |ints| != count:
        throw SyntaxError("invalid input file",pos,pos)
    //
    return ints,pos

function parseInt(int pos, string input) => (int,int)
throws SyntaxError:
    //
    int start = pos
    // check for negative input
    if pos < |input| && input[pos] == '-':
        pos = pos + 1
    // match remainder
    while pos < |input| && Char.isDigit(input[pos]):
        pos = pos + 1
    // check for error
    if pos == start:
        throw SyntaxError("Missing number",start,pos)
    // done
    return Int.parse(input[start..pos]),pos

function skipBreak(int index, string input) => int:
    while index < |input| && input[index] == '-':
        index = index + 1
    //
    return skipWhiteSpace(index,input)

function skipWhiteSpace(int index, string input) => int:
    while index < |input| && isWhiteSpace(input[index]):
        index = index + 1
    //
    return index

function isWhiteSpace(char c) => bool:
    return c == ' ' || c == '\t' || c == '\r' || c == '\n'

// ========================================================
// Main
// ========================================================

method printMat(System.Console sys, Matrix A):
    for i in 0 .. A.height:
        for j in 0 .. A.width:
            sys.out.print(A.data[i][j])
            sys.out.print(" ")
        sys.out.println("")

method main(System.Console sys):
    if |sys.args| == 0:
        sys.out.println("usage: matrix <input-file>")
    else:
        File.Reader file = File.Reader(sys.args[0])
        // first, read data
        string input = String.fromASCII(file.readAll())
        try:
            // second, build the matrices
            Matrix A, Matrix B = parseFile(input)
            // third, run the benchmark
            Matrix C = multiply(A,B)
            // finally, print the result!
            printMat(sys,C)
        catch(SyntaxError e):
            sys.out.println("error - " ++ e.msg)
