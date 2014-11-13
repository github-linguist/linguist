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

import * from whiley.io.File
import SyntaxError from whiley.lang.Errors

// ====================================================
// A simple calculator for expressions
// ====================================================

constant ADD is 0
constant SUB is 1
constant MUL is 2
constant DIV is 3

// binary operation
constant BOp is { ADD, SUB, MUL, DIV }
type BinOp is { BOp op, Expr lhs, Expr rhs } 

// variables
type Var is { string id }

// list access
type ListAccess is { 
    Expr src, 
    Expr index
} 

// expression tree
type Expr is int |  // constant
    Var |              // variable
    BinOp |            // binary operator
    [Expr] |           // list constructor
    ListAccess         // list access

// values
type Value is int | [Value]

// stmts
type Print is { Expr rhs }
type Set is { string lhs, Expr rhs }
type Stmt is Print | Set

// ====================================================
// Expression Evaluator
// ====================================================

type RuntimeError is { string msg }

function evaluate(Expr e, {string=>Value} env) => Value
// Runtime error thrown if evaluation gets "stuck" (e.g. expression is
// not well-formed)
throws RuntimeError:
    //
    if e is int:
        return e
    else if e is Var:
        return env[e.id]
    else if e is BinOp:
        Value lhs = evaluate(e.lhs, env)
        Value rhs = evaluate(e.rhs, env)
        // check if stuck
        if !(lhs is int && rhs is int):
            throw {msg: "arithmetic attempted on non-numeric value"}
        // switch statement would be good
        if e.op == ADD:
            return lhs + rhs
        else if e.op == SUB:
            return lhs - rhs
        else if e.op == MUL:
            return lhs * rhs
        else if rhs != 0:
            return lhs / rhs
        throw {msg: "divide-by-zero"}
    else if e is [Expr]:
        [Value] r = []
        for i in e:
            Value v = evaluate(i, env)
            r = r ++ [v]
        return r
    else if e is ListAccess:
        Value src = evaluate(e.src, env)
        Value index = evaluate(e.index, env)
        // santity checks
        if src is [Value] && index is int && index >= 0 && index < |src|:
            return src[index]
        else:
            throw {msg: "invalid list access"}
    else:
        return 0 // dead-code

// ====================================================
// Expression Parser
// ====================================================

type State is { string input, int pos }

// Top-level parse method
function parse(State st) => (Stmt,State) 
throws SyntaxError:
    //
    Var keyword, Var v
    Expr e
    int start = st.pos
    //
    keyword,st = parseIdentifier(st)
    switch keyword.id:
        case "print":
            e,st = parseAddSubExpr(st)
            return {rhs: e},st
        case "set":
            st = parseWhiteSpace(st)
            v,st = parseIdentifier(st)
            e,st = parseAddSubExpr(st)
            return {lhs: v.id, rhs: e},st
        default:
            throw SyntaxError("unknown statement",start,st.pos-1)

function parseAddSubExpr(State st) => (Expr, State) 
throws SyntaxError:    
    //
    Expr lhs, Expr rhs      
    // First, pass left-hand side 
    lhs,st = parseMulDivExpr(st)
    
    st = parseWhiteSpace(st)
    // Second, see if there is a right-hand side
    if st.pos < |st.input| && st.input[st.pos] == '+':
        // add expression
        st.pos = st.pos + 1
        rhs,st = parseAddSubExpr(st)        
        return {op: ADD, lhs: lhs, rhs: rhs},st
    else if st.pos < |st.input| && st.input[st.pos] == '-':
        // subtract expression
        st.pos = st.pos + 1
        (rhs,st) = parseAddSubExpr(st)        
        return {op: SUB, lhs: lhs, rhs: rhs},st
    
    // No right-hand side
    return (lhs,st)

function parseMulDivExpr(State st) => (Expr, State) 
throws SyntaxError:    
    Expr lhs, Expr rhs
    // First, pass left-hand side
    (lhs,st) = parseTerm(st)
    
    st = parseWhiteSpace(st)
    // Second, see if there is a right-hand side
    if st.pos < |st.input| && st.input[st.pos] == '*':
        // add expression
        st.pos = st.pos + 1
        (rhs,st) = parseMulDivExpr(st)                
        return {op: MUL, lhs: lhs, rhs: rhs}, st
    else if st.pos < |st.input| && st.input[st.pos] == '/':
        // subtract expression
        st.pos = st.pos + 1
        (rhs,st) = parseMulDivExpr(st)        
        return {op: DIV, lhs: lhs, rhs: rhs}, st
    
    // No right-hand side
    return (lhs,st)

function parseTerm(State st) => (Expr, State) 
throws SyntaxError:
    //
    st = parseWhiteSpace(st)        
    if st.pos < |st.input|:
        if Char.isLetter(st.input[st.pos]):
            return parseIdentifier(st)
        else if Char.isDigit(st.input[st.pos]):
            return parseNumber(st)
        else if st.input[st.pos] == '[':
            return parseList(st)
    throw SyntaxError("expecting number or variable",st.pos,st.pos)

function parseIdentifier(State st) => (Var, State):
    //
    string txt = ""
    // inch forward until end of identifier reached
    while st.pos < |st.input| && Char.isLetter(st.input[st.pos]):
        txt = txt ++ st.input[st.pos]
        st.pos = st.pos + 1
    return ({id:txt}, st)

function parseNumber(State st) => (Expr, State) 
throws SyntaxError:    
    // inch forward until end of identifier reached
    int start = st.pos
    while st.pos < |st.input| && Char.isDigit(st.input[st.pos]):
        st.pos = st.pos + 1    
    return Int.parse(st.input[start..st.pos]), st

function parseList(State st) => (Expr, State) 
throws SyntaxError:    
    //
    st.pos = st.pos + 1 // skip '['
    st = parseWhiteSpace(st)
    [Expr] l = [] // initial list
    bool firstTime = true
    while st.pos < |st.input| && st.input[st.pos] != ']':
        if !firstTime && st.input[st.pos] != ',':
            throw SyntaxError("expecting comma",st.pos,st.pos)
        else if !firstTime:
            st.pos = st.pos + 1 // skip ','
        firstTime = false
        Expr e
        e,st = parseAddSubExpr(st)
        // perform annoying error check    
        l = l ++ [e]
        st = parseWhiteSpace(st)
    st.pos = st.pos + 1
    return l,st
 
// Parse all whitespace upto end-of-file
function parseWhiteSpace(State st) => State:
    while st.pos < |st.input| && Char.isWhiteSpace(st.input[st.pos]):
        st.pos = st.pos + 1
    return st

// ====================================================
// Main Method
// ====================================================

public method main(System.Console sys):
    if(|sys.args| == 0):
        sys.out.println("no parameter provided!")
    else:
        File.Reader file = File.Reader(sys.args[0])
        string input = String.fromASCII(file.readAll())
        
        try:
            {string=>Value} env = {"$"=>0} 
            State st = {pos: 0, input: input}
            while st.pos < |st.input|:
                Stmt s
                Value r
                s,st = parse(st)
                r = evaluate(s.rhs,env)
                if s is Set:
                    env[s.lhs] = r
                else:
                    sys.out.println(r)
                st = parseWhiteSpace(st)
        catch(RuntimeError e1):
            sys.out.println("runtime error: " ++ e1.msg)
        catch(SyntaxError e2):
            sys.out.println("syntax error: " ++ e2.msg)
