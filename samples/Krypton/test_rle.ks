#!/usr/bin/env kr
// kr lsp/test_rle.ks
// Replaces lsp/test_rle.js. Sends documentSymbol against
// algorithms/run_length_encoding.k and prints the result.
import "k:proc"
import "k:fs"

func frame(body) {
    emit "Content-Length: " + len(body) + "\r\n\r\n" + body
}

func _bodyStart(buf) {
    let i = 0
    while i + 3 < len(buf) {
        if buf[i] == "\r" && buf[i + 1] == "\n" && buf[i + 2] == "\r" && buf[i + 3] == "\n" {
            emit i + 4
        }
        i += 1
    }
    emit -1
}

func _cLen(headers) {
    let p = indexOf(headers, "Content-Length:")
    if p < 0 { emit 0 }
    let i = p + 15
    while i < len(headers) && (headers[i] == " " || headers[i] == "\t") { i += 1 }
    let sb = sbNew()
    while i < len(headers) && headers[i] >= "0" && headers[i] <= "9" {
        sb = sbAppend(sb, headers[i])
        i += 1
    }
    emit toInt(sbToString(sb))
}

// JSON string-escape a Krypton string for embedding in a request body.
func _jstr(s) {
    let out = sbNew()
    out = sbAppend(out, "\"")
    let i = 0
    while i < len(s) {
        let c = s[i]
        if c == "\\"      { out = sbAppend(out, "\\\\") }
        else if c == "\"" { out = sbAppend(out, "\\\"") }
        else if c == "\n" { out = sbAppend(out, "\\n") }
        else if c == "\r" { out = sbAppend(out, "\\r") }
        else if c == "\t" { out = sbAppend(out, "\\t") }
        else { out = sbAppend(out, c) }
        i += 1
    }
    out = sbAppend(out, "\"")
    emit sbToString(out)
}

just run {
    let filePath = "algorithms/run_length_encoding.k"
    if fsFileExists(filePath) != "1" {
        kp("FAIL: " + filePath + " not found (run from repo root)")
        exit("1")
    }
    let text = readFile(filePath)
    let uri = "file:///" + filePath

    let klsPath = "C:\\krypton\\kls.exe"
    if fsFileExists(klsPath) != "1" { klsPath = "kls.exe" }

    let p = procSpawn(klsPath, "")
    if p == "0" {
        kp("FAIL: kls.exe not found")
        exit("1")
    }

    procWrite(p, frame("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":0,\"rootUri\":null,\"capabilities\":{}}}"))
    procWrite(p, frame("{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}"))
    procWrite(p, frame("{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{\"textDocument\":{\"uri\":" + _jstr(uri) + ",\"languageId\":\"krypton\",\"version\":1,\"text\":" + _jstr(text) + "}}}"))
    procWrite(p, frame("{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"textDocument/documentSymbol\",\"params\":{\"textDocument\":{\"uri\":" + _jstr(uri) + "}}}"))
    procWrite(p, frame("{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}"))
    procWrite(p, frame("{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}"))

    procSleep(500)
    let buf = procRead(p, 65536)

    kp("DOCUMENT SYMBOLS:")
    let pos = 0
    let nf = 0
    while pos < len(buf) {
        let tail = substring(buf, pos, len(buf))
        let off = _bodyStart(tail)
        if off < 0 { pos = len(buf) }
        else {
            let body = substring(tail, off, off + _cLen(substring(tail, 0, off - 4)))
            if contains(body, "\"id\":2") {
                kp(body)
                nf += 1
            }
            pos = pos + off + _cLen(substring(tail, 0, off - 4))
        }
    }
    if nf == 0 { kp("(no documentSymbol response received)") }
    procClose(p)
}
