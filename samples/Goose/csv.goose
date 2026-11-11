// Comma-separated values as RFC 4180 defines them (docs/stdlib.md): a
// reader that hands each record's fields to a block as slices of the text,
// and the quoting a writer needs.

namespace csv;

import std;

// F(fields) for each record of `text`, in order: `fields` holds the
// record's fields, each a slice into the text, so nothing is copied. A
// quoted field comes without its surrounding quotes and with any doubled
// quotes inside left doubled; it may hold separators, quotes and line
// breaks. Records end at LF or CRLF, a final line break being optional. A
// header is a record like any other.
//
// Returns false at the first malformed record, which F does not see: a
// quote in a field that is not quoted, a quoted field with no closing
// quote, or anything but a separator or a line break after a closing quote.
fn each_record<F>(text: const u8[:], sep: u8 = ',') -> bool {
    assert(sep != '"' && sep != '\n' && sep != '\r');
    var fields: (const u8[:])[>..] = [];
    var pos = 0;
    while pos < text.len {
        fields.clear();
        loop {
            if pos < text.len && text[pos] == '"' {
                pos++;
                let start = pos;
                loop {
                    guard pos < text.len else { return false; }
                    if text[pos] == '"' {
                        if pos + 1 < text.len && text[pos + 1] == '"' { pos += 2; continue; }
                        break;
                    }
                    pos++;
                }
                fields.push(text[start..pos]);
                pos++;
                if pos + 1 < text.len && text[pos] == '\r' && text[pos + 1] == '\n' { pos++; }
                guard pos >= text.len || text[pos] == sep || text[pos] == '\n' else { return false; }
            } else {
                let start = pos;
                while pos < text.len && text[pos] != sep && text[pos] != '\n' && text[pos] != '"' { pos++; }
                guard pos >= text.len || text[pos] != '"' else { return false; }
                let crlf = pos > start && pos < text.len && text[pos] == '\n' && text[pos - 1] == '\r';
                fields.push(text[start..if crlf { pos - 1 } else { pos }]);
            }
            if pos >= text.len { break; }
            let c = text[pos];
            pos++;
            if c != sep { break; }
        }
        F(fields[..]);
    }
    return true;
}

// Appends `s` as a field: as it is, or quoted with its quotes doubled where
// it holds the separator, a quote or a line break.
fn format_field(out: u8[>..]&, s: u8[:], sep: u8 = ',') {
    var plain = true;
    for c in s {
        if c == sep || c == '"' || c == '\n' || c == '\r' { plain = false; break; }
    }
    if plain { out.append(s); return; }
    out.push('"');
    format_replaced(out, s, "\"", "\"\"");
    out.push('"');
}
