/*
        Source: https://github.com/lucy-language/lucy/blob/master/src/lexer.c3

*/


module lucy::lexer;

import std;
import lucy::util, lucy::token;

struct Lexer
{
    String text;
    int    pos;
    char   current;
    int    line;
    int    column;
}

fn Lexer new(String text)
{
    if (text.len == 0)
    {
        return
        {
            .text    = text,
            .pos     = 0,
            .current = '\0',
            .line    = 1,
            .column  = 0
        };
    }

    return
    {
        .text    = text,
        .pos     = 0,
        .current = text[0],
        .line    = 1,
        .column  = 0
    };
}

fn void Lexer.consume(&self, int amount = 1)
{
    self.pos += amount;
    if (self.pos < self.text.len)
    {
        self.current = self.text[self.pos];
    } else
    {
        self.current = '\0';
    }
}

fn char Lexer.peek(&self)
{
    return self.text[self.pos + 1];
}

fn Token[] Lexer.tokenize(&self)
{
    Token[] tokens;

    while (self.current != '\0')
    {
        while (self.current.is_space())
        {
            self.column++;

            if (util::in((char[]){ '\n', '\r' }, self.current))
            {
                self.column = 0;
                self.line++;
            }
            self.consume();
        }

        if (self.current == '\0') break;

        int start_column = self.column;
        int start_line   = self.line;

        if (self.current == '&' && self.peek() == '&') { util::array_insert(&tokens, (Token){ AND, "&&", { self.column, self.column + 2 }, self.line, self.get_context_lines() }); self.consume(2); self.column += 2; }
        if (self.current == '|' && self.peek() == '|') { util::array_insert(&tokens, (Token){ OR,  "||", { self.column, self.column + 2 }, self.line, self.get_context_lines() }); self.consume(2); self.column += 2; }
        
        if (self.current == '=' && self.peek() == '=') { util::array_insert(&tokens, (Token){ IS_EQUAL,  "==", { self.column, self.column + 2 }, self.line, self.get_context_lines() }); self.consume(2); self.column += 2; }
        if (self.current == '!' && self.peek() == '=') { util::array_insert(&tokens, (Token){ NOT_EQUAL, "!=", { self.column, self.column + 2 }, self.line, self.get_context_lines() }); self.consume(2); self.column += 2; }
        if (self.current == '>' && self.peek() == '=') { util::array_insert(&tokens, (Token){ GT,        ">=", { self.column, self.column + 2 }, self.line, self.get_context_lines() }); self.consume(2); self.column += 2; }
        if (self.current == '<' && self.peek() == '=') { util::array_insert(&tokens, (Token){ LTE,       "<=", { self.column, self.column + 2 }, self.line, self.get_context_lines() }); self.consume(2); self.column += 2; }
        if (self.current == '>') { util::array_insert(&tokens, (Token){ GT, ">", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '<') { util::array_insert(&tokens, (Token){ LT, "<", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }

        if (self.current == ';') { util::array_insert(&tokens, (Token){ SEMICOLON, ";", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '.') { util::array_insert(&tokens, (Token){ PERIOD,    ".", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == ',') { util::array_insert(&tokens, (Token){ COMMA,     ",", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '(') { util::array_insert(&tokens, (Token){ LPAREN,    "(", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == ')') { util::array_insert(&tokens, (Token){ RPAREN,    ")", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '{') { util::array_insert(&tokens, (Token){ LBRACE,    "{", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '}') { util::array_insert(&tokens, (Token){ RBRACE,    "}", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '[') { util::array_insert(&tokens, (Token){ LBRACKET,  "[", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == ']') { util::array_insert(&tokens, (Token){ RBRACKET,  "]", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '%') { util::array_insert(&tokens, (Token){ PERCENT,   "%", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '@') { util::array_insert(&tokens, (Token){ AT,        "@", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '=') { util::array_insert(&tokens, (Token){ EQUALS,    "=", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == ':') { util::array_insert(&tokens, (Token){ COLON,     ":", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '!') { util::array_insert(&tokens, (Token){ BANG,      "!", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }

        if (self.current == '+') { util::array_insert(&tokens, (Token){ PLUS,   "+", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '-') { util::array_insert(&tokens, (Token){ MINUS,  "-", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '*') { util::array_insert(&tokens, (Token){ STAR,   "*", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }
        if (self.current == '/') { util::array_insert(&tokens, (Token){ SLASH,  "/", { self.column, self.column + 1 }, self.line, self.get_context_lines() }); self.consume(); self.column++; }

        if (self.current == '#')
        {
            while (self.current != '\0' && !util::in((char[]){ '\n', '\r' }, self.current))
            {
                self.consume();
            }
        }

        if (self.current == '"')                            util::array_insert(&tokens, self.string());
        if (self.current == '\'')                           util::array_insert(&tokens, self.character());
        if (self.current.is_digit())                        util::array_insert(&tokens, self.number());
        if (self.current == '$' || self.current.is_alpha()) util::array_insert(&tokens, self.identifier());

        bool consumed = (self.column > start_column) || (self.line > start_line);
        if (!consumed && !self.current.is_space() && self.current != '\0')
        {
            util::panicf("Undefined character `%s`", (String){self.current}, file: $$FILE, line: $$LINE, function: $$FUNC);
        }
    }

    util::array_insert(&tokens, (Token){ EOF, "EOF", { self.column, self.column }, self.line, self.get_context_lines() });

    return tokens;
}

fn Token Lexer.string(&self)
{
    DString result;
    defer result.free();
    int start = self.column;

    self.column++;
    self.consume();

    while (self.current != '"')
    {
        if (self.current == '\\')
        {
            result.append_char(self.current);
            self.consume();
            self.column++;
        }
        result.append_char(self.current);
        self.column++;
        self.consume();
    }
    self.column++;
    self.consume();

    return { STRING, result.copy_str(mem), { start, self.column }, self.line, self.get_context_lines() };
}

fn Token Lexer.character(&self)
{
    DString result;
    defer result.free();
    int start = self.column;

    self.column++;
    self.consume();
    
    if (self.current != '\'')
    {
        if (self.current == '\\')
        {
            result.append_char(self.current);
            self.consume();
            self.column++;
        }
        result.append_char(self.current);
        self.column++;
        self.consume();
    }
    self.column++;
    self.consume();

    return { CHAR, result.copy_str(mem), { start, self.column }, self.line, self.get_context_lines() };
}

fn Token Lexer.number(&self)
{
    DString result;
    defer result.free();
    int start = self.column;

    if (self.current == '0' && util::in((char[]){ 'x', 'X' }, self.peek()))
    {
        result.append_char(self.current);
        self.consume();
        result.append_char(self.current);
        self.consume();
        self.column += 2;

        while (self.current.is_digit() || util::in((char[])util::LOWERCASE[0..5], self.current) || util::in((char[])util::UPPERCASE[0..5], self.current))
        {
            result.append_char(self.current);
            self.column++;
            self.consume();
        }

        return { INTEGER, result.copy_str(mem), { start, self.column }, self.line, self.get_context_lines() };
    }

    while (self.current.is_digit())
    {
        result.append_char(self.current);
        self.column++;
        self.consume();
    }

    bool has_decimal_or_exponent = false;
    if (self.current == '.')
    {
        has_decimal_or_exponent = true;
        result.append_char(self.current);
        self.column++;
        self.consume();

        while (self.current.is_digit())
        {
            result.append_char(self.current);
            self.column++;
            self.consume();
        }
    }

    if (util::in((char[]){ 'e', 'E' }, self.current))
    {
        has_decimal_or_exponent = true;
        result.append_char(self.current);
        self.column++;
        self.consume();

        if (util::in((char[]){ '+', '-' }, self.current))
        {
            result.append_char(self.current);
            self.column++;
            self.consume();
        }

        while (self.current.is_digit())
        {
            result.append_char(self.current);
            self.column++;
            self.consume();
        }
    }

    if (util::in((char[]){ 'f', 'F' }, self.current))
    {
        self.column++;
        self.consume();
        return { FLOAT, result.copy_str(mem), { start, self.column }, self.line, self.get_context_lines() };
    }

    if (!has_decimal_or_exponent)
    {
        return { INTEGER, result.copy_str(mem), { start, self.column }, self.line, self.get_context_lines() };
    }

    return { DOUBLE, result.copy_str(mem), { start, self.column }, self.line, self.get_context_lines() };
}

fn Token Lexer.identifier(&self)
{
    DString result;
    defer result.free();
    int start = self.column;

    while (self.current.is_alnum() || util::in((char[]){ '_', '$' }, self.current))
    {
        result.append_char(self.current);
        self.column++;
        self.consume();
    }

    String str = result.copy_str(mem);

    if (util::in((String[]){ "true", "false" }, str))
    {
        return { BOOLEAN, str, { start, self.column }, self.line, self.get_context_lines() };
    }

    if (str == "if")     return { KW_IF,     str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "as")     return { KW_AS,     str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "pkg")    return { KW_PKG,    str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "def")    return { KW_DEF,    str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "use")    return { KW_USE,    str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "ext")    return { KW_EXT,    str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "var")    return { KW_VAR,    str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "for")    return { KW_FOR,    str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "else")   return { KW_ELSE,   str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "case")   return { KW_CASE,   str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "link")   return { KW_LINK,   str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "null")   return { KW_NULL,   str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "const")  return { KW_CONST,  str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "macro")  return { KW_MACRO,  str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "while")  return { KW_WHILE,  str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "struct") return { KW_STRUCT, str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "return") return { KW_RETURN, str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "global") return { KW_GLOBAL, str, { start, self.column }, self.line, self.get_context_lines() };
    if (str == "switch") return { KW_SWITCH, str, { start, self.column }, self.line, self.get_context_lines() };

    return { IDENTIFIER, str, { start, self.column }, self.line, self.get_context_lines() };
}

fn String[] Lexer.get_context_lines(&self)
{
    int      error_line   = self.line;
    String[] context_lines;
    int      current_pos  = 0;
    int      current_line = 1;
    int      line_start   = 0;
    int      text_len     = self.text.len;
    int      start_line   = max(1, error_line - 3);

    while (current_pos < text_len && current_line <= error_line)
    {
        if (current_line >= start_line)
        {
            int line_end = current_pos;
            while (line_end < text_len && self.text[line_end] != '\n' && self.text[line_end] != '\r')
            {
                line_end++;
            }

            if (line_start <= line_end && line_end < text_len)
            {
                util::array_insert(&context_lines, self.text[line_start..line_end]);
            }
        }

        while (current_pos < text_len && self.text[current_pos] != '\n' && self.text[current_pos] != '\r')
        {
            current_pos++;
        }

        if (current_pos < text_len)
        {
            if (self.text[current_pos] == '\r')
            {
                current_pos++;
                if (current_pos < text_len && self.text[current_pos] == '\n')
                {
                    current_pos++;
                }
            } else
            {
                current_pos++;
            }
        }

        current_line++;
        line_start = current_pos;
    }

    return context_lines;
}