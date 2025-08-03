/*
MIT LICENSE
Copyright 2025 Kiana Bennett

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the “Software”), to deal in the Software without
restriction, including without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.



Source: https://github.com/lucy-language/lucy/blob/master/src/parser.c3

*/


module lucy::parser;

import lucy::lexer, lucy::node, lucy::token, lucy::errors, lucy::util;

struct Parser
{
    String  path;
    Token[] tokens;
    int     pos;
}

fn Parser new(Lexer lexer, String path)
{
    return
    {
        path,
        lexer.tokenize(),
        0
    };
}

fn Token Parser.current(&self) => self.tokens[self.pos];
fn Token Parser.last(&self)    => self.tokens[self.pos - 1];
fn Token Parser.next(&self)    => self.tokens[self.pos + 1];
fn void  Parser.consume(&self) => self.pos++;
fn void  Parser.try_consume(&self, TokenType expected)
{
    if (self.current().type != expected)
    {
        Token last = self.last();
        switch (expected)
        {
            case SEMICOLON:
                Range range = { last.range.end, last.range.end + 1 };
                errors::print_range(self.path, last.line, range, last.ctx, "`;` expected after statement");
            default:        errors::print(self.path, self.current(), string::format(mem, "Expected token %s, got %s", expected, self.current().type));
        }
    }
    self.consume();
}

fn ASTNode* Parser.parse(&self)
{
    ASTNode*   node       = malloc(ASTNode.sizeof);
    ASTNode[]  links      = self.parse_links();
    ASTNode*   pkg        = self.parse_pkg();
    ASTNode*[] statements = self.parse_statements();
    
    self.try_consume(EOF);

    *node = { FILE, { .file_node = { links, pkg, statements } }, {}, 0, {} };

    return node;
}

fn ASTNode[] Parser.parse_links(&self)
{
    ASTNode[] links;

    while (self.current().type == KW_LINK)
    {
        Token    current = self.current();
        int      start   = current.range.start;
        int      line    = current.line;
        String[] ctx     = current.ctx;

        self.consume();
        self.try_consume(LT);

        ASTNode* name = self.parse_single_identifier();

        self.try_consume(GT);
        self.try_consume(SEMICOLON);

        int end = self.last().range.end;

        util::array_insert(&links, (ASTNode){ LINK, { .link_node = { name } }, { start, end }, line, ctx });
    }

    return links;
}

fn ASTNode* Parser.parse_pkg(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();
    ASTNode* path = self.parse_path();
    self.try_consume(SEMICOLON);

    int end = self.last().range.end;

    *node = { PKG, { .pkg_node = { path } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_path(&self)
{
    ASTNode*   node    = malloc(ASTNode.sizeof);
    Token      current = self.current();
    int        start   = current.range.start;
    int        line    = current.line;
    String[]   ctx     = current.ctx;
    ASTNode*[] segments;

    util::array_insert(&segments, self.parse_single_identifier());
    while (self.current().type == PERIOD)
    {
        self.consume();
        util::array_insert(&segments, self.parse_single_identifier());
    }

    int end = self.last().range.end;

    *node = { PATH, { .path_node = { segments } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_single_identifier(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;
    String   value   = current.value.copy(mem);

    self.try_consume(IDENTIFIER);

    int end = self.last().range.end;

    *node = { IDENT, { .ident_node = { value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode*[] Parser.parse_statements(&self)
{
    ASTNode*[] statements;

    while (util::in((TokenType[]){ KW_USE, KW_DEF, KW_EXT, KW_STRUCT, KW_CONST, KW_MACRO }, self.current().type))
    {
        util::array_insert(&statements, self.parse_statement());
    }

    return statements;
}

fn ASTNode* Parser.parse_statement(&self)
{
    switch (self.current().type)
    {
        case KW_USE:    return self.parse_use();
        case KW_DEF:    return self.parse_def();
        case KW_EXT:    return self.parse_ext();
        case KW_CONST:  return self.parse_const();
        case KW_MACRO:  return self.parse_macro();
        case KW_STRUCT: return self.parse_struct();

        default: errors::print(self.path, self.current(), "Statement expected");
    }
}

fn ASTNode* Parser.parse_use(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;
    
    self.consume();
    ASTNode* path = self.parse_path();
    self.try_consume(SEMICOLON);

    int end = self.last().range.end;
    
    *node = { USE, { .use_node = { path } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_def(&self)
{
    ASTNode*   node    = malloc(ASTNode.sizeof);
    Token      current = self.current();
    int        start   = current.range.start;
    int        line    = current.line;
    String[]   ctx     = current.ctx;

    self.consume();
    ASTNode*   type       = self.parse_type();
    ASTNode*   name       = self.parse_single_identifier();
    ASTNode*[] parameters = self.parse_parameters();
    ASTNode*[] children   = self.parse_body();

    int end = self.last().range.end;

    *node = { DEF, { .def_node = { type, name, parameters, children } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_type(&self)
{
    ASTNode* node;
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    if (self.current().type == LPAREN)
    {
        node = self.parse_tuple_type();
    } else
    {
        node = self.parse_single_identifier();
    }

    int end = self.last().range.end;

    return node;
}

fn ASTNode*[] Parser.parse_parameters(&self)
{
    ASTNode*[] parameters;

    self.try_consume(LPAREN);

    if (self.current().type != RPAREN)
    {
        util::array_insert(&parameters, self.parse_parameter());

        while (self.current().type == COMMA)
        {
            self.consume();
            util::array_insert(&parameters, self.parse_parameter());
        }
    }

    self.try_consume(RPAREN);

    return parameters;
}

fn ASTNode* Parser.parse_parameter(&self)
{
    ASTNode*   node    = malloc(ASTNode.sizeof);
    Token      current = self.current();
    int        start   = current.range.start;
    int        line    = current.line;
    String[]   ctx     = current.ctx;

    bool is_vararg = current.type == KW_VAR;
    if (is_vararg) self.consume();

    ASTNode* type = malloc(ASTNode.sizeof);
    ASTNode* name = malloc(ASTNode.sizeof);

    *type = util::make_null();
    *name = util::make_null();

    ASTNode* type_or_name = self.parse_type();

    if (self.current().type != IDENTIFIER && type_or_name.node_type == IDENT)
    {// untyped
        name = type_or_name;
    } else
    {
        type = type_or_name;
        name = self.parse_single_identifier();
    }

    int end = self.last().range.end;
    
    *node = { PARAMETER, { .parameter_node = { is_vararg, type, name } }, { start, end }, line, ctx };
    
    return node;
}

fn ASTNode*[] Parser.parse_body(&self)
{
    ASTNode*[] body;

    self.try_consume(LBRACE);

    while (util::in((TokenType[]){ IDENTIFIER, KW_VAR, KW_WHILE, KW_FOR, KW_RETURN, KW_IF, KW_SWITCH }, self.current().type))
    {
        util::array_insert(&body, self.parse_body_statement());
    }

    self.try_consume(RBRACE);

    return body;
}

fn ASTNode* Parser.parse_body_statement(&self)
{
    switch (self.current().type)
    {
        case KW_IF:      return self.parse_if();
        case KW_VAR:     return self.parse_var();
        case KW_FOR:     return self.parse_for();
        case KW_WHILE:   return self.parse_while();
        case KW_RETURN:  return self.parse_return();
        case KW_SWITCH:  return self.parse_switch();
        case IDENTIFIER:
            ASTNode* node = self.parse_identifier();
            self.try_consume(SEMICOLON);
            return node;

        default: errors::print(self.path, self.current(), "Statement expected");
    }
}

fn ASTNode* Parser.parse_identifier(&self)
{
    if (self.next().type == LPAREN) return self.parse_call();
    if (self.next().type == PERIOD) return self.parse_access();
    if (self.next().type == EQUALS) return self.parse_reassign();
    
    if (
        util::in((TokenType[]){ PLUS, MINUS, BANG }, self.next().type) &&
        util::in((TokenType[]){ PLUS, MINUS, BANG }, self.tokens[self.pos + 2].type)) return self.parse_inc();

    return self.parse_single_identifier();
}

fn ASTNode* Parser.parse_ext(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();

    ASTNode*   type       = self.parse_type();
    ASTNode*   name       = self.parse_single_identifier();
    ASTNode*[] parameters = self.parse_parameters();
    ASTNode*   aka        = malloc(ASTNode.sizeof);
    *aka = util::make_null();

    if (self.current().type == KW_AS)
    {
        self.consume();
        aka = self.parse_single_identifier();
    }

    self.try_consume(SEMICOLON);

    int end = self.last().range.end;

    *node = { EXT, { .ext_node = { type, name, parameters, aka } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_struct(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();
    ASTNode*   name   = self.parse_single_identifier();
    ASTNode*[] fields = self.parse_struct_fields();

    int end = self.last().range.end;

    *node = { STRUCT, { .struct_node = { name, fields } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode*[] Parser.parse_struct_fields(&self)
{
    ASTNode*[] struct_fields;

    self.try_consume(LBRACE);
    while (self.current().type != RBRACE)
    {
        util::array_insert(&struct_fields, self.parse_struct_field());
    }
    self.try_consume(RBRACE);

    return struct_fields;
}

fn ASTNode* Parser.parse_struct_field(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    ASTNode* type = self.parse_type();
    ASTNode* name = self.parse_single_identifier();
    self.try_consume(SEMICOLON);

    int end = self.last().range.end;
    
    *node = { FIELD, { .field_node = { type, name } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_var(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.try_consume(KW_VAR);
    ASTNode*   type  = self.parse_type();
    ASTNode*[] names;
    
    util::array_insert(&names, self.parse_single_identifier());
    while (self.current().type == COMMA)
    {
        self.consume();
        util::array_insert(&names, self.parse_single_identifier());
    }

    ASTNode* value = malloc(ASTNode.sizeof);
    *value = util::make_null();

    if (self.current().type == EQUALS)
    {
        self.consume();
        value = self.parse_expression();
    }

    self.try_consume(SEMICOLON);

    int end = self.last().range.end;

    *node = { VAR, { .var_node = { type, names, value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_expression(&self)
{
    ASTNode* node  = self.parse_term();
    int      start = node.range.start;
    int      line  = node.line;
    String[] ctx   = node.ctx;

    while (util::in((TokenType[]){ PLUS, MINUS, IS_EQUAL, NOT_EQUAL, GTE, LTE, GT, LT, AND, OR }, self.current().type))
    {
        String operator = self.current().value;
        self.consume();
        
        ASTNode* left = malloc(ASTNode.sizeof);
        *left = *node;
        
        ASTNode* right = self.parse_term();
        int      end   = right.range.end;

        *node = { BIN, { .bin_node = { operator, left, right } }, { start, end }, line, ctx };
    }

    return node;
}

fn ASTNode* Parser.parse_term(&self)
{
    ASTNode* node  = self.parse_factor();
    int      start = node.range.start;
    int      line  = node.line;
    String[] ctx   = node.ctx;

    while (util::in((TokenType[]){ STAR, SLASH }, self.current().type))
    {
        String operator = self.current().value;
        self.consume();
        
        ASTNode* left = malloc(ASTNode.sizeof);
        *left = *node;
        
        ASTNode* right = self.parse_factor();
        int      end   = right.range.end;

        *node = { BIN, { .bin_node = { operator, left, right } }, { start, end }, line, ctx };
    }

    return node;
}

fn ASTNode* Parser.parse_factor(&self)
{
    switch (self.current().type)
    {
        case CHAR:       return self.parse_char();
        case PLUS:
        case BANG:
        case MINUS:      return self.parse_unary();
        case FLOAT:      return self.parse_float();
        case LBRACE:     return self.parse_init();
        case DOUBLE:     return self.parse_double();
        case STRING:     return self.parse_string();
        case LPAREN:
            self.consume();
            ASTNode* expression = self.parse_expression();
            self.try_consume(RPAREN);
            (*expression).range.start--;
            (*expression).range.end++;
            return expression;
        case BOOLEAN:    return self.parse_bool();
        case INTEGER:    return self.parse_int();
        case IDENTIFIER: return self.parse_identifier();

        default: return self.parse_null();
    }
}

fn ASTNode* Parser.parse_init(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.try_consume(LBRACE);
    
    ASTNode*[] values;
    
    if (self.current().type != RBRACE)
    {
        util::array_insert(&values, self.parse_init_value());
        while (self.current().type == COMMA)
        {
            self.consume();
            util::array_insert(&values, self.parse_init_value());
        }
    }
    
    self.try_consume(RBRACE);

    int end = self.last().range.end;

    *node = { INIT, { .init_node = { values } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_init_value(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;
    
    ASTNode* name = malloc(ASTNode.sizeof);
    *name = util::make_null();

    if (self.current().type == PERIOD)
    {
        self.consume();
        name = self.parse_single_identifier();
        self.try_consume(EQUALS);
    }

    ASTNode* value = self.parse_expression();

    int end = self.last().range.end;

    *node = { INIT_VALUE, { .init_value_node = { name, value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_int(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;
    String   value   = current.value;

    self.try_consume(INTEGER);

    int end = self.last().range.end;

    *node = { INT, { .int_node = { value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_tuple_type(&self)
{
    ASTNode*   node    = malloc(ASTNode.sizeof);
    Token      current = self.current();
    int        start   = current.range.start;
    int        line    = current.line;
    String[]   ctx     = current.ctx;
    ASTNode*[] values;
    
    self.try_consume(LPAREN);

    if (self.current().type != RPAREN)
    {
        util::array_insert(&values, self.parse_type());
        while (self.current().type == COMMA)
        {
            self.consume();
            util::array_insert(&values, self.parse_type());
        }
    }

    self.try_consume(RPAREN);

    int end = self.last().range.end;

    *node = { TUPLE, { .tuple_node = { values } }, { start, end }, line, ctx };
    
    return node;
}

fn ASTNode* Parser.parse_call(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    ASTNode*   name = self.parse_single_identifier();
    ASTNode*[] arguments = self.parse_arguments();

    int end = self.last().range.end;
    
    *node = { CALL, { .call_node = { name, arguments } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode*[] Parser.parse_arguments(&self)
{
    ASTNode*[] arguments;

    self.try_consume(LPAREN);

    if (self.current().type != RPAREN)
    {
        util::array_insert(&arguments, self.parse_expression());
        while (self.current().type == COMMA)
        {
            self.consume();
            util::array_insert(&arguments, self.parse_expression());
        }
    }

    self.try_consume(RPAREN);

    return arguments;
}

fn ASTNode* Parser.parse_double(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;
    String   value   = current.value;

    self.try_consume(DOUBLE);

    int end = self.last().range.end;

    *node = { DOUBLE, { .double_node = { value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_float(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;
    String   value   = current.value;

    self.try_consume(FLOAT);

    int end = self.last().range.end;

    *node = { FLOAT, { .float_node = { value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_bool(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;
    String   value   = current.value;

    self.try_consume(BOOLEAN);

    int end = self.last().range.end;

    *node = { BOOL, { .bool_node = { value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_string(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;
    String   value   = current.value;

    self.try_consume(STRING);

    int end = self.last().range.end;

    *node = { STR, { .str_node = { value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_char(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;
    String   value   = current.value;

    self.try_consume(CHAR);

    int end = self.last().range.end;

    *node = { CHAR, { .char_node = { value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_access(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    ASTNode* identifier = self.parse_single_identifier();
    self.try_consume(PERIOD);
    ASTNode* next       = self.parse_identifier();

    int end = self.last().range.end;

    *node = { ACCESS, { .access_node = { identifier, next } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_while(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();
    self.try_consume(LPAREN);
    ASTNode* condition = self.parse_expression();
    self.try_consume(RPAREN);
    ASTNode*[] children = self.parse_body();

    int end = self.last().range.end;

    *node = { WHILE, { .while_node = { condition, children } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_reassign(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    ASTNode* identifier = self.parse_single_identifier();
    self.try_consume(EQUALS);
    ASTNode* value = self.parse_expression();

    int end = self.last().range.end;

    *node = { REASSIGN, { .reassign_node = { identifier, value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_for(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();
    self.try_consume(LPAREN);

    ASTNode* statement  = self.parse_var();
    ASTNode* condition  = self.parse_expression();
    self.try_consume(SEMICOLON);
    ASTNode* expression = self.parse_expression();

    self.try_consume(RPAREN);

    ASTNode*[] children = self.parse_body();

    int end = self.last().range.end;

    *node = { FOR, { .for_node = { statement, condition, expression, children } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_null(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.try_consume(KW_NULL);

    int end = self.last().range.end;

    *node = { NULL, { .null_node = { "null" } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_inc(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    ASTNode* identifier = self.parse_single_identifier();

    Token operator = self.current();
    self.try_consume(operator.type);
    self.try_consume(operator.type);

    int end = self.last().range.end;

    *node = { INC, { .inc_node = { identifier, operator.value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_return(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();

    ASTNode*[] values;
    if (self.current().type != SEMICOLON)
    {
        util::array_insert(&values, self.parse_expression());
        while (self.current().type == COMMA)
        {
            self.consume();
            util::array_insert(&values, self.parse_expression());
        }
    }

    self.try_consume(SEMICOLON);

    int end = self.last().range.end;

    *node = { RETURN, { .return_node = { values } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_if(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();
    self.try_consume(LPAREN);
    ASTNode* condition = self.parse_expression();
    self.try_consume(RPAREN);

    ASTNode*[] children = self.parse_body();

    ASTNode* else_if = malloc(ASTNode.sizeof);
    *else_if = util::make_null();

    ASTNode*[] else_children;
    if (self.current().type == KW_ELSE)
    {
        self.consume();

        if (self.current().type == KW_IF)
        {
            else_if = self.parse_if();
        } else
        {
            else_children = self.parse_body();
        }
    }

    int end = self.last().range.end;

    *node = { IF, { .if_node = { condition, children, else_if, else_children } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_switch(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();
    self.try_consume(LPAREN);
    ASTNode* condition = self.parse_expression();
    self.try_consume(RPAREN);

    self.try_consume(LBRACE);
    ASTNode*[] cases;
    while (self.current().type == KW_CASE)
    {
        util::array_insert(&cases, self.parse_case());
    }

    self.try_consume(RBRACE);

    int end = self.last().range.end;

    *node = { SWITCH, { .switch_node = { condition, cases } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_case(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();
    ASTNode* condition = self.parse_expression();
    self.try_consume(COLON);

    ASTNode*[] children;
    while (!util::in((TokenType[]){ KW_CASE, RBRACE }, self.current().type))
    {
        util::array_insert(&children, self.parse_body_statement());
    }

    int end = self.last().range.end;

    *node = { CASE, { .case_node = { condition, children } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_const(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    self.consume();
    ASTNode* type = self.parse_type();
    ASTNode* name = self.parse_single_identifier();

    self.try_consume(EQUALS);
    ASTNode* value = self.parse_expression();

    self.try_consume(SEMICOLON);

    int end = self.last().range.end;

    *node = { CONST, { .const_node = { type, name, value } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_macro(&self)
{
    ASTNode*   node    = malloc(ASTNode.sizeof);
    Token      current = self.current();
    int        start   = current.range.start;
    int        line    = current.line;
    String[]   ctx     = current.ctx;

    self.consume();
    ASTNode*   type       = self.parse_type();
    ASTNode*   name       = self.parse_single_identifier();
    ASTNode*[] parameters = self.parse_parameters();
    ASTNode*[] children   = self.parse_body();

    int end = self.last().range.end;

    *node = { MACRO, { .macro_node = { type, name, parameters, children } }, { start, end }, line, ctx };

    return node;
}

fn ASTNode* Parser.parse_unary(&self)
{
    ASTNode* node    = malloc(ASTNode.sizeof);
    Token    current = self.current();
    int      start   = current.range.start;
    int      line    = current.line;
    String[] ctx     = current.ctx;

    String operator = self.current().value;
    self.consume();

    ASTNode* value = self.parse_expression();

    int end = self.last().range.end;

    *node = { UNARY, { .unary_node = { operator, value } }, { start, end }, line, ctx };

    return node;
}


