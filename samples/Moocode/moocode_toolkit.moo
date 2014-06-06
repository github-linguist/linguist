;; while (read(player) != ".") endwhile

I M P O R T A N T
=================

The following code cannot be used as is.  You will need to rewrite
functionality that is not present in your server/core.  The most
straight-forward target (other than Stunt/Improvise) is a server/core
that provides a map datatype and anonymous objects.

Installation in my server uses the following object numbers:

#36819 -> MOOcode Experimental Language Package
  #36820 -> Changelog
  #36821 -> Dictionary
  #36822 -> MOOcode Compiler
  #38128 -> Syntax Tree Pretty Printer
  #37644 -> Tokenizer Prototype
  #37645 -> Parser Prototype
  #37648 -> Symbol Prototype
  #37649 -> Literal Prototype
  #37650 -> Statement Prototype
  #37651 -> Operator Prototype
  #37652 -> Control Flow Statement Prototype
  #37653 -> Assignment Operator Prototype
  #38140 -> Compound Assignment Operator Prototype
  #38123 -> Prefix Operator Prototype
  #37654 -> Infix Operator Prototype
  #37655 -> Name Prototype
  #37656 -> Bracket Operator Prototype
  #37657 -> Brace Operator Prototype
  #37658 -> If Statement Prototype
  #38119 -> For Statement Prototype
  #38120 -> Loop Statement Prototype
  #38126 -> Fork Statement Prototype
  #38127 -> Try Statement Prototype
  #37659 -> Invocation Operator Prototype
  #37660 -> Verb Selector Operator Prototype
  #37661 -> Property Selector Operator Prototype
  #38124 -> Error Catching Operator Prototype
  #38122 -> Positional Symbol Prototype
  #38141 -> From Statement Prototype
  #37662 -> Utilities
#36823 -> MOOcode Experimental Language Package Tests
  #36824 -> MOOcode Compiler Tests
  #37646 -> Tokenizer Tests
  #37647 -> Parser Tests
.

; /* BASE */

; parent($plastic.tokenizer_proto)

@program _:_ensure_prototype as application/x-moocode
(typeof(this) == OBJ) || raise(E_INVARG, "Callable on prototypes only");
.

@program _:_ensure_instance as application/x-moocode
(typeof(this) == ANON) || raise(E_INVARG, "Callable on instances only");
.

; /* COMPILER */

@program $plastic.compiler:_lookup as application/x-moocode
$private();

this:_ensure_instance();

{name} = args;

if (`value = this.variable_map[name] ! E_RANGE')
  return value;
elseif (name in this.variable_map || name in this.reserved_names)
  value = name;
  while (value in this.variable_map || value in this.reserved_names)
    value = tostr("_", value);
  endwhile
  this.variable_map[name] = value;
  return value;
else
  value = name;
  this.variable_map[name] = value;
  return value;
endif
.

@program $plastic.compiler:_generate as application/x-moocode
$private();

this:_ensure_instance();

{name} = args;

if (`value = this.variable_map[name] ! E_RANGE')
  return value;
else
  value = tostr("_", random());
  while (value in this.variable_map || value in this.reserved_names)
    value = tostr("_", random());
  endwhile
  this.variable_map[name] = value;
  return value;
endif
.

@program $plastic.compiler:compile as application/x-moocode
this:_ensure_prototype();

{source, ?options = []} = args;

tokenizer = this.plastic.tokenizer_proto:create(source);
parser = this.plastic.parser_proto:create(tokenizer);
compiler = create(this, 1);

try
  statements = parser:statements();
except ex (ANY)
  return {0, {tostr("Line ", ex[3].tokenizer.row, ":  ", ex[2])}};
endtry

source = {};

for statement in (statements)
  if (statement.type != "statement")
    source = {@source, tostr(compiler:p(statement), ";")};
  else
    source = {@source, @compiler:p(statement)};
  endif
endfor

return {1, source};
.

@program $plastic.compiler:p as application/x-moocode
this:_ensure_instance();

{statement} = args;

ticks_left() < 10000 || seconds_left() < 2 && suspend(0);

if (statement.type == "variable")
  return this:_lookup(statement.value);
elseif (statement.type == "unique")
  return this:_generate(statement.value);
elseif (isa(statement, this.plastic.sign_operator_proto))
  if (statement.type == "unary")
    return tostr(statement.value == "-" ? "-" | "", this:p(statement.first));
  else
    return tostr("(", this:p(statement.first), " ", statement.value, " ", this:p(statement.second), ")");
  endif

elseif (isa(statement, this.plastic.control_flow_statement_proto))
  if ((first = statement.first) != 0)
    return {tostr(statement.id, " " , this:p(first), ";")};
  else
    return {tostr(statement.id, ";")};
  endif

elseif (isa(statement, this.plastic.if_statement_proto))
  value =  statement.value;

  code = {tostr("if (", this:p(value[1]), ")")};

  for s in (value[2])
    if (respond_to(s, "std"))
      code = {@code, @this:p(s)};
    else
      code = {@code, this:p(s) + ";"};
    endif
  endfor

  i = 3;
  while (length(value) >= i && typeof(value[i]) != LIST)
    code = {@code, tostr("elseif (", this:p(value[i]), ")")};

    i = i + 1;

    for s in (value[i])
      if (respond_to(s, "std"))
        code = {@code, @this:p(s)};
      else
        code = {@code, this:p(s) + ";"};
      endif
    endfor

    i = i + 1;
  endwhile

  if (length(value) == i)
    code = {@code, "else"};

    for s in (value[i])
      if (respond_to(s, "std"))
        code = {@code, @this:p(s)};
      else
        code = {@code, this:p(s) + ";"};
      endif
    endfor
  endif

  code = {@code, "endif"};

  return code;

elseif (isa(statement, this.plastic.for_statement_proto))
  value =  statement.value;

  if (statement.subtype == "range")
    code = {tostr("for ", this:p(value[1]), " in [", this:p(value[2]), "..", this:p(value[3]), "]")};
    statements = value[4];
  elseif (length(value) == 4)
    code = {tostr("for ", this:p(value[1]), ", ", this:p(value[2]), " in (", this:p(value[3]), ")")};
    statements = value[4];
  else
    code = {tostr("for ", this:p(value[1]), " in (", this:p(value[2]), ")")};
    statements = value[3];
  endif

  for s in (statements)
    if (respond_to(s, "std"))
      code = {@code, @this:p(s)};
    else
      code = {@code, this:p(s) + ";"};
    endif
  endfor

  code = {@code, "endfor"};

  return code;

elseif (isa(statement, this.plastic.loop_statement_proto))
  value =  statement.value;

  i = 0;

  if (length(value) > 2)
    prefix = tostr("while ", this:p(value[i = i + 1]));
  else
    prefix = tostr("while");
  endif

  if (statement.id == "while")
    code = {tostr(prefix, " (", this:p(value[i = i + 1]), ")")};
  else
    code = {tostr(prefix, " (!(", this:p(value[i = i + 1]), "))")};
  endif

  for s in (value[i = i + 1])
    if (respond_to(s, "std"))
      code = {@code, @this:p(s)};
    else
      code = {@code, this:p(s) + ";"};
    endif
  endfor

  code = {@code, "endwhile"};

  return code;

elseif (isa(statement, this.plastic.fork_statement_proto))
  value =  statement.value;

  i = 0;

  if (length(value) > 2)
    code = {tostr("fork ", this:p(value[i = i + 1]), " (", this:p(value[i = i + 1]), ")")};
  else
    code = {tostr("fork", " (", this:p(value[i = i + 1]), ")")};
  endif

  for s in (value[i = i + 1])
    if (respond_to(s, "std"))
      code = {@code, @this:p(s)};
    else
      code = {@code, this:p(s) + ";"};
    endif
  endfor

  code = {@code, "endfork"};

  return code;

elseif (isa(statement, this.plastic.try_statement_proto))
  value =  statement.value;

  code = {"try"};

  for s in (value[1])
    if (respond_to(s, "std"))
      code = {@code, @this:p(s)};
    else
      code = {@code, this:p(s) + ";"};
    endif
  endfor

  if (statement.subtype == "finally")
    code = {@code, "finally"};

    for s in (value[2])
      if (respond_to(s, "std"))
        code = {@code, @this:p(s)};
      else
        code = {@code, this:p(s) + ";"};
      endif
    endfor

  else
    for value in (value[2..$])
      if (length(value) == 3)
        x = {};
        for s in (value[2])
          x = {@x, this:p(s)};
        endfor

        code = {@code, tostr("except ", this:p(value[1]), " (", x:join(", "), ")")};
        statements = value[3];

      else
        x = {};
        for s in (value[1])
          x = {@x, this:p(s)};
        endfor

        code = {@code, tostr("except (", x:join(", "), ")")};
        statements = value[2];

      endif

      for s in (statements)
        if (respond_to(s, "std"))
          code = {@code, @this:p(s)};
        else
          code = {@code, this:p(s) + ";"};
        endif
      endfor

    endfor
  endif

  code = {@code, "endtry"};

  return code;

elseif (isa(statement, this.plastic.assignment_operator_proto))
  if (statement.first.type == "pattern")
    res = "{";
    rest = 0;
    for v in (statement.first.value)
      if (v.type == "unary")
        v = tostr("@", this:p(v.first));
      elseif (v.type == "binary")
        v = tostr("?", this:p(v.first), " = ", this:p(v.second));
      else
        v = this:p(v);
      endif
      res = tostr(res, (rest ? ", " | ""), v);
      rest = 1;
    endfor
    res = tostr(res, "}");
    return tostr("(", res, " ", statement.value, " ", this:p(statement.second), ")");
  else
    return tostr("(", this:p(statement.first), " ", statement.value, " ", this:p(statement.second), ")");
  endif

elseif (isa(statement, this.plastic.bracket_operator_proto))
  if (statement.type == "ternary")
    return tostr("(", this:p(statement.first), "[", this:p(statement.second), "..", this:p(statement.third), "])");
  elseif (statement.type == "binary")
    return tostr("(", this:p(statement.first), "[", this:p(statement.second), "])");
  else
    res = "[";
    first = 1;
    for v in (statement.value)
      ticks_left() < 10000 || seconds_left() < 2 && suspend(0);
      res = tostr(res, (first ? "" | ", "), this:p(v[1]), " -> ", this:p(v[2]));
      first = 0;
    endfor
    res = tostr(res, "]");
    return res;
    return {res};
  endif

elseif (isa(statement, this.plastic.brace_operator_proto))
  res = "{";
  first = 1;
  for v in (statement.value)
    ticks_left() < 10000 || seconds_left() < 2 && suspend(0);
    res = tostr(res, (first ? "" | ", "), this:p(v));
    first = 0;
  endfor
  res = tostr(res, "}");
  return res;

elseif (isa(statement, this.plastic.invocation_operator_proto))
  if (statement.type == "ternary")
    a = {};
    for v in (statement.third)
      a = {@a, this:p(v)};
    endfor
    if (statement.second.type == "identifier")
      return tostr(this:p(statement.first), ":", this:p(statement.second), "(", a:join(", "), ")");
    else
      return tostr(this:p(statement.first), ":(", this:p(statement.second), ")(", a:join(", "), ")");
    endif
  elseif (statement.type == "binary")
    a = {};
    for v in (statement.second)
      a = {@a, this:p(v)};
    endfor
    return tostr(this:p(statement.first), "(", a:join(", "), ")");
  else
    return tostr(this:p(statement.first));
  endif

elseif (isa(statement, this.plastic.property_selector_operator_proto))
  if (statement.second.type == "identifier")
    return tostr(this:p(statement.first), ".", this:p(statement.second));
  else
    return tostr(this:p(statement.first), ".(", this:p(statement.second) + ")");
  endif

elseif (isa(statement, this.plastic.error_catching_operator_proto))
  if (statement.type == "unary")
    return tostr(statement.value, this:p(statement.first));
  endif

  x = {};
  for s in (statement.second)
    x = {@x, this:p(s)};
  endfor

  second = x:join(", ");

  if (statement.type == "ternary")
    return tostr("`", this:p(statement.first), " ! ", second, " => ", this:p(statement.third), "'");
  else
    return tostr("`", this:p(statement.first), " ! ", second, "'");
  endif

elseif (isa(statement, this.plastic.literal_proto))
  return toliteral(statement.value);
elseif (isa(statement, this.plastic.positional_symbol_proto))
  return statement.value;
elseif (isa(statement, this.plastic.prefix_operator_proto))
  return tostr(statement.value, this:p(statement.first));
elseif (isa(statement, this.plastic.infix_operator_proto))
  value = statement.value;
  value = (value != "**") ? value | "^";
  return tostr("(", this:p(statement.first), " ", value, " ", this:p(statement.second), ")");
elseif (isa(statement, this.plastic.traditional_ternary_operator_proto))
  return tostr("(", this:p(statement.first), " ? ", this:p(statement.second), " | ", this:p(statement.third), ")");
elseif (isa(statement, this.plastic.name_proto))
  return statement.value;
else
  raise(E_INVARG);
endif
.

; /* PRINTER */

@program $plastic.printer:_print as application/x-moocode
{statement, ?indent = ""} = args;

if (typeof(statement) == LIST)
  result = {tostr(indent, "-")};

  for item in (statement)
    result = {@result, @this:_print(item, indent + "  ")};
  endfor

  return result;
endif

if (`typeof(statement.value) == LIST ! ANY')
  result = {tostr(indent, statement.id, " : ", statement.type)};

  for value in (statement.value)
    result = {@result, @this:_print(value, indent + "  ")};
  endfor
else
  result = {tostr(indent, typeof(statement.value) == ERR ? toliteral(statement.value) | statement.value, " : ", statement.type)};

  for prop in ({"first", "second", "third"})
    if (`value = statement.(prop) ! E_PROPNF' != E_PROPNF && value != 0)
      result = {@result, @this:_print(value, indent + "  ")};
    endif
  endfor
endif

return result;
.

@program $plastic.printer:print as application/x-moocode
{source, ?options = []} = args;

tokenizer = this.plastic.tokenizer_proto:create(source);
parser = this.plastic.parser_proto:create(tokenizer);

statements = parser:statements();

source = {};

for statement in (statements)
  source = {@source, @this:_print(statement)};
endfor

return source;
.

; /* TOKENIZER */

@program $plastic.tokenizer_proto:create as application/x-moocode
this:_ensure_prototype();

instance = create(this, 1);

instance.row = 1;
instance.column = 1;
instance.source = (length(args) == 1 && typeof(args[1]) == LIST) ? args[1] | args;

return instance;
.

@program $plastic.tokenizer_proto:advance as application/x-moocode
this:_ensure_instance();

this.token = 0;

if (!this.source)
  return this;
endif

row = this.row;
column = this.column;
source = this.source;

eol = 0;
block_comment = 0;
inline_comment = 0;

while loop (length(source) >= row)

  if (column > (len = length(source[row])))
    eol = 1;
    inline_comment = 0;
    row = row + 1;
    column = 1;
    continue loop;
  endif

  next_two = len > column ? source[row][column..column + 1] | "";

  if (block_comment && next_two == "*/")
    block_comment = 0;
    column = column + 2;
    continue loop;
  elseif (next_two == "/*")
    block_comment = 1;
    column = column + 2;
    continue loop;
  elseif (next_two == "//")
    inline_comment = 1;
    column = column + 2;
    continue loop;
  endif

  if (block_comment || inline_comment)
    column = column + 1;
    continue loop;
  endif

  if (len >= column && ((c = source[row][column]) == "      " || c == " "))
    column = column + 1;
    continue loop;
  endif

  if (this.token)
    this.token["eol"] = eol;
    eol = 0;
    break loop;
  endif

  if (`c = source[row][column] ! E_RANGE')

    /* name and error */
    /* MOO error literals look like names but they're not.  Worse, a
     * valid error like E_PERM is treated like a literal, while an
     * invalid error like E_FOO is treated like a variable.  Any name
     * that starts with the characters "E_" is *now* an error literal,
     * but invalid errors are errors.
     */
    if ((c >= "a" && c <= "z") || c == "_" || c == "$")
      col1 = column; /* mark the start */
      column = column + 1;
      while (`c = source[row][column] ! E_RANGE')
        if ((c >= "a" && c <= "z") || (c >= "0" && c <= "9") || c == "_")
          column = column + 1;
        else
          break;
        endif
      endwhile
      col2 = column - 1;
      chars = source[row][col1..col2];
      if (index(chars, "E_") == 1)
        try
          this.token = ["type" -> "error", "value" -> this.errors[chars]];
        except ex (E_RANGE)
          this.token = ["type" -> "error", "value" -> chars, "error" -> tostr("Invalid error: ", chars)];
        endtry
      else
        this.token = ["type" -> "name", "value" -> chars];
      endif
      continue loop;

    /* object number */
    elseif (c == "#")
      col1 = column; /* mark the start */
      column = column + 1;
      if (`c = source[row][column] ! E_RANGE')
        if (c == "+" || c == "-")
          column = column + 1;
        endif
      endif
      while (`c = source[row][column] ! E_RANGE')
        if (c >= "0" && c <= "9")
          column = column + 1;
        else
          break;
        endif
      endwhile
      col2 = column - 1;
      chars = source[row][col1..col2];
      if (chars[$] < "0" || chars[$] > "9")
        this.token = ["type" -> "object", "value" -> chars, "error" -> "Bad object number"];
      elseif (c >= "a" && c <= "z")
        this.token = ["type" -> "object", "value" -> chars + c, "error" -> "Bad object number"];
      else
        this.token = ["type" -> "object", "value" -> toobj(chars)];
      endif
      continue loop;

    /* number */
    elseif (c >= "0" && c <= "9")
      float = 0;
      col1 = column; /* mark the start */
      column = column + 1;
      while (`c = source[row][column] ! E_RANGE')
        if (c >= "0" && c <= "9")
          column = column + 1;
        else
          break;
        endif
      endwhile
      if (c == "." && ((cc = `source[row][column + 1] ! E_RANGE') != ".")) /* not `..' */
        float = 1;
        column = column + 1;
        while (`c = source[row][column] ! E_RANGE')
          if (c >= "0" && c <= "9")
            column = column + 1;
          else
            break;
          endif
        endwhile
      endif
      if (c == "e")
        float = 1;
        column = column + 1;
        if (`c = source[row][column] ! E_RANGE' && c in {"-", "+"})
          column = column + 1;
        endif
        while (`c = source[row][column] ! E_RANGE')
          if (c >= "0" && c <= "9")
            column = column + 1;
          else
            break;
          endif
        endwhile
      endif
      col2 = column - 1;
      chars = source[row][col1..col2];
      if ((chars[$] < "0" || chars[$] > "9") && chars[$] != ".")
        this.token = ["type" -> "number", "value" -> chars, "error" -> "Bad number"];
      elseif (c >= "a" && c <= "z")
        this.token = ["type" -> "number", "value" -> chars + c, "error" -> "Bad number"];
      else
        this.token = ["type" -> "number", "value" -> float ? tofloat(chars) | toint(chars)];
      endif
      continue loop;

    /* string */
    elseif (c == "\"" || c == "'")
      esc = 0;
      chars = "";
      q = c;
      col1 = column; /* mark the start */
      column = column + 1;
      while (`c = source[row][column] ! E_RANGE' && (c != q || esc))
        column = column + 1;
        if (c != "\\" || esc)
          chars = tostr(chars, c);
          esc = 0;
        else
          esc = 1;
        endif
      endwhile
      column = column + 1;
      col2 = column - 1;
      if (c != q)
        this.token = ["type" -> "string", "value" -> source[row][col1..col2 - 1], "error" -> "Unterminated string"];
        continue loop;
      else
        this.token = ["type" -> "string", "value" -> chars];
        continue loop;
      endif

    /* possible multi-character operator */
    elseif (index("-+<>=*/%!|&.", c))
      col1 = column; /* mark the start */
      column = column + 1;
      if (`c = source[row][column] ! E_RANGE' && index(">=*!|&.", c))
        column = column + 1;
        this.token = ["type" -> "operator", "value" -> source[row][col1..column - 1]];
        continue loop;
      else
        this.token = ["type" -> "operator", "value" -> source[row][col1]];
        continue loop;
      endif

    /* operator */
    else
      column = column + 1;
      this.token = ["type" -> "operator", "value" -> c];
      continue loop;

    endif

    column = column + 1;
  endif
endwhile

this.row = row;
this.column = column;
this.source = source;

/* check for unterminated comment */
if (block_comment)
  this.token = ["type" -> "comment", "value" -> "", "error" -> "Unterminated comment"];
endif

/* dollar sign by itself is not a name */
if (this.token && this.token["type"] == "name" && this.token["value"] == "$")
  this.token["type"] = "operator";
endif

/* catch the last token */
if (row > length(source) && this.token)
  this.token["eol"] = 1;
endif

return this;
.

@program $plastic.tokenizer_proto:token as application/x-moocode
this:_ensure_instance();

return this.token;
.

; /* PARSER */

@program $plastic.parser_proto:create as application/x-moocode
this:_ensure_prototype();

{tokenizer, @options} = args;

instance = create(this, 1);
instance.tokenizer = tokenizer;
instance.symbols = [];

plastic = this.plastic;

/* `(end)' is required */
instance:symbol("(end)");
instance:symbol("(name)", 0, plastic.name_proto);
instance:symbol("(literal)", 0, plastic.literal_proto);
instance:symbol(";", 0, plastic.operator_proto);
instance:symbol(",", 0, plastic.operator_proto);
instance:symbol("]", 0, plastic.operator_proto);
instance:symbol("}", 0, plastic.operator_proto);
instance:symbol("->", 0, plastic.operator_proto);
instance:symbol("=>", 0, plastic.operator_proto);
instance:symbol("..", 0, plastic.operator_proto);
instance:symbol("|", 0, plastic.operator_proto);
instance:symbol("`", 0, plastic.operator_proto);
instance:symbol("!", 0, plastic.prefix_operator_proto);
instance:symbol("!!", 50, plastic.error_catching_operator_proto);
instance:symbol("=", 100, plastic.assignment_operator_proto);
instance:symbol("+=", 100, plastic.compound_assignment_operator_proto);
instance:symbol("-=", 100, plastic.compound_assignment_operator_proto);
instance:symbol("*=", 100, plastic.compound_assignment_operator_proto);
instance:symbol("/=", 100, plastic.compound_assignment_operator_proto);
instance:symbol("%=", 100, plastic.compound_assignment_operator_proto);
instance:symbol("?", 200, plastic.traditional_ternary_operator_proto);
instance:symbol("&&", 300, plastic.infix_operator_proto, ["right" -> 1]);
instance:symbol("||", 300, plastic.infix_operator_proto, ["right" -> 1]);
instance:symbol("!=", 400, plastic.infix_operator_proto);
instance:symbol("==", 400, plastic.infix_operator_proto);
instance:symbol("<", 400, plastic.infix_operator_proto);
instance:symbol("<=", 400, plastic.infix_operator_proto);
instance:symbol(">", 400, plastic.infix_operator_proto);
instance:symbol(">=", 400, plastic.infix_operator_proto);
instance:symbol("in", 400, plastic.infix_operator_proto);
instance:symbol("+", 500, plastic.sign_operator_proto);
instance:symbol("-", 500, plastic.sign_operator_proto);
instance:symbol("*", 600, plastic.infix_operator_proto);
instance:symbol("/", 600, plastic.infix_operator_proto);
instance:symbol("%", 600, plastic.infix_operator_proto);
instance:symbol("**", 650, plastic.infix_operator_proto);
instance:symbol("[", 800, plastic.bracket_operator_proto);
instance:symbol("{", 0, plastic.brace_operator_proto); /* never bind left */
instance:symbol("return", 0, plastic.control_flow_statement_proto);
instance:symbol("break", 0, plastic.control_flow_statement_proto);
instance:symbol("continue", 0, plastic.control_flow_statement_proto);
instance:symbol("if", 0, plastic.if_statement_proto);
instance:symbol("for", 0, plastic.for_statement_proto);
instance:symbol("while", 0, plastic.loop_statement_proto);
instance:symbol("until", 0, plastic.loop_statement_proto);
instance:symbol("fork", 0, plastic.fork_statement_proto);
instance:symbol("try", 0, plastic.try_statement_proto);
instance:symbol("from", 0, plastic.from_statement_proto);
instance:symbol(":", 800, plastic.verb_selector_operator_proto);
instance:symbol(".", 800, plastic.property_selector_operator_proto);
/* the infix form is function/verb invocation */
instance:symbol("(", 800, plastic.invocation_operator_proto);
instance:symbol(")", 0, plastic.operator_proto);

return instance;
.

@program $plastic.parser_proto:symbol as application/x-moocode
this:_ensure_instance();

{id, ?bp = 0, ?proto = $nothing, ?options = []} = args;

proto = valid(proto) ? proto | this.plastic.symbol_proto;

if ((symbol = `this.symbols[id] ! E_RANGE') == E_RANGE)
  symbol = proto:create(id, bp, options);
endif

this.symbols[id] = symbol;

return symbol;
.

@program $plastic.parser_proto:reserve_statement as application/x-moocode
this:_ensure_instance();

{symbol} = args;

id = symbol.id;

/* raise error if this symbol is not a name, statement or keyword */
if ((type = this.symbols[id].type) != "name" && type != "statement" && type != "keyword")
  an_or_a = index("aeiou", type[1]) ? "an" | "a";
  raise("Syntax error", tostr("`", id, "' is ", an_or_a, " ", type), this);
endif

symbol.reserved = 1;
symbol.type = verb[9..$];

this.symbols[id] = symbol;
.

@program $plastic.parser_proto:make_identifier as application/x-moocode
this:_ensure_instance();

{symbol} = args;

id = symbol.id;

/* raise error if this symbol is reserved */
if (this.symbols[id].reserved)
  raise("Syntax error", tostr("`", id, "' is reserved"), this);
endif

symbol.reserved = 0;
symbol.type = verb[6..$];

this.symbols[id] = symbol;
.

@program $plastic.parser_proto:token as application/x-moocode
this:_ensure_instance();

{?ttid = 0} = args;

if (this.token == 0)
  this.tokenizer:advance();
  token = this.tokenizer.token;
  if (token)
    type = token["type"];
    value = token["value"];
    eol = token["eol"];
    if (`token["error"] ! E_RANGE')
      raise("Syntax error", token["error"], this);
    elseif (type == "number" || type == "string" || type == "object" || type == "error")
      symbol = this:symbol("(literal)");
      this.token = symbol:clone();
      this.token.type = type;
      this.token.value = value;
      this.token.eol = eol;
    elseif (type == "operator")
      /* Update the symbol table itself and give the operator the
       * initial type "operator" (the type will change to "unary",
       * "binary" or "ternary" when we learn how this symbol is used in
       * the program).
       */
      /* check the symbol table */
      if ((symbol = `this.symbols[value] ! E_RANGE') == E_RANGE)
        raise("Syntax error", tostr("Unknown operator:  `", value, "'"), this);
      endif
      this.token = symbol:clone();
      this.token.type = "operator";
      this.token.value = value;
      this.token.eol = eol;
    elseif (type == "name")
      /* Update the symbol table itself and give the name the initial
       * type "name" (the type will change to "variable", "identifier",
       * "statement" or "keyword" when we learn how this symbol is used
       * in the program).
       */
      id = value;
      /* peek into the symbol table */
      if ((symbol = `this.symbols[id] ! E_RANGE') != E_RANGE)
        this.token = symbol:clone();
        this.symbols[id] = this.token;
        this.token.type = symbol.type || "name";
        this.token.id = id;
        this.token.value = value;
        this.token.eol = eol;
      else
        symbol = this:symbol("(name)");
        this.token = symbol:clone();
        this.symbols[id] = this.token;
        this.token.type = "name";
        this.token.id = id;
        this.token.value = value;
        this.token.eol = eol;
      endif
    else
      raise("Syntax error", "Unexpected token", this);
    endif
  else
    symbol = this:symbol("(end)");
    this.token = symbol:clone();
  endif
endif

if (ttid)
  this:advance(ttid);
endif

return this.token;
.

@program $plastic.parser_proto:advance as application/x-moocode
this:_ensure_instance();

{?id = 0} = args;

/* raise error if token doesn't match expectation */
if (id && this.token != 0 && this.token.id != id)
  raise("Syntax error", tostr("Expected `", id, "'"), this);
endif

this.token = 0;

return this;
.

@program $plastic.parser_proto:expression as application/x-moocode
this:_ensure_instance();

{?bp = 0} = args;

token = this:token();
this:advance();

/* don't call `nud()' and/or `led()' on `(end)' */
if (token.id == "(end)")
  return token;
endif

left = token:nud(this);

while (bp < this:token().bp)
  this.plastic.utilities:suspend_if_necessary();

  token = this:token();
  this:advance();
  left = token:led(this, left);
endwhile

return left;
.

@program $plastic.parser_proto:statement as application/x-moocode
this:_ensure_instance();

token = this:token();

/* disregarded naked semicolons */
while (token.id == ";")
  this:advance();
  token = this:token();
endwhile

/* either the beginning of a statement */
/* or an expression with an optional semicolon */
if (respond_to(token, "std"))
  this:advance();
  return token:std(this);
else
  expression = this:expression();
  if (this:token().id == ";")
    this:advance();
  endif
  return expression;
endif
.

@program $plastic.parser_proto:statements as application/x-moocode
this:_ensure_instance();

terminals = args;

statements = {};

while (1)
  this.plastic.utilities:suspend_if_necessary();

  token = this:token();
  if (token.id == "(end)" || ((token.type in {"name", "statement", "keyword"}) && token.value in terminals))
    break;
  endif
  statement = this:statement();
  if (statement.id == "(end)")
    break;
  endif
  statements = {@statements, statement};
endwhile

return statements;
.

@program $plastic.parser_proto:parse_all as application/x-moocode
this:_ensure_instance();

/* This is the API entry-point for clients that want to turn a
 * stream of tokens into a syntax tree and to return it for further
 * modification, changing ownership to the client in the process.
 * Assumes "change owner" permission has been granted by the
 * caller.
 */

stack = statements = this:statements();

while (stack)
  this.plastic.utilities:suspend_if_necessary();
  {top, @stack} = stack;
  stack = {@stack, @this.plastic.utilities:children(top)};
  if (typeof(top) == ANON && isa(top, this.plastic.symbol_proto))
    this.object_utilities:change_owner(top, caller_perms());
  endif
endwhile

return statements;
.

@program $plastic.parser_proto:push as application/x-moocode
this:_ensure_instance();

definition = args;
{id, @rest} = definition;

new = 0;

if (`this.symbols[id] ! E_RANGE' == E_RANGE)
  this:symbol(@definition);
  new = 1;
endif

return {new, id};
.

@program $plastic.parser_proto:pop as application/x-moocode
this:_ensure_instance();

{args} = args;
{new, id} = args;

if (new)
  this.symbols = this.symbols:delete(id);
endif
.


; /* UTILITIES */

@program $plastic.utilities:suspend_if_necessary as application/x-moocode
(ticks_left() < 10000 || seconds_left() < 2) && suspend(0);
.

@program $plastic.utilities:parse_map_sequence as application/x-moocode
{parser, separator, infix, terminator, ?symbols = {}} = args;

ids = {};
for symbol in (symbols)
  ids = {@ids, parser:push(@symbol)};
endfor

if (terminator && parser:token().id == terminator)
  return {};
endif

key = parser:expression(0);
parser:advance(infix);
value = parser:expression(0);
map = {{key, value}};

while (parser:token().id == separator)
  this:suspend_if_necessary();
  map && parser:advance(separator);
  key = parser:expression(0);
  parser:advance(infix);
  value = parser:expression(0);
  map = {@map, {key, value}};
endwhile

for id in (ids)
  parser:pop(id);
endfor

return map;
.

@program $plastic.utilities:parse_list_sequence as application/x-moocode
{parser, separator, terminator, ?symbols = "defaults"} = args;

/* enable defaults */
if (symbols && symbols == "defaults")
  symbols = {{"@", 0, this.plastic.prefix_operator_proto}};
endif

ids = {};
for symbol in (symbols)
  ids = {@ids, parser:push(@symbol)};
endfor

if (terminator && parser:token().id == terminator)
  return {};
endif

expression = parser:expression(0);
list = {expression};

while (parser:token().id == separator)
  this:suspend_if_necessary();
  parser:advance(separator);
  expression = parser:expression(0);
  list = {@list, expression};
endwhile

for id in (ids)
  parser:pop(id);
endfor

return list;
.

@program $plastic.utilities:validate_scattering_pattern as application/x-moocode
{parser, pattern} = args;

state = 1;

for element in (pattern)
  if (state == 1 && element.type == "variable")
    continue;
  elseif ((state == 1 || state == 2) && element.type == "binary" && element.id == "=")
    if (element.first.type == "variable")
      state = 2;
      continue;
    endif
  elseif ((state == 1 || state == 2) && element.type == "unary" && element.id == "@")
    if (element.first.type == "variable")
      state = 3;
      continue;
    endif
  endif

  raise("Syntax error", "Illegal scattering pattern", parser);
endfor
.

@program $plastic.utilities:children as application/x-moocode
{node} = args;

/* Intelligently gather children from various places.
 */

if (typeof(node) == LIST)
  return node;
elseif (`typeof(value = node.value) == LIST ! ANY')
  return value;
else
  children = {};
  for prop in ({"first", "second", "third"})
    if (`value = node.(prop) ! E_PROPNF => 0' != 0)
      children = {@children, value};
    endif
  endfor
  return children;
endif
.

@program $plastic.utilities:match as application/x-moocode
{root, pattern} = args;

/* Pattern is a map.  The keys specify the properties (`id', `type',
 * etc.) to match on.  The values specify the property values for the
 * match comparison.  Conducts a depth first search for pattern.
 */

keys = pattern:keys();
matches = {};
stack = {root};

while next (stack)
  this:suspend_if_necessary();
  {top, @stack} = stack;
  stack = {@stack, @this:children(top)};
  if (typeof(top) == ANON && isa(top, this.plastic.symbol_proto))
    for key in (keys)
      if (top.(key) != pattern[key])
        continue next;
      endif
    endfor
    matches = {@matches, top};
  endif
endwhile

return matches;
.


@program $plastic.symbol_proto:create as application/x-moocode
{id, ?bp = 0, ?opts = []} = args;
(typeof(this) == OBJ) || raise(E_PERM, "Call not allowed on anonymous object");
instance = create(this, 1);
instance.id = id;
instance.value = id;
instance.bp = bp;
for v, k in (opts)
  if (k in {"id", "type", "bp", "right"})
    instance.(k) = v;
  endif
endfor
return instance;
.

@program $plastic.symbol_proto:clone as application/x-moocode
(typeof(this) == OBJ) && raise(E_PERM, "Call not allowed on permanent object");
parents = parents(this);
instance = create(parents, 1);
for ancestor in (ancestors(this))
  for property in (`properties(ancestor) ! E_PERM => {}')
    instance.(property) = this.(property);
  endfor
endfor
return instance;
.

@program $plastic.symbol_proto:nud as application/x-moocode
{parser} = args;
raise("Syntax error", tostr("Undefined: ", this.id), parser);
.

@program $plastic.symbol_proto:led as application/x-moocode
{parser, _} = args;
raise("Syntax error", tostr("Missing operator: ", this.id), parser);
.


@program $plastic.literal_proto:nud as application/x-moocode
return this;
.


@program $plastic.positional_symbol_proto:nud as application/x-moocode
return this;
.


@program $plastic.prefix_operator_proto:nud as application/x-moocode
{parser} = args;

first = parser:expression(700);

if (first.id == "(end)")
  raise("Syntax error", "Expected an expression", parser);
endif

this.type = "unary";
this.first = first;

return this;
.


@program $plastic.infix_operator_proto:led as application/x-moocode
{parser, first} = args;

right = this.right && 1; /* does this operator associate to the right? */

second = parser:expression(this.bp - right);

if (second.id == "(end)")
  raise("Syntax error", "Expected an expression", parser);
endif

this.type = "binary";
this.first = first;
this.second = second;

return this;
.


@program $plastic.sign_operator_proto:nud as application/x-moocode
{parser} = args;

first = parser:expression(700);

if (first.id == "(end)")
  raise("Syntax error", "Expected an expression", parser);
endif

this.type = "unary";
this.first = first;

return this;
.

@program $plastic.sign_operator_proto:led as application/x-moocode
{parser, first} = args;

second = parser:expression(this.bp);

if (second.id == "(end)")
  raise("Syntax error", "Expected an expression", parser);
endif

this.type = "binary";
this.first = first;
this.second = second;

return this;
.


@program $plastic.statement_proto:std as application/x-moocode
{parser} = args;
raise("Syntax error", "Undefined", parser);
.



@program $plastic.control_flow_statement_proto:std as application/x-moocode
{parser} = args;

if (this.id != "return" && parser.loop_depth < 1)
  raise("Syntax error", tostr("No enclosing loop for ", this.id), parser);
endif

/* update the symbol table */
parser:reserve_statement(this);

if (!this.eol && parser:token().id != ";")
  expression = parser:expression(0);
  this.first = expression;
endif

if (this.id != "return" && this.first != 0)
  if (this.first.type != "variable")
    raise("Syntax error", "Loop name must be a name", parser);
  endif
  if (!(this.first.value in parser.loop_variables))
    raise("Syntax error", tostr("Invalid loop name for ", this.id), parser);
  endif
endif

if (parser:token().id == ";")
  parser:advance(";");
endif

return this;
.


@program $plastic.if_statement_proto:std as application/x-moocode
{parser} = args;

/* update the symbol table */
parser:reserve_statement(this);

a = {};

/* the predicate */
parser:token("(");
expression = parser:expression(0);
a = {@a, expression};
parser:token(")");

/* the consequent */
statements = parser:statements("elseif", "else", "endif", "end");
a = {@a, statements};

/* the alternatives */
while (parser:token().id == "elseif")
  parser:advance("elseif");

  /* predicate */
  parser:token("(");
  expression = parser:expression(0);
  a = {@a, expression};
  parser:token(")");

  /* consequent */
  statements = parser:statements("elseif", "else", "endif", "end");
  a = {@a, statements};

  /* update the symbol table */
  symbol = parser.symbols["elseif"];
  parser:reserve_keyword(symbol);
endwhile

/* the final alternative */
if (parser:token().id == "else")
  parser:advance("else");

  statements = parser:statements("endif", "end");
  a = {@a, statements};

  /* update the symbol table */
  symbol = parser.symbols["else"];
  parser:reserve_keyword(symbol);
endif

/* the last token must be "endif" or "end" */
if ((id = parser:token().id) == "endif")
  parser:advance("endif");
else
  parser:advance("end");
endif

/* update the symbol table */
symbol = parser.symbols[id];
parser:reserve_keyword(symbol);

/* store the parts in this token's `value' */
this.value = a;

return this;
.


@program $plastic.for_statement_proto:std as application/x-moocode
{parser} = args;

/* update the symbol table */
parser:reserve_statement(this);

a = {};

/* the index(s) */
variables = {};

variable = parser:token();
parser:make_variable(variable);
parser:advance();
variables = {@variables, variable};

if (parser:token().id == ",")
  while (parser:token().id == ",")
    parser:advance(",");
    variable = parser:token();
    parser:make_variable(variable);
    parser:advance();
    variables = {@variables, variable};
  endwhile
elseif (parser:token().id == "->")
  while (parser:token().id == "->")
    parser:advance("->");
    variable = parser:token();
    parser:make_variable(variable);
    parser:advance();
    variables = {variable, @variables};
  endwhile
endif

a = {@a, @variables};

for _, i in (variables)
  variables[i] = variables[i].id;
endfor

parser:token("in");

/* update the symbol table */
symbol = parser.symbols["in"];
parser:reserve_keyword(symbol);

/* could be a range or a collection */
if (parser:token().id == "[")
  /* range */
  this.subtype = "range";
  length(a) < 2 || raise("Syntax error", "Too many loop variables", parser);
  parser:token("[");
  first = parser:expression(0);
  a = {@a, first};
  parser:token("..");
  second = parser:expression(0);
  a = {@a, second};
  parser:token("]");
else
  /* collection */
  this.subtype = "collection";
  length(a) < 3 || raise("Syntax error", "Too many loop variables", parser);
  parser:token("(");
  expression = parser:expression(0);
  a = {@a, expression};
  parser:token(")");
endif

/* the body */
parser.loop_variables = {@parser.loop_variables, @variables};
parser.loop_depth = parser.loop_depth + 1;

statements = parser:statements("endfor", "end");
a = {@a, statements};

l = length(variables);
parser.loop_variables = parser.loop_variables[1..$ - l];
parser.loop_depth = parser.loop_depth - 1;

/* the last token must be "endfor" or "end" */
if ((id = parser:token().id) == "endfor")
  parser:advance("endfor");
else
  parser:advance("end");
endif

/* update the symbol table */
symbol = parser.symbols[id];
parser:reserve_keyword(symbol);

/* store the parts in this token's `value' */
this.value = a;

return this;
.


@program $plastic.loop_statement_proto:std as application/x-moocode
{parser} = args;

/* update the symbol table */
parser:reserve_statement(this);

end = tostr("end", this.id);

a = {};

/* possible, optional loop name */
variables = {};
if ((type = parser:token().type) == "variable" || type == "name")
  variable = parser:token();
  parser:make_variable(variable);
  parser:advance();
  variables = {@variables, variable.id};
  a = {@a, variable};
endif

/* the condition */
parser:token("(");
expression = parser:expression(0);
a = {@a, expression};
parser:token(")");

/* the body */
parser.loop_variables = {@parser.loop_variables, @variables};
parser.loop_depth = parser.loop_depth + 1;

statements = parser:statements(end, "end");
a = {@a, statements};

l = length(variables);
parser.loop_variables = parser.loop_variables[1..$ - l];
parser.loop_depth = parser.loop_depth - 1;

/* the last token must be "endwhile/enduntil" or "end" */
if ((id = parser:token().id) == end)
  parser:advance(end);
else
  parser:advance("end");
endif

/* update the symbol table */
symbol = parser.symbols[id];
parser:reserve_keyword(symbol);

/* store the parts in this token's `value' */
this.value = a;

return this;
.


@program $plastic.fork_statement_proto:std as application/x-moocode
{parser} = args;

/* update the symbol table */
parser:reserve_statement(this);

a = {};

/* possible, optional task name */
if ((type = parser:token().type) == "variable" || type == "name")
  variable = parser:token();
  parser:make_variable(variable);
  parser:advance();
  a = {@a, variable};
endif

/* the expression */
parser:token("(");
expression = parser:expression(0);
a = {@a, expression};
parser:token(")");

/* the body */
statements = parser:statements("endfork", "end");
a = {@a, statements};

/* the last token must be "endfork" or "end" */
if ((id = parser:token().id) == "endfork")
  parser:advance("endfork");
else
  parser:advance("end");
endif

/* update the symbol table */
symbol = parser.symbols[id];
parser:reserve_keyword(symbol);

/* store the parts in this token's `value' */
this.value = a;

return this;
.


@program $plastic.try_statement_proto:std as application/x-moocode
{parser} = args;

/* update the symbol table */
parser:reserve_statement(this);

a = {};

/* the body */
body = parser:statements("except", "finally", "endtry", "end");
a = {@a, body};

if (parser:token().id == "finally")
  parser:advance("finally");

  b = parser:statements("endtry", "end");

  /* update the symbol table */
  symbol = parser.symbols["finally"];
  parser:reserve_keyword(symbol);

  this.subtype = "finally";

  a = {@a, b};

else
  b = {};

  id = parser:push("@", 0, this.plastic.prefix_operator_proto);

  /* the exceptions */
  while (parser:token().id == "except")
    parser:advance("except");

    /* variable and codes */
    if ((variable = parser:token()).id != "(")
      parser:advance();

      if (variable.type != "name" && variable.type != "variable")
        raise("Syntax error", "Variable must be an identifier", parser);
      endif
      parser:make_variable(variable);

      parser:token("(");
      if ((token = parser:token()).id == "ANY")
        parser:advance("ANY");
        symbol = parser.symbols["ANY"];
        parser:reserve_keyword(symbol);
        codes = {token};
      else
        codes = this.plastic.utilities:parse_list_sequence(parser, ",", ")");
      endif
      parser:token(")");

      !codes && raise("Syntax error", "Codes may not be empty", parser);

      b = {variable, codes};

    /* just codes */
    else
      parser:token("(");
      if ((token = parser:token()).id == "ANY")
        parser:advance("ANY");
        symbol = parser.symbols["ANY"];
        parser:reserve_keyword(symbol);
        codes = {token};
      else
        codes = this.plastic.utilities:parse_list_sequence(parser, ",", ")");
      endif
      parser:token(")");

      !codes && raise("Syntax error", "Codes may not be empty", parser);

      b = {codes};

    endif

    /* handler */
    handler = parser:statements("except", "finally", "endtry", "end");
    b = {@b, handler};

    /* update the symbol table */
    symbol = parser.symbols["except"];
    parser:reserve_keyword(symbol);

    a = {@a, b};

  endwhile

  parser:pop(id);

  if (!b)
    raise("Syntax error", "Missing except", parser);
  endif

  this.subtype = "except";

endif

/* the last token must be "endtry" or "end" */
if ((id = parser:token().id) == "endtry")
  parser:advance("endtry");
else
  parser:advance("end");
endif

/* update the symbol table */
symbol = parser.symbols[id];
parser:reserve_keyword(symbol);

/* store the parts in this token's `value' */
this.value = a;

return this;
.


@program $plastic.assignment_operator_proto:led as application/x-moocode
{parser, first} = args;

if (first.type == "unary" && first.id == "{") /* scattering syntax */
  this.plastic.utilities:validate_scattering_pattern(parser, first.value);
  first.type = "pattern";
endif

if (first.type != "variable" && first.type != "pattern" && first.id != "." && first.id != "[")
  raise("Syntax error", "Illegal expression on left side of assignment", parser);
endif

second = parser:expression(this.bp - 1);

if (second.id == "(end)")
  raise("Syntax error", "Expected an expression", parser);
endif

this.type = "binary";
this.first = first;
this.second = second;

return this;
.


@program $plastic.compound_assignment_operator_proto:led as application/x-moocode
{parser, first} = args;

if (first.type != "variable" && first.id != "." && first.id != "[")
  raise("Syntax error", "Illegal expression on left side of assignment", parser);
endif

second = parser:expression(this.bp - 1);

if (second.id == "(end)")
  raise("Syntax error", "Expected an expression", parser);
endif

op = this.id[1];
bp = parser.symbols[op].bp;

inner = parser.plastic.infix_operator_proto:create(op, bp);
inner.type = "binary";
inner.first = first;
inner.second = second;

outer = parser.plastic.assignment_operator_proto:create("=", this.bp);
outer.type = "binary";
outer.first = first;
outer.second = inner;

return outer;
.


@program $plastic.name_proto:nud as application/x-moocode
{parser} = args;

/* Assume the name is a variable.  Subsequent usage may modify this
 * assumption.
 */
parser:make_variable(this);

return this;
.


@program $plastic.bracket_operator_proto:nud as application/x-moocode
{parser} = args;

sequence = this.plastic.utilities:parse_map_sequence(parser, ",", "->", "]");
parser:token("]");

this.type = "unary";
this.value = sequence;
this.first = this;

return this;
.

@program $plastic.bracket_operator_proto:led as application/x-moocode
{parser, first} = args;

caret = parser:push("^", 0, this.plastic.positional_symbol_proto);
dollar = parser:push("$", 0, this.plastic.positional_symbol_proto);

second = parser:expression(0);

if (parser:token().id == "..")
  parser:advance("..");
  third = parser:expression(0);
  parser:advance("]");
  this.type = "ternary";
  this.first = first;
  this.second = second;
  this.third = third;
else
  parser:advance("]");
  this.type = "binary";
  this.first = first;
  this.second = second;
endif

parser:pop(caret);
parser:pop(dollar);

return this;
.


@program $plastic.brace_operator_proto:nud as application/x-moocode
{parser} = args;

sequence = this.plastic.utilities:parse_list_sequence(parser, ",", "}");
parser:token("}");

this.type = "unary";
this.value = sequence;
this.first = this;

return this;
.



@program $plastic.invocation_operator_proto:nud as application/x-moocode
{parser} = args;

expression = parser:expression(0);
parser:token(")");

this.type = "unary";
this.first = expression;

return this;
.

@program $plastic.invocation_operator_proto:led as application/x-moocode
{parser, left} = args;

sequence = this.plastic.utilities:parse_list_sequence(parser, ",", ")");
parser:token(")");

/* The invocation operator handles function and verb invocation, and
 * guards against use on properties.
 */
if (left.id == ".")
  raise("Syntax error", "Invalid application of invocation", parser);
elseif (left.id == ":")
  first = left.first;
  second = left.second;

  /* the verb selector operator has our back */

  this.type = "ternary";
  this.first = first;
  this.second = second;
  this.third = sequence;
else
  if (left.type != "variable")
    raise("Syntax error", "Expected an identifier", parser);
  endif

  parser:make_identifier(left);

  if (`import = parser.imports[left.value] ! E_RANGE' != E_RANGE)
    this.type = "ternary";
    this.first = import;
    this.second = left;
    this.third = sequence;
  else
    this.type = "binary";
    this.first = left;
    this.second = sequence;
  endif
endif

return this;
.



@program $plastic.verb_selector_operator_proto:led as application/x-moocode
{parser, first} = args;

if (parser:token().id == "(")
  parser:advance("(");
  second = parser:expression(0);
  parser:advance(")");
else
  second = parser:expression(this.bp);
  if (second.type != "variable")
    raise("Syntax error", "Expected an identifier", parser);
  endif
  parser:make_identifier(second);
endif

if (parser:token().id != "(")
  raise("Syntax error", "Expected `('", parser);
endif

this.type = "binary";
this.first = first;
this.second = second;

return this;
.


@program $plastic.property_selector_operator_proto:led as application/x-moocode
{parser, first} = args;

if (parser:token().id == "(")
  parser:advance("(");
  second = parser:expression(0);
  parser:advance(")");
else
  second = parser:expression(this.bp);
  if (second.type != "variable")
    raise("Syntax error", "Expected an identifier", parser);
  endif
  parser:make_identifier(second);
endif

this.type = "binary";
this.first = first;
this.second = second;

return this;
.


@program $plastic.error_catching_operator_proto:nud as application/x-moocode
{parser} = args;

first = parser:expression(700);

if (first.id == "(end)")
  raise("Syntax error", "Expected an expression", parser);
endif

this.type = "unary";
this.first = first;

return this;
.

@program $plastic.error_catching_operator_proto:led as application/x-moocode
{parser, first} = args;

/* Error codes are either the keyword `ANY' or a list of expressions
 * (see 4.1.12 Catching Errors in Expressions).
 */
id = parser:push("@", 0, this.plastic.prefix_operator_proto);

if ((token = parser:token()).id == "ANY")
  parser:advance("ANY");
  symbol = parser.symbols["ANY"];
  parser:reserve_keyword(symbol);
  second = {token};
else
  second = this.plastic.utilities:parse_list_sequence(parser, ",", "");

  if (second[$].id == "(end)")
    raise("Syntax error", "Expected `ANY' or a list of expressions", parser);
  endif
endif

parser:pop(id);

if ((token = parser:token()).id == "=>")
  parser:advance("=>");
  third = parser:expression(0);

  if (third.id == "(end)")
    raise("Syntax error", "Expected an expression", parser);
  endif

  this.type = "ternary";
  this.first = first;
  this.second = second;
  this.third = third;
else
  this.type = "binary";
  this.first = first;
  this.second = second;
endif

return this;
.


@program $plastic.traditional_ternary_operator_proto:led as application/x-moocode
{parser, first} = args;

second = parser:expression(0);
parser:token("|");
third = parser:expression(0);

if (second.id == "(end)" || third.id == "(end)")
  raise("Syntax error", "Expected an expression", parser);
endif

this.type = "ternary";
this.first = first;
this.second = second;
this.third = third;

return this;
.

@program $plastic.from_statement_proto:std as application/x-moocode
{parser} = args;

/* update the symbol table */
parser:reserve_statement(this);

types = {"name", "variable", "identifier"};

/* the reference */
if ((target = parser:token()).type == "string")
  parser:advance();
elseif (target.type in types && target.value == "this")
  parser:advance();
elseif (target.type in types && target.value[1] == "$")
  target = parser:expression();
  if (target.id == ".")
    temp = target;
    while (temp.id == ".")
      if ((temp.first.id != "." && !(temp.first.type in types)) && !(temp.second.type in types))
        raise("Syntax error", tostr("Invalid reference: ", target.id), parser);
      endif
      temp = temp.first;
    endwhile
  elseif (target.value[1] == "$")
    /* ok */
  else
    raise("Syntax error", tostr("Invalid reference: ", target.id), parser);
  endif
else
  raise("Syntax error", tostr("Invalid reference: ", target.id), parser);
endif

parser:token("use");

/* update the symbol table */
symbol = parser.symbols["use"];
parser:reserve_keyword(symbol);

/* the import(s) */
imports = {};

if ((import = parser:token()).id == "(end)")
  raise("Syntax error", "Expected an identifier", parser);
elseif (!(import.type in types))
  raise("Syntax error", "Import must be an identifier", parser);
endif
parser:make_identifier(import);
parser:advance();
imports = {@imports, import};

while (parser:token().id == ",")
  parser:advance(",");
  if ((import = parser:token()).id == "(end)")
    raise("Syntax error", "Expected an identifier", parser);
  elseif (!(import.type in types))
    raise("Syntax error", "Import must be an identifier", parser);
  endif
  parser:make_identifier(import);
  parser:advance();
  imports = {@imports, import};
endwhile

/* generate code */
if (target.type == "string")
  temp = parser.plastic.invocation_operator_proto:create("(");
  temp.type = "binary";
  temp.first = parser.plastic.name_proto:create("$lookup");
  temp.first.type = "identifier";
  temp.first.value = "$lookup";
  temp.second = {target};
  target = temp;
endif

first = parser.plastic.name_proto:create(tostr(random()));
first.type = "unique";

if (!length(imports))
  raise("Syntax error", "Missing imports", parser);
endif

for import in (imports)
  if (`parser.imports[import.id] ! E_RANGE' != E_RANGE)
    raise("Syntax error", "Duplicate imports", parser);
  endif
  parser.imports[import.id] = first;
endfor

result = parser.plastic.assignment_operator_proto:create("=");
result.type = "binary";
result.first = first;
result.second = target;

return result;
.
