class Tomlrb::GeneratedParser
token IDENTIFIER STRING_MULTI STRING_BASIC STRING_LITERAL_MULTI STRING_LITERAL DATETIME LOCAL_TIME INTEGER NON_DEC_INTEGER FLOAT FLOAT_KEYWORD BOOLEAN NEWLINE EOS
rule
  expressions
    | expressions expression
    | expressions EOS
    ;
  expression
    : table
    | assignment
    | inline_table
    | NEWLINE
    ;
  table
    : table_start table_identifier table_end newlines
    | table_start table_identifier table_end EOS
    | table_start table_end newlines
    | table_start table_end EOS
    ;
  table_start
    : '[' '[' { @handler.start_(:array_of_tables) }
    | '[' { @handler.start_(:table) }
    ;
    ;
  table_end
    : ']' ']' { array = @handler.end_(:array_of_tables); @handler.set_context(array, is_array_of_tables: true) }
    | ']' { array = @handler.end_(:table); @handler.set_context(array) }
    ;
  table_identifier
    : table_identifier '.' table_identifier_component { @handler.push(val[2]) }
    | table_identifier '.' FLOAT { val[2].split('.').each { |k| @handler.push(k) } }
    | FLOAT {
      keys = val[0].split('.')
      @handler.start_(:table)
      keys.each { |key| @handler.push(key) }
    }
    | table_identifier_component { @handler.push(val[0]) }
    ;
  table_identifier_component
    : IDENTIFIER
    | STRING_BASIC { result = StringUtils.replace_escaped_chars(val[0]) }
    | STRING_LITERAL
    | INTEGER
    | NON_DEC_INTEGER
    | FLOAT_KEYWORD
    | BOOLEAN
    | DATETIME { result = val[0][0] }
    | LOCAL_TIME { result = val[0][0] }
    ;
  inline_table
    : inline_table_start inline_table_end
    | inline_table_start inline_continued inline_table_end
    ;
  inline_table_start
    : '{' { @handler.start_(:inline) }
    ;
  inline_table_end
    : '}' {
      array = @handler.end_(:inline)
      @handler.push_inline(array)
    }
    ;
  inline_continued
    : inline_assignment
    | inline_assignment inline_next
    ;
  inline_next
    : ',' inline_continued
    ;
  inline_assignment
    : inline_assignment_key '=' value {
      keys = @handler.end_(:inline_keys)
      @handler.push(keys)
    }
    ;
  inline_assignment_key
    : inline_assignment_key '.' assignment_key_component { 
      @handler.push(val[2]) 
    }
    | inline_assignment_key '.' FLOAT { val[2].split('.').each { |k| @handler.push(k) } }
    | FLOAT {
      keys = val[0].split('.')
      @handler.start_(:inline_keys)
      keys.each { |key| @handler.push(key) }
    }
    | assignment_key_component { 
      @handler.start_(:inline_keys) 
      @handler.push(val[0]) 
    }
    ;
  assignment
    : assignment_key '=' value EOS {
      keys = @handler.end_(:keys)
      value = keys.pop
      @handler.validate_value(value)
      @handler.push(value)
      @handler.assign(keys)
    }
    | assignment_key '=' value NEWLINE {
      keys = @handler.end_(:keys)
      value = keys.pop
      @handler.validate_value(value)
      @handler.push(value)
      @handler.assign(keys)
    }
    ;
  assignment_key
    : assignment_key '.' assignment_key_component { @handler.push(val[2]) }
    | assignment_key '.' FLOAT { val[2].split('.').each { |k| @handler.push(k) } }
    | FLOAT {
      keys = val[0].split('.')
      @handler.start_(:keys)
      keys.each { |key| @handler.push(key) }
    }
    | assignment_key_component { @handler.start_(:keys); @handler.push(val[0]) }
    ;
  assignment_key_component
    : IDENTIFIER
    | STRING_BASIC { result = StringUtils.replace_escaped_chars(val[0]) }
    | STRING_LITERAL
    | INTEGER
    | NON_DEC_INTEGER
    | FLOAT_KEYWORD
    | BOOLEAN
    | DATETIME { result = val[0][0] }
    | LOCAL_TIME { result = val[0][0] }
    ;
  array
    : start_array array_first_value array_values comma end_array
    | start_array array_first_value array_values end_array
    | start_array array_first_value comma end_array
    | start_array array_first_value end_array
    | start_array end_array
    ;
  array_first_value
    : newlines non_nil_value
    | non_nil_value
    ;
  array_values
    : array_values array_value
    | array_value
    ;
  array_value
    : comma newlines non_nil_value
    | comma non_nil_value
    ;
  start_array
    : '[' { @handler.start_(:array) }
    ;
  end_array
    : newlines ']' { array = @handler.end_(:array); @handler.push(array.compact) }
    | ']' { array = @handler.end_(:array); @handler.push(array.compact) }
    ;
  comma
    : newlines ','
    | ','
    ;
  newlines
    : newlines NEWLINE
    | NEWLINE
    ;
  value
    : scalar { @handler.push(val[0]) }
    | array
    | inline_table
    ;
  non_nil_value
    : non_nil_scalar { @handler.push(val[0]) }
    | array
    | inline_table
    ;
  scalar
    : string
    | literal
    ;
  non_nil_scalar
    : string
    | non_nil_literal
    ;
  literal
    | non_nil_literal
    ;
  non_nil_literal
    : FLOAT { result = val[0].to_f }
    | FLOAT_KEYWORD {
      v = val[0]
      result = if v.end_with?('nan')
                 Float::NAN
               else
                 (v[0] == '-' ? -1 : 1) * Float::INFINITY
               end
    }
    | INTEGER { result = val[0].to_i }
    | NON_DEC_INTEGER {
      base = case val[0][1]
             when 'x' then 16
             when 'o' then 8
             when 'b' then 2
             end
      result = val[0].to_i(base)
    }
    | BOOLEAN { result = val[0] == 'true' ? true : false }
    | DATETIME {
      _str, year, month, day, hour, min, sec, offset = val[0]
      result = if offset.nil?
                 if hour.nil?
                   LocalDate.new(year, month, day)
                 else
                   LocalDateTime.new(year, month, day, hour, min || 0, sec.to_f)
                 end
               else
                 # Patch for 24:00:00 which Ruby parses
                 if hour.to_i == 24 && min.to_i == 0 && sec.to_i == 0
                   hour = (hour.to_i + 1).to_s
                 end

                 time = Time.new(year, month, day, hour || 0, min || 0, sec.to_f, offset)
                 # Should be out of parser.y?
                 raise ArgumentError, "Invalid Offset Date-Time: #{year}-#{month}-#{day}T#{hour}:#{min}:#{sec}#{offset}" unless min.to_i == time.min && hour.to_i == time.hour && day.to_i == time.day && month.to_i == time.month && year.to_i == time.year
                 time
               end
    }
    | LOCAL_TIME { result = LocalTime.new(*val[0][1..-1]) }
    ;
  string
    : STRING_MULTI { result = StringUtils.replace_escaped_chars(StringUtils.multiline_replacements(val[0])) }
    | STRING_BASIC { result = StringUtils.replace_escaped_chars(val[0]) }
    | STRING_LITERAL_MULTI { result = StringUtils.strip_spaces(val[0]) }
    | STRING_LITERAL { result = val[0] }
    ;
