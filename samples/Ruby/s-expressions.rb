class SExpr
  def initialize(str)
    @original = str
    @data = parse_sexpr(str)
  end
  attr_reader :data, :original

  def to_sexpr
    @data.to_sexpr
  end

  private

  def parse_sexpr(str)
    state = :token_start
    tokens = []
    word = ""
    str.each_char do |char|
      case state

      when :token_start
        case char
        when "("
          tokens << :lbr
        when ")"
          tokens << :rbr
        when /\s/
          # do nothing, just consume the whitespace
        when  '"'
          state = :read_quoted_string
          word = ""
        else
          state = :read_string_or_number
          word = char
        end

      when :read_quoted_string
        case char
        when '"'
          tokens << word
          state = :token_start
        else
          word << char
        end

      when :read_string_or_number
        case char
        when /\s/
          tokens << symbol_or_number(word)
          state = :token_start
        when ')'
          tokens << symbol_or_number(word)
          tokens << :rbr
          state = :token_start
        else
          word << char
        end
      end
    end

    sexpr_tokens_to_array(tokens)
  end

  def symbol_or_number(word)
    begin
      Integer(word)
    rescue ArgumentError
      begin
        Float(word)
      rescue ArgumentError
        word.to_sym
      end
    end
  end

  def sexpr_tokens_to_array(tokens, idx = 0)
    result = []
    while idx < tokens.length
      case tokens[idx]
      when :lbr
        tmp, idx = sexpr_tokens_to_array(tokens, idx + 1)
        result << tmp
      when :rbr
        return [result, idx]
      else
        result << tokens[idx]
      end
      idx += 1
    end
    result[0]
  end
end

class Object
  def to_sexpr
    self
  end
end

class String
  def to_sexpr
    self.match(/[\s()]/) ? self.inspect : self
  end
end

class Symbol
  alias :to_sexpr :to_s
end

class Array
  def to_sexpr
    "(%s)" % inject([]) {|a, elem| a << elem.to_sexpr}.join(" ")
  end
end


sexpr = SExpr.new <<END
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
END

puts "original sexpr:\n#{sexpr.original}"
puts "\nruby data structure:\n#{sexpr.data.inspect}"
puts "\nand back to S-Expr:\n#{sexpr.to_sexpr}"
