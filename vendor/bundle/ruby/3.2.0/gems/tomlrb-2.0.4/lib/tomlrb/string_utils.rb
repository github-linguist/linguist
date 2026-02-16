# frozen-string-literal: true

module Tomlrb
  class StringUtils
    SPECIAL_CHARS = {
      '\\t'  => "\t",
      '\\b'  => "\b",
      '\\f'  => "\f",
      '\\n'  => "\n",
      '\\r'  => "\r",
      '\\"'  => '"',
      '\\\\' => '\\'
    }.freeze

    def self.multiline_replacements(str)
      strip_spaces(str).gsub(/\\+\s*\n\s*/) do |matched|
        if matched.match(/\\+/)[0].length.odd?
          matched.gsub(/\\\s*\n\s*/, '')
        else
          matched
        end
      end
    end

    def self.replace_escaped_chars(str)
      str.gsub(/\\(u[\da-fA-F]{4}|U[\da-fA-F]{8}|.)/) do |m|
        if m.size == 2
          SPECIAL_CHARS[m] || (raise Tomlrb::ParseError.new "Escape sequence #{m} is reserved")
        else
          m[2..-1].to_i(16).chr(Encoding::UTF_8)
        end
      end
    end

    def self.strip_spaces(str)
      str[0] = '' if str[0] == "\n"
      str
    end
  end
end
