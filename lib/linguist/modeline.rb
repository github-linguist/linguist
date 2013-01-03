require 'strscan'

module Linguist
  # Generic programming language modeline guess mechanism.
  #
  # Guesses at a language based on any found modeline
  class Modeline
    # limit to 100k
    BYTE_LIMIT = 100_000

    # Start state on token, ignore anything till the next newline
    COMMENT_DELIMS = [
      '//', # C++
      '/*', # c
      '--', # lua
      '#',  # Ruby, python, shell
    ]

    START_COMMENT =  Regexp.compile(COMMENT_DELIMS.map { |c|
      "\s*#{Regexp.escape(c)}"
    }.join("|"))

    def self.extract_mode(data)
      return nil if data.nil?

      s = StringScanner.new(data)
      until s.eos?
        break if s.pos >= BYTE_LIMIT

        need_eol = true
        if token = s.scan(START_COMMENT)
          if token = s.scan(/\s+(vi:|vim:|ex:)\s*/)
            need_eol = false
            s.scan(/se[t]?\s+/)
            s.scan_until(/\n|\Z/).split(/:|\s/).each do |opt|
              if opt =~ /(\w+)=(\w+)/
                key = $1
                val = $2
                if key == "ft" || key == "filetype"
                  return val
                end
              end
            end
          elsif token = s.scan(/\s+-\*-\s+mode:\s+/)
            # single line emacs mode...
            # in theory, emacs can also specify this
            # using a Local Variables: line followed
            # by a mode: line somewhere
            # and ending with an End: tag line
            # but let's not bother with a parser for that yet
            return s.scan_until(/;|\s|-/).chop
          end
        end
        if need_eol
          s.scan_until(/\n|\Z/)
        end
      end
      return nil
    end
  end
end
