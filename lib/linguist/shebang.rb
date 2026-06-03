# frozen_string_literal: true

module Linguist
  class Shebang
    # Public: Use shebang to detect language of the blob.
    #
    # blob               - An object that quacks like a blob.
    # candidates         - A list of candidate languages.
    #
    # Examples
    #
    #   Shebang.call(FileBlob.new("path/to/file"))
    #
    # Returns an array of languages from the candidate list for which the
    # blob's shebang is valid. Returns an empty list if there is no shebang.
    # If the candidate list is empty, any language is a valid candidate.
    def self.call(blob, candidates)
      return [] if blob.symlink?

      languages = Language.find_by_interpreter interpreter(blob.data)
      candidates.any? ? candidates & languages : languages
    end

    # Public: Get the interpreter from the shebang
    #
    # Returns a String or nil
    def self.interpreter(data)
      # First line must start with #!
      return unless data.start_with?("#!")

      shebang = data[0, data.index($/) || data.length]

      s = StringScanner.new(shebang)

      # There was nothing after the #!
      return unless path = s.scan(/^#!\s*\S+/)

      # Keep going
      script = path.split('/').last

      # if /usr/bin/env type shebang then walk the string
      if script == 'env'
        s.scan(/\s+/)
        while s.scan(/((-[i0uCSv]*|--\S+)\s+)+/) || # skip over optional arguments e.g. -vS
              s.scan(/(\S+=\S+\s+)+/) # skip over variable arguments e.g. foo=bar
          # do nothing
        end
        script = s.scan(/\S+/)
      end

      # Interpreter was /usr/bin/env with no arguments
      return unless script

      # "python2.6" -> "python2"
      script.sub!(/(\.\d+)$/, '')

      # #! perl -> perl
      script.sub!(/^#!\s*/, '')

      # Check for multiline shebang hacks that call `exec`
      if script == 'sh' &&
        data.lines.first(5).any? { |l| l.match(/exec (\w+)[\s"']+\$0[\s"']+\$@/) }
        script = $1
      end

      # osascript can be called with an optional `-l <language>` argument, which may not be a language with an interpreter.
      # In this case, return and rely on the subsequent strategies to determine the language.
      if script == 'osascript'
        return if s.scan_until(/\-l\s?/)
      end

      File.basename(script)
    end
  end
end
