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
    # Returns an Array of Languages from the candidate list for which the
    # blob's shebang is valid. Returns an empty list if there is not shebang.
    # If the candidate list is empty, any language is a valid candidate.
    def self.call(blob, candidates)
      return [] if blob.symlink?

      languages = Language.find_by_interpreter interpreter(blob.data)
      if candidates.any? then candidates & languages else languages end
    end

    # Public: Get the interpreter from the shebang
    #
    # Returns a String or nil
    def self.interpreter(data)
      shebang = data.lines.first

      # First line must start with #!
      return unless shebang && shebang.start_with?("#!")

      s = StringScanner.new(shebang)

      # There was nothing after the #!
      return unless path = s.scan(/^#!\s*\S+/)

      # Keep going
      script = path.split('/').last

      # if /usr/bin/env type shebang then walk the string
      if script == 'env'
        s.scan(/\s+/)
        s.scan(/.*=[^\s]+\s+/) # skip over variable arguments e.g. foo=bar
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
        data.lines.first(5).any? { |l| l.match(/exec (\w+).+\$0.+\$@/) }
        script = $1
      end

      File.basename(script)
    end
  end
end
