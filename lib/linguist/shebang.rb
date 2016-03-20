module Linguist
  class Shebang
    # Public: Use shebang to detect language of the blob.
    #
    # blob               - An object that quacks like a blob.
    #
    # Examples
    #
    #   Shebang.call(FileBlob.new("path/to/file"))
    #
    # Returns an Array with one Language if the blob has a shebang with a valid
    # interpreter, or empty if there is no shebang.
    def self.call(blob, _ = nil)
      Language.find_by_interpreter interpreter(blob.data)
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
