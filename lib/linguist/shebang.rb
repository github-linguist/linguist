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
      lines = data.lines
      return unless match = /^#! ?(.*)$/.match(lines.first)

      tokens = match[0].split(' ')
      script = tokens.first.split('/').last

      script = tokens[1] if script == 'env'

      # If script has an invalid shebang, we might get here
      return unless script

      # "python2.6" -> "python2"
      script.sub! $1, '' if script =~ /(\.\d+)$/

      # Check for multiline shebang hacks that call `exec`
      if script == 'sh' &&
        lines.first(5).any? { |l| l.match(/exec (\w+).+\$0.+\$@/) }
        script = $1
      end

      File.basename(script)
    end
  end
end
