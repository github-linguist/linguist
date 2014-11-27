module Linguist
  # Check if there's a shebang line and use that as authoritative
  class Shebang
    def self.call(blob, _)
      Language.find_by_interpreter interpreter(blob.data)
    end

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
        lines[0...5].any? { |l| l.match(/exec (\w+).+\$0.+\$@/) }
        script = $1
      end

      File.basename(script)
    end
  end
end
