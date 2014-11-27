module Linguist
  # Check if there's a shebang line and use that as authoritative
  class Shebang
    def self.call(blob, _)
      Language.find_by_interpreter(new(blob.data).interpreter)
    end

    attr_reader :data

    def initialize(data)
      @data = data
    end

    def interpreter
      lines = data.lines.to_a

      if lines.any? && (match = lines[0].match(/(.+)\n?/)) && (bang = match[0]) =~ /^#!/
        bang.sub!(/^#! /, '#!')
        tokens = bang.split(' ')
        pieces = tokens.first.split('/')

        if pieces.size > 1
          script = pieces.last
        else
          script = pieces.first.sub('#!', '')
        end

        script = script == 'env' ? tokens[1] : script

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
      else
        nil
      end

    end
  end
end
