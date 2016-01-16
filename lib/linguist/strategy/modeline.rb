module Linguist
  module Strategy
    class Modeline
      EMACS_MODELINE = /-\*-\s*(?:(?!mode)[\w-]+\s*:\s*(?:[\w+-]+)\s*;?\s*)*(?:mode\s*:)?\s*([\w+-]+)\s*(?:;\s*(?!mode)[\w-]+\s*:\s*[\w+-]+\s*)*;?\s*-\*-/i

      # First form vim modeline
      # [text]{white}{vi:|vim:|ex:}[white]{options}
      # ex: 'vim: syntax=perl'
      VIM_MODELINE_1 = /(?:vim|vi|ex):\s*(?:ft|filetype|syntax)=(\w+)\s?/i

      # Second form vim modeline (compatible with some versions of Vi)
      # [text]{white}{vi:|vim:|Vim:|ex:}[white]se[t] {options}:[text]
      # ex: 'vim set syntax=perl:'
      VIM_MODELINE_2 = /(?:vim|vi|Vim|ex):\s*se(?:t)?.*\s(?:ft|filetype|syntax)=(\w+)\s?.*:/i

      MODELINES = [EMACS_MODELINE, VIM_MODELINE_1, VIM_MODELINE_2]

      # Public: Detects language based on Vim and Emacs modelines
      #
      # blob               - An object that quacks like a blob.
      #
      # Examples
      #
      #   Modeline.call(FileBlob.new("path/to/file"))
      #
      # Returns an Array with one Language if the blob has a Vim or Emacs modeline
      # that matches a Language name or alias. Returns an empty array if no match.
      def self.call(blob, _ = nil)
        Array(Language.find_by_alias(modeline(blob.data)))
      end

      # Public: Get the modeline from the first n-lines of the file
      #
      # Returns a String or nil
      def self.modeline(data)
        match = MODELINES.map { |regex| data.match(regex) }.reject(&:nil?).first
        match[1] if match
      end
    end
  end
end
