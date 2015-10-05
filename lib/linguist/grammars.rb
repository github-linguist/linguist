# Note: This file is included in the github-linguist-grammars gem, not the
# github-linguist gem.

module Linguist
  module Grammars
    # Get the path to the directory containing the language grammar JSON files.
    #
    # Returns a String.
    def self.path
      File.expand_path("../../../grammars", __FILE__)
    end
  end
end
