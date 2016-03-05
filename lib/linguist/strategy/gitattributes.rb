require 'rugged'

module Linguist
  module Strategy
    class Gitattributes
      # Public: Detects language based on .gitattributes
      def self.call(blob, _ = nil)
        repo = Rugged::Repository.discover(File.dirname(blob.path))
        attr = repo.attributes(blob.path)
        Language.find_by_alias attr['linguist-language']
      end
    end
  end
end
