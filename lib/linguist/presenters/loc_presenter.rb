module Linguist
  # This class handles the output of each of the
  # command options
  module Presenters
    class Loc
      def initialize(repo, io = $stdout)
        @repo = repo
        @io = io
      end

      def present
        @repo.languages.each do |lang, lang_loc| 
          @io.puts "#{lang}: #{lang_loc}"
        end
      end
    end
  end
end
