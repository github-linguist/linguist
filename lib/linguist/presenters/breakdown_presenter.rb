module Linguist
  # This class handles the output of each of the
  # command options
  module Presenters
    class Breakdown
      def initialize(repo, io = $stdout)
        @repo = repo
        @io = io
      end

      def present 
        @io.puts
        file_breakdown = @repo.breakdown_by_file
        file_breakdown.each do |lang, files|
          @io.puts "#{lang}:"
          files.each do |file|
            @io.puts file
          end
          @io.puts
        end
      end
    end
  end
end
