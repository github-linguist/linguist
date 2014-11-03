module Linguist
  module Strategy
    class Filename
      def self.call(blob, _)
        name = blob.name.to_s

        # A bit of an elegant hack. If the file is executable but extensionless,
        # append a "magic" extension so it can be classified with other
        # languages that have shebang scripts.
        extension = FileBlob.new(name).extension
        if extension.empty? && blob.mode && (blob.mode.to_i(8) & 05) == 05
          name += ".script!"
        end

        # First try to find languages that match based on filename.
        Language.find_by_filename(name)
      end
    end
  end
end
