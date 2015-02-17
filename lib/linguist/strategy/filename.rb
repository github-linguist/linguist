module Linguist
  module Strategy
    # Detects language based on filename and/or extension
    class Filename
      def self.call(blob, _)
        name = blob.name.to_s

        # A bit of an elegant hack. If the file is executable but extensionless,
        # append a "magic" extension so it can be classified with other
        # languages that have shebang scripts.
        extensions = FileBlob.new(name).extensions
        if extensions.empty? && blob.mode && (blob.mode.to_i(8) & 05) == 05
          name += ".script!"
        end

        Language.find_by_filename(name)
      end
    end
  end
end
