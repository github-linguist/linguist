module Linguist
  module Strategy
    # Detects man pages based on numeric file extensions with group suffixes.
    class Manpage

      # Public: RegExp for matching conventional manpage extensions
      #
      # This is the same expression as that used by `github/markup`
      MANPAGE_EXTS = /\.(?:[1-9](?![0-9])[a-z_0-9]*|0p|n|man|mdoc)(?:\.in)?$/i

      # Public: Use the file extension to match a possible man page,
      # only if no other candidates were previously identified.
      #
      # blob               - An object that quacks like a blob.
      # candidates         - A list of candidate languages.
      #
      # Examples
      #
      #   Manpage.call(FileBlob.new("path/to/file"))
      #
      # Returns:
      #   1. The list of candidates if it wasn't empty
      #   2. An array of ["Roff", "Roff Manpage"] if the file's
      #      extension matches a valid-looking man(1) section
      #   3. An empty Array for anything else
      #
      def self.call(blob, candidates = [])
        return candidates if candidates.any?

        if blob.name =~ MANPAGE_EXTS
          return [
            Language["Roff Manpage"],
            Language["Roff"],
            # Language["Text"] TODO: Uncomment once #4258 gets merged
          ];
        end

        []
      end
    end
  end
end
