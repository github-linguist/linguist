module Linguist
  class Repository
    def initialize(paths)
      @paths = paths

      @computed_stats = false
      @language = @size = nil
      @sizes = Hash.new { 0 }
    end

    def [](path)
      @paths[path]
    end

    def language
      compute_stats
      @language
    end

    def languages
      compute_stats
      @sizes
    end

    def size
      compute_stats
      @size
    end

    def compute_stats
      return if @computed_stats

      @paths.each do |path, blob|
        next if blob.vendored? || blob.generated?

        language = blob.language

        if language.common?
          @sizes[language] += blob.size
        end
      end

      @size = @sizes.inject(0) { |s,(k,v)| s + v }

      if primary = @sizes.max_by { |(_, size)| size }
        @language = primary[0]
      end

      @computed_stats = true
    end
  end
end
