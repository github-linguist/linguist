module Linguist
  class Repository
    IgnorePaths = %w(
      yui
      tiny_mce
      ckeditor
      redbox
      active_scaffold
      rico_corner
      code_highlighter
      vendor
      bundle
      node_modules
      cache
      assets
      bookends
      dojo
      extjs
      sencha
      mochikit
      prototype(.*)\.js
      mootools\.js
      jquery([^.]*)(\.min)?\.js
      jquery\-\d\.\d\.\d(\.min)?\.js
      effects\.js
      controls\.js
      dragdrop\.js
      fabfile\.py
      less([^.]*)(\.min)?\.js
      less\-\d+\.\d+\.\d+(\.min)?\.js
    )

    IgnoreRegexp = Regexp.new(IgnorePaths.join('|'))

    def initialize(paths)
      @paths = paths

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
      @paths.each do |path, blob|
        next if path =~ IgnoreRegexp

        language = blob.language

        if language.common?
          @sizes[language] += blob.size
        end
      end

      @size = @sizes.inject(0) { |s,(k,v)| s + v }

      if primary = @sizes.max_by { |(_, size)| size }
        @language = primary[0]
      end

      nil
    end
  end
end
