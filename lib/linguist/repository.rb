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

      @stats     = nil
      @languages = Hash.new { 0 }
      @sizes     = Hash.new { 0 }
    end

    def [](path)
      @paths[path]
    end

    def language
      stats[:primary]
    end

    def stats
      @stats ||= compute_stats
    end

    def compute_stats
      @paths.each do |path, blob|
        next if path =~ IgnoreRegexp

        language = blob.language

        if language.common?
          @languages[language.name] += 1
          @sizes[language.name]     += blob.size
        end
      end

      total_size = @sizes.inject(0) { |s,(k,v)| s + v }

      results = {
        :total_size => total_size
      }

      @sizes.each do |language, size|
        results[language] = size
      end

      primary = @sizes.max_by { |(language, size)|
        size
      }

      if primary
        results[:primary] = primary[0]
      end

      results
    end
  end
end
