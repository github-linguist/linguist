require 'linguist/tokenizer'
require 'set'

module Linguist
  # Language content classifier.
  class Classifier
    # Maximum number of bytes to consider for classification.
    # This is only used at evaluation time. During training, full content of
    # samples is used.
    CLASSIFIER_CONSIDER_BYTES = 50 * 1024

    # Public: Use the classifier to detect language of the blob.
    #
    # blob               - An object that quacks like a blob.
    # possible_languages - Array of Language objects
    #
    # Examples
    #
    #   Classifier.call(FileBlob.new("path/to/file"), [
    #     Language["Ruby"], Language["Python"]
    #   ])
    #
    # Returns an Array of Language objects, most probable first.
    def self.call(blob, possible_languages)
      language_names = possible_languages.map(&:name)
      classify(Samples.cache, blob.data[0...CLASSIFIER_CONSIDER_BYTES], language_names).map do |name, _|
        Language[name] # Return the actual Language objects
      end
    end

    # Public: Train classifier that data is a certain language.
    #
    # db       - Hash classifier database object
    # language - String language of data
    # data     - String contents of file or array of tokens.
    #
    # Examples
    #
    #   Classifier.train!(db, 'Ruby', "def hello; end")
    #
    # Returns nil.
    #
    # Set LINGUIST_DEBUG=1, =2 or =3 to print internal statistics.
    def self.train!(db, language, data)
      tokens = data
      tokens = Tokenizer.tokenize(tokens) if tokens.is_a?(String)

      db['vocabulary'] ||= {}
      # Set hash to autoincremented index value
      if db['vocabulary'].default_proc.nil?
        db['vocabulary'].default_proc = proc do |hash, key|
          hash[key] = hash.length
        end
      end

      db['samples'] ||= {}
      db['samples'][language] ||= []

      termfreq = to_vocabulary_index_termfreq(db['vocabulary'], tokens)
      db['samples'][language] << termfreq

      nil
    end

    # Public: Finalize training.
    #
    # db - Hash classifier database object
    #
    # Examples:
    #   Classifier.finalize_train!(db)
    #
    # Returns nil.
    #
    # This method must be called after the last #train! call.
    def self.finalize_train!(db)
      db['vocabulary'] ||= {}

      # Unset hash autoincrement
      db['vocabulary'].default_proc = nil

      db['samples'] ||= []
      filter_vocab_by_freq! db, MIN_DOCUMENT_FREQUENCY
      sort_vocab! db
      db['icf'] = inverse_class_freqs db
      normalize_samples! db
      db['centroids'] = get_centroids db
      db.delete 'samples'
      nil
    end

    # Public: Guess language of data.
    #
    # db        - Hash of classifier tokens database.
    # data      - Array of tokens or String data to analyze.
    # languages - Array of language name Strings to restrict to.
    #
    # Examples
    #
    #   Classifier.classify(db, "def hello; end")
    #   # => [ 'Ruby', 0.90], ['Python', 0.2], ... ]
    #
    # Returns sorted Array of result pairs. Each pair contains the
    # String language name and a Float score between 0.0 and 1.0.
    def self.classify(db, tokens, languages = nil)
      languages ||= db['centroids'].keys
      new(db).classify(tokens, languages)
    end

    # Internal: Initialize a Classifier.
    def initialize(db = {})
      @vocabulary = db['vocabulary']
      @centroids  = db['centroids']
      @icf = db['icf']
    end

    # Internal: Guess language of data
    #
    # data      - Array of tokens or String data to analyze.
    # languages - Array of language name Strings to restrict to.
    #
    # Returns sorted Array of result pairs. Each pair contains the
    # String language name and a Float score between 0.0 and 1.0.
    def classify(tokens, languages)
      return [] if tokens.nil? || languages.empty?
      tokens = Tokenizer.tokenize(tokens) if tokens.is_a?(String)

      debug_dump_tokens(tokens) if verbosity >= 3

      vec = Classifier.to_vocabulary_index_termfreq_gaps(@vocabulary, tokens)
      vec.each do |idx, freq|
        tf = 1.0 + Math.log(freq)
        vec[idx] = tf * @icf[idx]
      end
      return [] if vec.empty?
      Classifier.l2_normalize!(vec)

      scores = {}
      languages.each do |language|
        centroid = @centroids[language]
        score = Classifier.similarity(vec, centroid)
        if score > 0.0
          scores[language] = score
        end
      end
      scores = scores.sort_by { |x| -x[1] }
      debug_dump_all_tokens(tokens, scores) if verbosity >= 2
      debug_dump_scores(scores) if verbosity >= 1
      scores
    end

    private
      MIN_DOCUMENT_FREQUENCY = 2

      def verbosity
        @verbosity ||= (ENV['LINGUIST_DEBUG'] || 0).to_i
      end

      def debug_dump_scores(scores)
        headers = ["Language", "Score"]
        rows = scores.map { |l, s| [l, "%.3f" % s] }
        dump_table(headers, rows)
      end

      def debug_dump_tokens(tokens)
        counts = Hash.new(0)
        tokens.each do |tok|
          idx = @vocabulary[tok]
          if not idx.nil?
            counts[tok] += 1
          end
        end

        norm = Classifier.l2_norm(counts)
        rows = counts.map do |tok, tf|
          idx = @vocabulary[tok]
          log_tf = 1.0 + Math.log(tf)
          with_icf = log_tf * @icf[idx]
          normalized = with_icf / norm
          row = [tok, tf, "%.3f" % log_tf, "%.3f" % with_icf, "%.3f" % normalized]
          [normalized, row]
        end

        headers = ["Token", "TF", "log", "*ICF", "L2"]
        rows = rows.sort_by { |x| -x[0] }.map { |_, row| row }
        dump_table(headers, rows)
      end

      # Internal: show a table of probabilities for each <token,language> pair.
      #
      # The number in each table entry is the number of "points" that each
      # token contributes toward the belief that the file under test is a
      # particular language.  Points are additive.
      def debug_dump_all_tokens(tokens, scores)
        languages = scores.map { |l, _| l }

        counts = Hash.new(0)
        tokens.each do |tok|
          idx = @vocabulary[tok]
          if not idx.nil?
            counts[tok] += 1
          end
        end

        data = {}
        norm = Classifier.l2_norm(counts)
        languages.each do |language|
          data[language] = {}
          counts.each do |tok, tf|
            idx = @vocabulary[tok]
            log_tf = 1.0 + Math.log(tf)
            with_icf = log_tf * @icf[idx]
            normalized = with_icf / norm
            data[language][tok] = normalized * @centroids[language][idx].to_f
          end
        end

        norm = Classifier.l2_norm(counts)
        rows = counts.map do |tok, tf|
          idx = @vocabulary[tok]
          log_tf = 1.0 + Math.log(tf)
          with_icf = log_tf * @icf[idx]
          normalized = with_icf / norm
          scores = languages.map do |l, _|
            [l, data[l][tok].to_f]
          end
          max_score = scores.to_h.values.max
          row = [tok] + scores.map do |l, s|
            if s == max_score
              "%.4f*" % s
            elsif s > 0.0
              "%.4f" % s
            else
              "-"
            end
          end
          [normalized, row]
        end
        headers = ["Token"] + (0..languages.length-1).map { |lidx| "[#{lidx}]" }
        rows = rows.sort_by { |x| -x[0] }.map { |_, row| row }
        legend = languages.each_with_index.map { |l, lidx| "[#{lidx}] = #{l}" }
        dump_table(headers, rows, legend)
      end

      def dump_table(header, rows, legend = nil)
        n_cols = header.length
        rows = rows.map { |r| r.map { |c| c.to_s } }
        col_widths = (0..n_cols - 1).map do |j|
          ([header[j].length] + rows.map { |row| row[j].length }).max
        end
        sep_line = "| #{(0..n_cols-1).map { |j| "-" * col_widths[j] }.join(" | ")} |"
        content_width = sep_line.length - 4
        top_line = "| #{"-" * content_width} |"

        format_row = Proc.new do |row|
          cells = row.zip(col_widths).map do |cell, width|
            "%-#{width}s" % cell
          end
          "| %s |" % cells.join(" | ")
        end

        puts top_line
        puts format_row.call(header)
        puts sep_line
        rows.each do |row|
          puts format_row.call(row)
        end
        puts top_line
        if legend
          legend.each do |line|
            puts "| %-#{content_width}s |" % line
          end
          puts top_line
        end
      end

      def self.to_vocabulary_index_termfreq(vocab, tokens)
        counts = Hash.new(0)
        tokens.each do |key|
          idx = vocab[key]
          counts[idx] += 1
        end
        counts
      end

      def self.to_vocabulary_index_termfreq_gaps(vocab, tokens)
        counts = Hash.new(0)
        tokens.each do |key|
          if vocab.key? key
            idx = vocab[key]
            counts[idx] += 1
          end
        end
        counts
      end

      def self.l2_norm(vec)
        norm = vec.values.inject(0.0) { |sum, x| sum + x**2 }
        Math.sqrt(norm)
      end

      def self.l2_normalize!(vec)
        norm = l2_norm(vec)
        vec.transform_values! do |value|
          value.to_f / norm
        end
        nil
      end

      def self.similarity(a, b)
        sum = 0.0
        a.each_key do |idx|
          if b.key? idx
            sum += a[idx] * b[idx]
          end
        end
        sum
      end

    # Filter vocabulary by minimum document frequency.
    def self.filter_vocab_by_freq!(db, min_freq)
      vocabulary = db['vocabulary']

      # Get document frequencies
      docfreq = Array.new(vocabulary.size, 0)
      db['samples'].each_value do |samples|
        samples.each do |sample|
          sample.each_key do |idx|
            docfreq[idx] += 1
          end
        end
      end

      vocabulary.select! do |_, idx|
        docfreq[idx] >= min_freq
      end

      nil
    end

    # Sort vocabulary lexicographically.
    def self.sort_vocab!(db)
      new_indices = Hash.new { |h,k| h[k] = h.length }
      db['vocabulary'].sort_by { |x| x[0] }.each do |term, idx|
        db['vocabulary'][term] = new_indices[idx]
      end
      new_indices.default_proc = nil

      db['samples'].transform_values! do |samples|
        samples.map do |sample|
          new_sample = {}
          sample.each do |idx, freq|
            new_idx = new_indices[idx]
            if not new_idx.nil?
              new_sample[new_idx] = freq
            end
          end
          new_sample
        end
      end
    end

    # Compute inverse class frequency (ICF) for every term.
    def self.inverse_class_freqs(db)
      icf = Array.new(db['vocabulary'].size, 0)
      db['samples'].each_value do |samples|
        terms = Set.new
        samples.each do |sample|
          terms |= sample.keys
        end
        terms.each do |idx|
          icf[idx] += 1
        end
      end
      icf.map! do |val|
        Math.log(db['samples'].size.to_f / val.to_f) + 1
      end
      icf
    end

    def self.normalize_samples!(db)
      icf = db['icf']
      db['samples'].each_value do |samples|
        samples.each do |sample|
          sample.each do |idx, freq|
            tf = 1.0 + Math.log(freq)
            sample[idx] = tf * icf[idx]
          end
          l2_normalize! sample
        end
      end
    end

    def self.get_centroids(db)
      centroids = {}
      db['samples'].each do |language, samples|
        centroid = Hash.new(0.0)
        samples.each do |sample|
          sample.each do |idx, val|
            centroid[idx] += val
          end
        end
        centroid.each_key do |idx|
          centroid[idx] = centroid[idx] / samples.length
        end
        l2_normalize! centroid
        centroids[language] = centroid
      end
      centroids
    end

  end
end
