require_relative "./helper"
require "tempfile"

class TestSamples < Minitest::Test
  include Linguist

  def test_up_to_date
    assert serialized = Samples.cache
    assert latest = Samples.data

    # Just warn, it shouldn't scare people off by breaking the build.
    if serialized['sha256'] != latest['sha256']
      warn "Samples database is out of date. Run `bundle exec rake samples`."

      expected = Tempfile.new('expected.json')
      expected.write Yajl.dump(serialized, :pretty => true)
      expected.close

      actual = Tempfile.new('actual.json')
      actual.write Yajl.dump(latest, :pretty => true)
      actual.close

      expected.unlink
      actual.unlink
    end
  end

  def test_verify
    assert data = Samples.cache

    assert_equal data['languages_total'], data['languages'].inject(0) { |n, (_, c)| n += c }
    assert_equal data['tokens_total'], data['language_tokens'].inject(0) { |n, (_, c)| n += c }
    assert_equal data['tokens_total'], data['tokens'].inject(0) { |n, (_, ts)| n += ts.inject(0) { |m, (_, c)| m += c } }
    assert !data["interpreters"].empty?
  end

  def test_ext_or_shebang
    Samples.each do |sample|
      if sample[:extname].to_s.empty? && !sample[:filename]
        assert sample[:interpreter], "#{sample[:path]} should have a file extension or a shebang, maybe it belongs in filenames/ subdir"
      end
    end
  end

  def test_filename_listed
    Samples.each do |sample|
      if sample[:filename]
        listed_filenames = Language[sample[:language]].filenames
        assert_includes listed_filenames, sample[:filename], "#{sample[:path]} isn't listed as a filename for #{sample[:language]} in languages.yml"
      end
    end
  end

  # Some named files are deliberately classified as Text without a corresponding sample as including
  # the sample would affect the classifier leading to incorrect analysis and classification.
  # This test ensures samples aren't added for those specific cases.
  def test_no_text_samples
    no_text_samples = ["go.mod", "go.sum"]
    Samples.each do |sample|
      if sample[:language] == "Text"
        refute_includes no_text_samples, sample[:filename], "#{sample[:filename]} should NOT be added as a sample for #{sample[:language]}"
      end
    end
  end

  # Check that there aren't samples with extensions or interpreters that
  # aren't explicitly defined in languages.yml
  languages_yml = File.expand_path("../../lib/linguist/languages.yml", __FILE__)
  YAML.load_file(languages_yml).each do |name, options|
    define_method "test_samples_have_parity_with_languages_yml_for_#{name}" do
      options['extensions'] ||= []
      if extnames = Samples.cache['extnames'][name]
        extnames.each do |extname|
          assert options['extensions'].index { |x| x.downcase.end_with? extname.downcase }, "#{name} has a sample with extension (#{extname.downcase}) that isn't explicitly defined in languages.yml"
        end
      end

      options['interpreters'] ||= []
      if interpreters = Samples.cache['interpreters'][name]
        interpreters.each do |interpreter|
          assert options['interpreters'].include?(interpreter),
            "#{name} has a sample with an interpreter (#{interpreter}) that isn't explicitly defined in languages.yml"
        end
      end
    end
  end

  # If a language extension isn't globally unique then make sure there are samples
  Linguist::Language.all.each do |language|
    define_method "test_#{language.name}_has_samples" do
      language.extensions.each do |extension|
        language_matches = Language.find_by_extension(extension)

        # Check for samples if more than one language matches the given extension.
        if language_matches.length > 1
          language_matches.each do |match|
            generic = Strategy::Extension.generic? extension
            samples = generic ? "test/fixtures/Generic/#{extension.sub(/^\./, "")}/#{match.name}/*" : "samples/#{match.name}/*#{extension}"
            assert Dir.glob(samples, File::FNM_CASEFOLD).any?, "Missing samples in #{samples.inspect}. See https://github.com/github/linguist/blob/master/CONTRIBUTING.md"
          end
        end
      end

      language.filenames.each do |filename|
        # Check for samples if more than one language matches the given filename
        if Language.find_by_filename(filename).size > 1
          sample = "samples/#{language.name}/filenames/#{filename}"
          assert File.exist?(sample),
            "Missing sample in #{sample.inspect}. See https://github.com/github/linguist/blob/master/CONTRIBUTING.md"
        end
      end
    end
  end
end
