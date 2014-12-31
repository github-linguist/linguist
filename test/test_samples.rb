require_relative "./helper"
require "tempfile"

class TestSamples < Minitest::Test
  include Linguist

  def test_up_to_date
    assert serialized = Samples.cache
    assert latest = Samples.data

    # Just warn, it shouldn't scare people off by breaking the build.
    if serialized['md5'] != latest['md5']
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

  # Check that there aren't samples with extensions or interpreters that
  # aren't explicitly defined in languages.yml
  languages_yml = File.expand_path("../../lib/linguist/languages.yml", __FILE__)
  YAML.load_file(languages_yml).each do |name, options|
    define_method "test_samples_have_parity_with_languages_yml_for_#{name}" do
      options['extensions'] ||= []
      if extnames = Samples.cache['extnames'][name]
        extnames.each do |extname|
          next if extname == '.script!'
          assert options['extensions'].include?(extname), "#{name} has a sample with extension (#{extname}) that isn't explicitly defined in languages.yml"
        end
      end

      options['interpreters'] ||= []
      if interpreters = Samples.cache['interpreters'][name]
        interpreters.each do |interpreter|
          # next if extname == '.script!'
          assert options['interpreters'].include?(interpreter), "#{name} has a sample with an interpreter (#{interpreter}) that isn't explicitly defined in languages.yml"
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
            samples = "samples/#{match.name}/*#{extension}"
            assert Dir.glob(samples).any?, "Missing samples in #{samples.inspect}. See https://github.com/github/linguist/blob/master/CONTRIBUTING.md"
          end
        end
      end

      language.filenames.each do |filename|
        # Check for samples if more than one language matches the given filename
        if Language.find_by_filename(filename).size > 1
          sample = "samples/#{language.name}/filenames/#{filename}"
          assert File.exists?(sample),
            "Missing sample in #{sample.inspect}. See https://github.com/github/linguist/blob/master/CONTRIBUTING.md"
        end
      end
    end
  end
end
