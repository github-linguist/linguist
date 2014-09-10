require 'linguist/samples'
require 'linguist/language'
require 'tempfile'
require 'yajl'
require 'test/unit'

class TestSamples < Test::Unit::TestCase
  include Linguist

  def test_up_to_date
    assert serialized = Samples::DATA
    assert latest = Samples.data

    # Just warn, it shouldn't scare people off by breaking the build.
    if serialized['md5'] != latest['md5']
      warn "Samples database is out of date. Run `bundle exec rake samples`."

      expected = Tempfile.new('expected.json')
      expected.write Yajl::Encoder.encode(serialized, :pretty => true)
      expected.close

      actual = Tempfile.new('actual.json')
      actual.write Yajl::Encoder.encode(latest, :pretty => true)
      actual.close

      expected.unlink
      actual.unlink
    end
  end

  def test_verify
    assert data = Samples::DATA

    assert_equal data['languages_total'], data['languages'].inject(0) { |n, (_, c)| n += c }
    assert_equal data['tokens_total'], data['language_tokens'].inject(0) { |n, (_, c)| n += c }
    assert_equal data['tokens_total'], data['tokens'].inject(0) { |n, (_, ts)| n += ts.inject(0) { |m, (_, c)| m += c } }
  end

  # Check that there aren't samples with extensions that aren't explicitly defined in languages.yml
  def test_parity
    extensions = Samples::DATA['extnames']
    languages_yml = File.expand_path("../../lib/linguist/languages.yml", __FILE__)
    languages = YAML.load_file(languages_yml)

    languages.each do |name, options|
      options['extensions'] ||= []

      if extnames = extensions[name]
        extnames.each do |extname|
          next if extname == '.script!'
          assert options['extensions'].include?(extname), "#{name} has a sample with extension (#{extname}) that isn't explicitly defined in languages.yml"
        end
      end
    end
  end

  # If a language extension isn't globally unique then make sure there are samples
  def test_presence
    Linguist::Language.all.each do |language|
      language.all_extensions.each do |extension|
        language_matches = Language.find_by_filename("foo#{extension}")

        # If there is more than one language match for a given extension
        # then check that there are examples for that language with the extension
        if language_matches.length > 1
          language_matches.each do |language|
            assert File.directory?("samples/#{language.name}"), "#{language.name} is missing a samples directory"
            assert Dir.glob("samples/#{language.name}/*#{extension}").any?, "#{language.name} is missing samples for extension #{extension}"
          end
        end
      end
    end
  end
end
