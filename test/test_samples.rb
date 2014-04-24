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

  Samples.each do |sample|
    define_method "test_#{sample[:path]}_not_binary" do
      blob = blob(sample[:path])
      assert !blob.binary_mime_type? && !blob.binary?, "#{sample[:path]} is a binary file"
    end
  end

  # FIXME: extract all of these to `test/helper.rb`
  require 'linguist/file_blob'
  require 'linguist/samples'
  def samples_path
    File.expand_path("../../samples", __FILE__)
  end

  def blob(name)
    name = File.join(samples_path, name) unless name =~ /^\//
    FileBlob.new(name, samples_path)
  end

end
