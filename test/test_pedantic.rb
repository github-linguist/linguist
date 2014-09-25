require 'test/unit'

class TestPedantic < Test::Unit::TestCase
  Lib = File.expand_path("../../lib/linguist", __FILE__)

  def file(name)
    File.read(File.join(Lib, name))
  end

  def test_language_names_are_sorted
    languages = []
    file("languages.yml").lines.each do |line|
      if line =~ /^(\w+):$/
        languages << $1
      end
    end
    assert_sorted languages
  end

  def test_extensions_are_sorted
    extensions = nil
    file("languages.yml").lines.each do |line|
      if line =~ /^  extensions:$/
        extensions = []
      elsif extensions && line =~ /^  - \.([\w-]+)( *#.*)?$/
        extensions << $1
      else
        assert_sorted extensions[1..-1] if extensions
        extensions = nil
      end
    end
  end

  def test_filenames_are_sorted
    filenames = nil
    file("languages.yml").lines.each do |line|
      if line =~ /^  filenames:$/
        filenames = []
      elsif filenames && line =~ /^  - \.(\w+)$/
        filenames << $1
      else
        assert_sorted filenames if filenames
        filenames = nil
      end
    end
  end

  def assert_sorted(list)
    previous = nil
    list.each do |item|
      if previous && previous > item
        flunk "#{previous} should come after #{item}"
      end
      previous = item
    end
  end
end
