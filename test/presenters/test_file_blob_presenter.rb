require_relative "../helper"

module Presenters
  class TestLoc < Minitest::Test

    def file
      'lib/linguist.rb'
    end
    
    def subject 
      @subject ||= Linguist::Presenters::FileBlob.new(file, io)
    end

    def io
      @io ||= StringIO.new 
    end
  
    def test_includes_language
      subject.present
      assert_match(/language:\s*Ruby/, io.string)
    end

    def test_includes_filename
      subject.present
      assert_match(/linguist\.rb/, io.string)
    end

    def test_includes_mime_type
      subject.present
      assert_match(/application\/x-ruby/, io.string)
    end
  end
end
