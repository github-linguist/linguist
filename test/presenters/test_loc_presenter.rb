require_relative "../helper"

module Presenters
  class TestLoc < Minitest::Test
    def rugged_repository
      @rugged ||= Rugged::Repository.new(File.expand_path("../../../.git", __FILE__))
    end

    def master_oid
      'd40b4a33deba710e2f494db357c654fbe5d4b419'
    end

    def linguist_repo(oid = master_oid)
      Linguist::Repository.new(rugged_repository, oid)
    end
    
    def subject 
      @subject ||= Linguist::Presenters::Loc.new(linguist_repo, io)
    end

    def io
      @io ||= StringIO.new 
    end
  
    def test_includes_language
      subject.present
      assert_match(/Ruby:/, io.string)
    end
  end
end
