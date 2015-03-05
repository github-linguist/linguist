require_relative "./helper"

class TestInstrumentation < Minitest::Test
  include Linguist

  class LocalInstrumenter
    Event = Struct.new(:name, :args)

    attr_reader :events

    def initialize
      @events = []
    end

    def instrument(name, *args)
      @events << Event.new(name, args)
      yield if block_given?
    end
  end

  def setup
    Linguist.instrumenter = LocalInstrumenter.new
  end

  def teardown
    Linguist.instrumenter = nil
  end

  def test_detection_instrumentation_with_binary_blob
    binary_blob = fixture_blob("Binary/octocat.ai")
    Language.detect(binary_blob)

    # Shouldn't instrument this (as it's binary)
    assert_equal 0, Linguist.instrumenter.events.size
  end

  def test_modeline_instrumentation
    blob = fixture_blob("Data/Modelines/ruby")
    Language.detect(blob)

    assert_equal 3, Linguist.instrumenter.events.size
    assert_equal "linguist.detected", Linguist.instrumenter.events.last.name
  end
end
