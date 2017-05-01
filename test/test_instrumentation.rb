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
    Linguist.detect(binary_blob)

    # Shouldn't instrument this (as it's binary)
    assert_equal 0, Linguist.instrumenter.events.size
  end

  def test_modeline_instrumentation
    blob = fixture_blob("Data/Modelines/ruby")
    Linguist.detect(blob)

    detect_event = Linguist.instrumenter.events.last
    detect_event_payload = detect_event[:args].first

    assert_equal 3, Linguist.instrumenter.events.size
    assert_equal "linguist.detected", detect_event.name
    assert_equal Language['Ruby'], detect_event_payload[:language]
    assert_equal blob, detect_event_payload[:blob]
    assert_equal Linguist::Strategy::Modeline, detect_event_payload[:strategy]
  end
end
