require 'linguist/blob_helper'
require 'linguist/generated'
require 'linguist/grammars'
require 'linguist/heuristics'
require 'linguist/language'
require 'linguist/repository'
require 'linguist/samples'
require 'linguist/shebang'
require 'linguist/version'
require 'linguist/strategy/manpage'
require 'linguist/strategy/xml'

class << Linguist
  # Public: Detects the Language of the blob.
  #
  # blob - an object that includes the Linguist `BlobHelper` interface;
  #       see Linguist::LazyBlob and Linguist::FileBlob for examples
  #
  # Returns Language or nil.
  def detect(blob, allow_empty: false)
    # Bail early if the blob is binary or empty.
    return nil if blob.likely_binary? || blob.binary? || (!allow_empty && blob.empty?)

    Linguist.instrument("linguist.detection", :blob => blob) do
      # Call each strategy until one candidate is returned.
      languages = []
      returning_strategy = nil

      STRATEGIES.each do |strategy|
        returning_strategy = strategy
        candidates = Linguist.instrument("linguist.strategy", :blob => blob, :strategy => strategy, :candidates => languages) do
          strategy.call(blob, languages)
        end
        if candidates.size == 1
          languages = candidates
          break
        elsif candidates.size > 1
          # More than one candidate was found, pass them to the next strategy.
          languages = candidates
        else
          # No candidates, try the next strategy
        end
      end

      Linguist.instrument("linguist.detected", :blob => blob, :strategy => returning_strategy, :language => languages.first)

      languages.first
    end
  end

  # Internal: The strategies used to detect the language of a file.
  #
  # A strategy is an object that has a `.call` method that takes two arguments:
  #
  #   blob - An object that quacks like a blob.
  #   languages - An Array of candidate Language objects that were returned by the
  #               previous strategy.
  #
  # A strategy should return an Array of Language candidates.
  #
  # Strategies are called in turn until a single Language is returned.
  STRATEGIES = [
    Linguist::Strategy::Modeline,
    Linguist::Strategy::Filename,
    Linguist::Shebang,
    Linguist::Strategy::Extension,
    Linguist::Strategy::XML,
    Linguist::Strategy::Manpage,
    Linguist::Heuristics,
    Linguist::Classifier
  ]

  # Public: Set an instrumenter.
  #
  #     class CustomInstrumenter
  #       def instrument(name, payload = {})
  #         warn "Instrumenting #{name}: #{payload[:blob]}"
  #       end
  #     end
  #
  #     Linguist.instrumenter = CustomInstrumenter.new
  #
  # The instrumenter must conform to the `ActiveSupport::Notifications`
  # interface, which defines `#instrument` and accepts:
  #
  # name    - the String name of the event (e.g. "linguist.detected")
  # payload - a Hash of the exception context.
  attr_accessor :instrumenter

  # Internal: Perform instrumentation on a block
  #
  #     Linguist.instrument("linguist.dosomething", :blob => blob) do
  #       # logic to instrument here.
  #     end
  #
  def instrument(*args, &bk)
    if instrumenter
      instrumenter.instrument(*args, &bk)
    elsif block_given?
      yield
    end
  end

end
