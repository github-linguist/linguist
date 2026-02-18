module Linguist
  class BasicInstrumenter
    attr_reader :detected_info

    def initialize
      @detected_info = {}
    end

    def instrument(name, payload = {})
      if name == "linguist.detected" && payload[:blob]
        @detected_info[payload[:blob].name] = {
          strategy: payload[:strategy].name.split("::").last,
          language: payload[:language]&.name
        }
      end
      yield if block_given?
    end
  end
end
