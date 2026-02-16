# frozen_string_literal: true

module Licensed
  class Report < Hash
    attr_reader :name
    attr_reader :target
    def initialize(name:, target:)
      super()
      @name = name
      @target = target
    end

    def reports
      @reports ||= []
    end

    def errors
      @errors ||= []
    end

    def warnings
      @warnings ||= []
    end

    def all_reports
      result = []
      result << self
      result.push(*reports.flat_map(&:all_reports))
    end

    # Returns the data from the report as a hash
    def to_h
      # add name, errors and warnings if they have real data
      output = {}
      output["name"] = name unless name.to_s.empty?
      output["errors"] = errors.dup if errors.any?
      output["warnings"] = warnings.dup if warnings.any?

      # merge the hash data from the report.  command-specified data always
      # overwrites local data
      output.merge(super)
    end
  end
end
