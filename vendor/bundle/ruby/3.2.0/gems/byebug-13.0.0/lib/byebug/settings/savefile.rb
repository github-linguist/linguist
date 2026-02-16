# frozen_string_literal: true

require_relative "../setting"

module Byebug
  #
  # Setting to customize the file where byebug's settings are saved.
  #
  class SavefileSetting < Setting
    DEFAULT = File.expand_path("#{ENV['HOME'] || '.'}/.byebug_save")

    def banner
      "File where settings are saved to. Default: ~/.byebug_save"
    end

    def to_s
      "The settings file is #{value}\n"
    end
  end
end
