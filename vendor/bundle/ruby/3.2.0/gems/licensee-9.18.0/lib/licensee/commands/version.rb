# frozen_string_literal: true

class LicenseeCLI < Thor
  desc 'version', 'Return the Licensee version'
  def version
    say Licensee::VERSION
  end
end
