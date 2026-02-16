# frozen_string_literal: true

class LicenseeCLI < Thor
  desc 'license-path [PATH]', "Returns the path to the given project's license file"
  def license_path(path)
    project = Licensee.project(path)

    exit 1 unless project.license_file

    if remote?
      say project.license_file.path
    else
      say File.expand_path(project.license_file.path, path)
    end
  end
end
