# frozen_string_literal: true

module Licensee
  module ProjectFiles
    autoload :ProjectFile, 'licensee/project_files/project_file'
    autoload :LicenseFile, 'licensee/project_files/license_file'
    autoload :PackageManagerFile, 'licensee/project_files/package_manager_file'
    autoload :ReadmeFile, 'licensee/project_files/readme_file'
  end
end
