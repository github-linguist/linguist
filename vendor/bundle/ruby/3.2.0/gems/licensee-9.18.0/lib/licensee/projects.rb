# frozen_string_literal: true

module Licensee
  module Projects
    autoload :Project, 'licensee/projects/project'
    autoload :FSProject, 'licensee/projects/fs_project'
    autoload :GitProject, 'licensee/projects/git_project'
    autoload :GitHubProject, 'licensee/projects/github_project'
  end
end
