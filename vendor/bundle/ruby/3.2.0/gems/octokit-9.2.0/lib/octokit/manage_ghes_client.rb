# frozen_string_literal: true

require 'octokit/configurable'
require 'octokit/connection'
require 'octokit/warnable'
require 'octokit/manage_ghes_client/manage_ghes'

module Octokit
  # ManageGHESClient is only meant to be used by GitHub Enterprise Server (GHES) operators
  # and provides access to the Manage GHES API endpoints.
  #
  # @see Octokit::Client Use Octokit::Client for regular API use for GitHub
  #   and GitHub Enterprise.
  # @see https://developer.github.com/v3/enterprise-admin/manage-ghes/
  class ManageGHESClient
    include Octokit::Configurable
    include Octokit::Connection
    include Octokit::Warnable
    include Octokit::ManageGHESClient::ManageAPI

    def initialize(options = {})
      # Use options passed in, but fall back to module defaults
      # rubocop:disable Style/HashEachMethods
      #
      # This may look like a `.keys.each` which should be replaced with `#each_key`, but
      # this doesn't actually work, since `#keys` is just a method we've defined ourselves.
      # The class doesn't fulfill the whole `Enumerable` contract.
      Octokit::Configurable.keys.each do |key|
        # rubocop:enable Style/HashEachMethods
        instance_variable_set(:"@#{key}", options[key] || Octokit.instance_variable_get(:"@#{key}"))
      end
    end

    protected

    def endpoint
      manage_ghes_endpoint
    end

    # Set Manage GHES API endpoint
    #
    # @param value [String] Manage GHES API endpoint
    def manage_ghes_endpoint=(value)
      reset_agent
      @manage_ghes_endpoint = value
    end

    # Set Manage GHES API username
    #
    # @param value [String] Manage GHES API username
    def manage_ghes_username=(value)
      reset_agent
      @manage_ghes_username = value
    end

    # Set Manage GHES API password
    #
    # @param value [String] Manage GHES API password
    def manage_ghes_password=(value)
      reset_agent
      @manage_ghes_password = value
    end
  end
end
