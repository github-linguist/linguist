# frozen_string_literal: true

module Octokit
  class Client
    # Methods for the Environments API
    #
    # @see https://docs.github.com/en/rest/deployments/environments
    module Environments
      # Fetch a single environment for a repository
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param environment_name [String] The name of the environment
      # @return <Sawyer::Resource> A single environment
      # @see https://docs.github.com/en/rest/deployments/environments#get-an-environment
      def environment(repo, environment_name, options = {})
        get("#{Repository.path repo}/environments/#{environment_name}", options)
      end

      # Lists the environments for a repository
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @option options [Integer] :per_page The number of results per page (max 100). Default: 30
      # @option options [Integer] :page Page number of the results to fetch. Default: 1
      # @return [Sawyer::Resource] Total count of environments and list of environments
      # @see https://docs.github.com/en/rest/deployments/environments#list-environments
      def environments(repo, options = {})
        paginate("#{Repository.path repo}/environments", options) do |data, last_response|
          data.environments.concat last_response.data.environments
          data.total_count += last_response.data.total_count
        end
      end
      alias list_environments environments

      # Create or update an environment with protection rules, such as required reviewers
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param environment_name [String] The name of the environment
      # @option options [Integer] :wait_timer The amount of time to delay a job after the job is initially triggered. The time (in minutes) must be an integer between 0 and 43,200 (30 days).
      # @option options [Array] :reviewers The people or teams that may review jobs that reference the environment. You can list up to six users or teams as reviewers.
      # @option options [Object] :deployment_branch_policy The type of deployment branch policy for this environment. To allow all branches to deploy, set to null.
      # @return [Sawyer::Resource] An environment
      # @see https://docs.github.com/en/rest/deployments/environments#create-or-update-an-environment
      def create_or_update_environment(repo, environment_name, options = {})
        put("#{Repository.path repo}/environments/#{environment_name}", options)
      end

      # Delete an Environment
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param environment_name [String] The name of the environment
      # @return [No Content]
      # @see https://docs.github.com/en/rest/deployments/environments#delete-an-environment
      def delete_environment(repo, environment_name, options = {})
        delete("#{Repository.path repo}/environments/#{environment_name}", options)
      end
    end
  end
end
