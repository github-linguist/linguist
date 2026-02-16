# frozen_string_literal: true

module Octokit
  class Client
    # Methods for the Actions Secrets API
    #
    # @see https://developer.github.com/v3/actions/secrets/
    module ActionsSecrets
      # Get public key for secrets encryption
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @return [Hash] key_id and key
      # @see https://developer.github.com/v3/actions/secrets/#get-your-public-key
      def get_actions_public_key(repo)
        get "#{Repository.path repo}/actions/secrets/public-key"
      end

      # Get public key for secrets encryption
      #
      # @param org [String] A GitHub organization
      # @return [Hash] key_id and key
      # @see https://developer.github.com/v3/actions/secrets/#get-your-public-key
      def get_org_actions_public_key(org)
        get "#{Organization.path org}/actions/secrets/public-key"
      end

      # List secrets
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @return [Hash] total_count and list of secrets (each item is hash with name, created_at and updated_at)
      # @see https://developer.github.com/v3/actions/secrets/#list-secrets-for-a-repository
      def list_actions_secrets(repo)
        paginate "#{Repository.path repo}/actions/secrets" do |data, last_response|
          data.secrets.concat last_response.data.secrets
        end
      end

      # List org secrets
      #
      # @param org [String] A GitHub organization
      # @return [Hash] total_count and list of secrets (each item is hash with name, created_at and updated_at)
      # @see https://developer.github.com/v3/actions/secrets/#list-organization-secrets
      def list_org_actions_secrets(org)
        paginate "#{Organization.path org}/actions/secrets" do |data, last_response|
          data.secrets.concat last_response.data.secrets
        end
      end

      # Get a secret
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param name [String] Name of secret
      # @return [Hash] name, created_at and updated_at
      # @see https://developer.github.com/v3/actions/secrets/#get-a-secret
      def get_actions_secret(repo, name)
        get "#{Repository.path repo}/actions/secrets/#{name}"
      end

      # Get an org secret
      #
      # @param org [String] A GitHub organization
      # @param name [String] Name of secret
      # @return [Hash] name, created_at and updated_at
      # @see https://developer.github.com/v3/actions/secrets/#get-a-secret
      def get_org_actions_secret(org, name)
        get "#{Organization.path org}/actions/secrets/#{name}"
      end

      # Create or update secrets
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param name [String] Name of secret
      # @param options [Hash] encrypted_value and key_id
      # @see https://developer.github.com/v3/actions/secrets/#create-or-update-a-secret-for-a-repository
      def create_or_update_actions_secret(repo, name, options)
        put "#{Repository.path repo}/actions/secrets/#{name}", options
      end

      # Create or update org secrets
      #
      # @param org [String] A GitHub organization
      # @param name [String] Name of secret
      # @param options [Hash] encrypted_value and key_id
      # @see https://developer.github.com/v3/actions/secrets/#create-or-update-a-secret
      def create_or_update_org_actions_secret(org, name, options)
        put "#{Organization.path org}/actions/secrets/#{name}", options
      end

      # Delete a secret
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param name [String] Name of secret
      # @see https://developer.github.com/v3/actions/secrets/#delete-a-secret-from-a-repository
      def delete_actions_secret(repo, name)
        boolean_from_response :delete, "#{Repository.path repo}/actions/secrets/#{name}"
      end

      # Delete an org secret
      #
      # @param org [String] A GitHub organization
      # @param name [String] Name of secret
      # @see https://developer.github.com/v3/actions/secrets/#delete-a-secret
      def delete_org_actions_secret(org, name)
        boolean_from_response :delete, "#{Organization.path org}/actions/secrets/#{name}"
      end

      # Get environment public key for secrets encryption
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param environment [String] Name of environment
      # @return [Hash] key_id and key
      # @see https://docs.github.com/en/rest/actions/secrets#get-an-environment-public-key
      def get_actions_environment_public_key(repo, environment)
        get "#{Repository.path repo}/environments/#{environment}/secrets/public-key"
      end

      # List environment secrets
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param environment [String] Name of environment
      # @return [Hash] total_count and list of secrets (each item is hash with name, created_at and updated_at)
      # @see https://developer.github.com/v3/actions/secrets/#list-environment-secrets
      def list_actions_environment_secrets(repo, environment)
        paginate "#{Repository.path repo}/environments/#{environment}/secrets" do |data, last_response|
          data.secrets.concat last_response.data.secrets
        end
      end

      # Get an environment secret
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param environment [String] Name of environment
      # @param name [String] Name of secret
      # @return [Hash] name, created_at and updated_at
      # @see https://docs.github.com/en/rest/actions/secrets#get-an-environment-secret
      def get_actions_environment_secret(repo, environment, name)
        get "#{Repository.path repo}/environments/#{environment}/secrets/#{name}"
      end

      # Create or update an environment secret
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param environment [String] Name of environment
      # @param name [String] Name of secret
      # @param options [Hash] encrypted_value and key_id
      # @see https://docs.github.com/en/rest/actions/secrets#create-or-update-an-environment-secret
      def create_or_update_actions_environment_secret(repo, environment, name, options)
        put "#{Repository.path repo}/environments/#{environment}/secrets/#{name}", options
      end

      # Delete environment secret
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param environment [String] Name of environment
      # @param name [String] Name of secret
      # @see https://docs.github.com/en/rest/actions/secrets#delete-an-environment-secret
      def delete_actions_environment_secret(repo, environment, name)
        boolean_from_response :delete, "#{Repository.path repo}/environments/#{environment}/secrets/#{name}"
      end
    end
  end
end
