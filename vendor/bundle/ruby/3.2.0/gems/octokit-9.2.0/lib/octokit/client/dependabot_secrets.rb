# frozen_string_literal: true

module Octokit
  class Client
    # Methods for the dependabot Secrets API
    #
    # @see https://docs.github.com/en/rest/dependabot/
    module DependabotSecrets
      # Get public key for secrets encryption
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @return [Hash] key_id and key
      # @see https://docs.github.com/en/rest/dependabot/repository-secrets#get-a-repository-public-key
      def get_dependabot_public_key(repo)
        get "#{Repository.path repo}/dependabot/secrets/public-key"
      end

      # Get public key for secrets encryption
      #
      # @param org [String] A GitHub organization
      # @return [Hash] key_id and key
      # @see https://docs.github.com/en/rest/dependabot/organization-secrets?apiVersion=2022-11-28#get-an-organization-public-key
      def get_org_dependabot_public_key(org)
        get "#{Organization.path org}/dependabot/secrets/public-key"
      end

      # List secrets
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @return [Hash] total_count and list of secrets (each item is hash with name, created_at and updated_at)
      # @see https://docs.github.com/en/rest/dependabot/repository-secrets?apiVersion=2022-11-28#list-repository-secrets
      def list_dependabot_secrets(repo)
        paginate "#{Repository.path repo}/dependabot/secrets" do |data, last_response|
          data.secrets.concat last_response.data.secrets
        end
      end

      # List org secrets
      #
      # @param org [String] A GitHub organization
      # @return [Hash] total_count and list of secrets (each item is hash with name, created_at and updated_at)
      # @see https://docs.github.com/en/rest/dependabot/organization-secrets?apiVersion=2022-11-28#list-organization-secrets
      def list_org_dependabot_secrets(org)
        paginate "#{Organization.path org}/dependabot/secrets" do |data, last_response|
          data.secrets.concat last_response.data.secrets
        end
      end

      # Get a secret
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param name [String] Name of secret
      # @return [Hash] name, created_at, updated_at, and visibility
      # @see https://docs.github.com/en/rest/dependabot/repository-secrets?apiVersion=2022-11-28#get-a-repository-secret
      def get_dependabot_secret(repo, name)
        get "#{Repository.path repo}/dependabot/secrets/#{name}"
      end

      # Get an org secret
      #
      # @param org [String] A GitHub organization
      # @param name [String] Name of secret
      # @return [Hash] name, created_at, updated_at, and visibility
      # @see https://docs.github.com/en/rest/dependabot/organization-secrets?apiVersion=2022-11-28#get-an-organization-secret
      def get_org_dependabot_secret(org, name)
        get "#{Organization.path org}/dependabot/secrets/#{name}"
      end

      # Create or update secrets
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param name [String] Name of secret
      # @param options [Hash] encrypted_value and key_id
      # @see https://docs.github.com/en/rest/dependabot/repository-secrets?apiVersion=2022-11-28#create-or-update-a-repository-secret
      def create_or_update_dependabot_secret(repo, name, options)
        put "#{Repository.path repo}/dependabot/secrets/#{name}", options
      end

      # Create or update org secrets
      #
      # @param org [String] A GitHub organization
      # @param name [String] Name of secret
      # @param options [Hash] encrypted_value and key_id
      # @see https://docs.github.com/en/rest/dependabot/organization-secrets?apiVersion=2022-11-28#create-or-update-an-organization-secret
      def create_or_update_org_dependabot_secret(org, name, options)
        put "#{Organization.path org}/dependabot/secrets/#{name}", options
      end

      # Delete a secret
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param name [String] Name of secret
      # @see https://docs.github.com/en/rest/dependabot/repository-secrets?apiVersion=2022-11-28#delete-a-repository-secret
      def delete_dependabot_secret(repo, name)
        boolean_from_response :delete, "#{Repository.path repo}/dependabot/secrets/#{name}"
      end

      # Delete an org secret
      #
      # @param org [String] A GitHub organization
      # @param name [String] Name of secret
      # @see https://docs.github.com/en/rest/dependabot/organization-secrets?apiVersion=2022-11-28#delete-an-organization-secret
      def delete_org_dependabot_secret(org, name)
        boolean_from_response :delete, "#{Organization.path org}/dependabot/secrets/#{name}"
      end
    end
  end
end
