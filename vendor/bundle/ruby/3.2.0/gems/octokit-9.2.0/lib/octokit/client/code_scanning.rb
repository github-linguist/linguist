# frozen_string_literal: true

require 'tempfile'
require 'zlib'

module Octokit
  class Client
    # Methods for the code scanning alerts API
    #
    # @see https://docs.github.com/rest/code-scanning
    module CodeScanning
      # Updates a code scanning default setup configuration
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param state [String] The desired state of code scanning default setup
      # @param query_suite [String] CodeQL query suite to be used
      # @param languages [Array] List of CodeQL languages to be analyzed
      #
      # @return [Sawyer::Resource] Action Run information
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#update-a-code-scanning-default-setup-configuration
      def update_code_scanning_default_config(repo, state, query_suite = nil, languages = nil, options = {})
        options[:state] = state
        options[:query_suite] = query_suite if query_suite
        options[:languages] = languages if languages

        patch "#{Repository.path repo}/code-scanning/default-setup", options
      end

      # Get Code Scanning Default Configuration
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      #
      # @return [Sawyer::Resource] CodeQl Default Setup Configuration Information
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#get-a-code-scanning-default-setup-configuration
      def get_code_scanning_default_config(repo, options = {})
        get "#{Repository.path repo}/code-scanning/default-setup", options
      end

      # Gets a CodeQL database for a language in a repository
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param language [String]
      #
      # @return [Sawyer::Resource] CodeQl Default Setup Configuration Information
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#get-a-codeql-database-for-a-repository
      def get_codeql_database_for_repo(repo, language, options = {})
        get "#{Repository.path repo}/code-scanning/codeql/databases/#{language}", options
      end

      # Lists the CodeQL databases that are available in a repository
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      #
      # @return [Array] List of CodeQL Databases
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#list-codeql-databases-for-a-repository
      def list_codeql_database_for_repo(repo, options = {})
        get "#{Repository.path repo}/code-scanning/codeql/databases", options
      end

      # Delete a specified code scanning analysis from a repository
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param analysis_id [Integer] ID of the code scanning analysis
      #
      # @return [Sawyer::Resource] Next Code Scanning Analysis Information
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#delete-a-code-scanning-analysis-from-a-repository
      def delete_code_scanning_analysis(repo, analysis_id, options = {})
        delete "#{Repository.path repo}/code-scanning/analyses/#{analysis_id}", options
      end

      # Get a code scanning analysis for a repository
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param analysis_id [Integer] ID of the code scanning analysis
      #
      # @return [Sawyer::Resource] Code Scanning Analysis
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#get-a-code-scanning-analysis-for-a-repository
      def get_code_scanning_analysis(repo, analysis_id, options = {})
        get "#{Repository.path repo}/code-scanning/analyses/#{analysis_id}", options
      end

      # List code scanning analyses for a repository
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      #
      # @return [Array] List of Code Scanning Analyses
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#list-code-scanning-analyses-for-a-repository
      def list_code_scanning_analysis(repo, options = {})
        paginate "#{Repository.path repo}/code-scanning/analyses", options
      end

      # List instances of a code scanning alert
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param alert_number [Integer] The number that identifies an alert
      #
      # @return [Array] List of Code Scanning Alerts
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#list-instances-of-a-code-scanning-alert
      def list_instances_of_code_scanning_alert(repo, alert_number, options = {})
        paginate "#{Repository.path repo}/code-scanning/alerts/#{alert_number}/instances", options
      end

      # Update a code scanning alert
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param alert_number [Integer] The number that identifies an alert
      # @param state [String] The reason for dismissing or closing the alert. Required when the state is dismissed
      #
      # @return [Sawyer::Resource] Code Scanning Alert information
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#update-a-code-scanning-alert
      def update_code_scanning_alert(repo, alert_number, state, reason, comment = nil, options = {})
        options[:state] = state
        options[:dismissed_reason] = reason
        options[:dismissed_comment] = comment if comment

        patch "#{Repository.path repo}/code-scanning/alerts/#{alert_number}", options
      end

      # Gets a single code scanning alert
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param alert_number [Integer] The number that identifies an alert
      #
      # @return [Sawyer::Resource] Code Scanning Alert
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#get-a-code-scanning-alert
      def get_code_scanning_alert(repo, alert_number, options = {})
        get "#{Repository.path repo}/code-scanning/alerts/#{alert_number}", options
      end

      # List code scanning alerts for a repository
      #
      # @param org [String] A GitHub organization
      #
      # @return [Array] Code Scanning Alert information
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#list-code-scanning-alerts-for-a-repository
      def list_code_scanning_alerts_for_repo(repo, options = {})
        paginate "#{Repository.path repo}/code-scanning/alerts", options
      end

      # List code scanning alerts for an organization
      #
      # @param org [String] A GitHub organization
      #
      # @return [Array] Code Scanning Alert information
      # @see https://docs.github.com/en/rest/code-scanning/code-scanning#list-code-scanning-alerts-for-an-organization
      def list_code_scanning_alerts_for_org(org, options = {})
        paginate "orgs/#{org}/code-scanning/alerts", options
      end

      # Uploads SARIF data containing the results of a code scanning analysis
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param file [String] Path to the SARIF file to upload
      # @param sha [String] The SHA of the commit to which the analysis you are uploading relates
      # @param ref [String] The full Git reference, formatted as `refs/heads/<branch name>`, `refs/pull/<number>/merge`, or `refs/pull/<number>/head`
      #
      # @return [Sawyer::Resource] SARIF upload information
      # @see https://docs.github.com/rest/code-scanning#upload-an-analysis-as-sarif-data
      def upload_sarif_data(repo, file, sha, ref, options = {})
        options[:sarif] = compress_sarif_data(file)
        options[:commit_sha] = sha
        options[:ref] = ref

        post "#{Repository.path repo}/code-scanning/sarifs", options
      end

      # Gets information about a SARIF upload
      #
      # @param repo [Integer, String, Repository, Hash] A GitHub repository
      # @param sarif_id [String] The SARIF ID obtained after uploading
      #
      # @return [Sawyer::Resource] SARIF upload information
      # @see https://docs.github.com/rest/code-scanning#get-information-about-a-sarif-upload
      def get_sarif_upload_information(repo, sarif_id, options = {})
        get "#{Repository.path repo}/code-scanning/sarifs/#{sarif_id}", options
      end

      private

      def compress_sarif_data(file)
        Tempfile.create('sarif.gz') do |tempfile|
          Zlib::GzipWriter.open(tempfile) do |gz_file|
            gz_file.write File.binread(file)
          end
          [tempfile.read].pack('m0') # Base64.strict_encode64
        end
      end
    end
  end
end
