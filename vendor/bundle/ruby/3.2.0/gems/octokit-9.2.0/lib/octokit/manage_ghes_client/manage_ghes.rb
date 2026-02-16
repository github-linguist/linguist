# frozen_string_literal: true

module Octokit
  # Client for the Manage GitHub Enterprise Server API
  class ManageGHESClient
    # Methods for the Manage GitHub Enterprise Server API
    #
    # @see https://developer.github.com/v3/enterprise-admin/manage-ghes
    module ManageAPI
      # Get information about the maintenance status of the GHES instance
      #
      # @return [nil]
      def maintenance_mode
        conn = authenticated_client

        @last_response = conn.get('/manage/v1/maintenance')
      end

      # Configure the maintenance mode of the GHES instance
      #
      # @param maintenance [Hash] A hash configuration of the maintenance mode status
      # @return [nil]
      def set_maintenance_mode(enabled, options = {})
        conn = authenticated_client

        options[:enabled] = enabled
        @last_response = conn.post('/manage/v1/maintenance', options)
      end
      alias configure_maintenance_mode set_maintenance_mode
    end

    # Uploads a license for the first time
    #
    # @param license [String] The path to your .ghl license file.
    #
    # @return [nil]
    def upload_license(license)
      conn = authenticated_client
      begin
        conn.request :multipart
      rescue Faraday::Error
        raise Faraday::Error, <<~ERROR
          The `faraday-multipart` gem is required to upload a license.
          Please add `gem "faraday-multipart"` to your Gemfile.
        ERROR
      end
      params = {}
      params[:license] = Faraday::FilePart.new(license, 'binary')
      params[:password] = @manage_ghes_password
      @last_response = conn.post('/manage/v1/config/init', params, { 'Content-Type' => 'multipart/form-data' })
    end

    # Start a configuration process.
    #
    # @return [nil]
    def start_configuration
      conn = authenticated_client
      @last_response = conn.post('/manage/v1/config/apply')
    end

    # Get information about the Enterprise installation
    #
    # @return [nil]
    def config_status
      conn = authenticated_client
      @last_response = conn.get('/manage/v1/config/apply')
    end
    alias config_check config_status

    # Get information about the Enterprise installation
    #
    # @return [nil]
    def settings
      conn = authenticated_client
      @last_response = conn.get('/manage/v1/config/settings')
    end
    alias get_settings settings

    # Modify the Enterprise settings
    #
    # @param settings [Hash] A hash configuration of the new settings
    #
    # @return [nil]
    def edit_settings(settings)
      conn = authenticated_client
      @last_response = conn.put('/manage/v1/config/settings', settings.to_json.to_s)
    end

    def authorized_keys
      conn = authenticated_client
      @last_response = conn.get('/manage/v1/access/ssh')
    end
    alias get_authorized_keys authorized_keys

    # Add an authorized SSH keys on the Enterprise install
    #
    # @param key Either the file path to a key, a File handler to the key, or the contents of the key itself
    # @return [nil]
    def add_authorized_key(key)
      conn = authenticated_client
      case key
      when String
        if File.exist?(key)
          key = File.open(key, 'r')
          content = key.read.strip
          key.close
        else
          content = key
        end
      when File
        content = key.read.strip
        key.close
      end

      queries = {}
      queries[:key] = content
      @last_response = conn.post('/manage/v1/access/ssh', queries)
    end

    # Removes an authorized SSH keys from the Enterprise install
    #
    # @param key Either the file path to a key, a File handler to the key, or the contents of the key itself
    # @return [nil]
    def remove_authorized_key(key)
      conn = authenticated_client
      case key
      when String
        if File.exist?(key)
          key = File.open(key, 'r')
          content = key.read.strip
          key.close
        else
          content = key
        end
      when File
        content = key.read.strip
        key.close
      end

      queries = {}
      queries[:key] = content
      @last_response = conn.run_request(:delete, '/manage/v1/access/ssh', queries, nil)
    end
    alias delete_authorized_key remove_authorized_key

    private

    def basic_authenticated?
      !!(@manage_ghes_username && @manage_ghes_password)
    end

    # If no username is provided, we assume root site admin should be used
    def root_site_admin_assumed?
      !@manage_ghes_username
    end

    def authenticated_client
      @authenticated_client ||= Faraday.new(url: @manage_ghes_endpoint) do |c|
        c.headers[:user_agent] = user_agent
        c.request  :json
        c.response :json
        c.adapter Faraday.default_adapter

        if root_site_admin_assumed?
          username = 'api_key'
        elsif basic_authenticated?
          username = @manage_ghes_username
        end
        c.request(*FARADAY_BASIC_AUTH_KEYS, username, @manage_ghes_password)

        # Disabling SSL is essential for certain self-hosted Enterprise instances
        c.ssl[:verify] = false if connection_options[:ssl] && !connection_options[:ssl][:verify]

        c.use Octokit::Response::RaiseError
      end
    end
  end
end
