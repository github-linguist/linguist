# frozen_string_literal: true

module Octokit
  class Client
    # Method to check scopes
    #
    # @see https://developer.github.com/v3/oauth_authorizations/#oauth-authorizations-api
    module Tokens
      # Check scopes for a token
      #
      # @param token [String] GitHub OAuth token
      # @param options [Hash] Header params for request
      # @return [Array<String>] OAuth scopes
      # @see https://developer.github.com/v3/oauth/#scopes
      def scopes(token = @access_token, options = {})
        options = options.dup
        raise ArgumentError, 'Access token required' if token.nil?

        auth = { 'Authorization' => "token #{token}" }
        headers = (options.delete(:headers) || {}).merge(auth)

        agent.call(:get, 'user', headers: headers)
             .headers['X-OAuth-Scopes']
             .to_s
             .split(',')
             .map(&:strip)
             .sort
      end
    end
  end
end
