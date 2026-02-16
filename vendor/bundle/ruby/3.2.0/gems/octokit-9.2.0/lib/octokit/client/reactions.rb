# frozen_string_literal: true

module Octokit
  class Client
    # Methods for the Reacions API
    #
    # @see https://developer.github.com/v3/reactions/
    module Reactions
      # List reactions for a commit comment
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param id [Integer] The id of the commit comment
      # @see https://developer.github.com/v3/reactions/#list-reactions-for-a-commit-comment
      #
      # @example
      #   @client.commit_comment_reactions("octokit/octokit.rb", 1)
      #
      # @return [Array<Sawyer::Resource>] Array of Hashes representing the reactions.
      def commit_comment_reactions(repo, id, options = {})
        get "#{Repository.path repo}/comments/#{id}/reactions", options
      end

      # Create a reaction for a commit comment
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param id [Integer] The id of the commit comment
      # @param reaction [String] The Reaction
      # @see https://developer.github.com/v3/reactions/#create-reaction-for-a-commit-comment
      # @see https://developer.github.com/v3/reactions/#reaction-types
      #
      # @example
      #   @client.create_commit_comment_reactions("octokit/octokit.rb", 1)
      #
      # @return [<Sawyer::Resource>] Hash representing the reaction
      def create_commit_comment_reaction(repo, id, reaction, options = {})
        options = options.merge(content: reaction)
        post "#{Repository.path repo}/comments/#{id}/reactions", options
      end

      # List reactions for an issue
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param number [Integer] The Issue number
      # @see https://developer.github.com/v3/reactions/#list-reactions-for-an-issue
      #
      # @example
      #   @client.issue_reactions("octokit/octokit.rb", 1)
      #
      # @return [Array<Sawyer::Resource>] Array of Hashes representing the reactions.
      def issue_reactions(repo, number, options = {})
        get "#{Repository.path repo}/issues/#{number}/reactions", options
      end

      # Create reaction for an issue
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param number [Integer] The Issue number
      # @param reaction [String] The Reaction
      #
      # @see https://developer.github.com/v3/reactions/#create-reaction-for-an-issue
      # @see https://developer.github.com/v3/reactions/#reaction-types
      #
      # @example
      #   @client.create_issue_reaction("octokit/octokit.rb", 1)
      #
      # @return [<Sawyer::Resource>] Hash representing the reaction.
      def create_issue_reaction(repo, number, reaction, options = {})
        options = options.merge(content: reaction)
        post "#{Repository.path repo}/issues/#{number}/reactions", options
      end

      # List reactions for an issue comment
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param id [Integer] The Issue comment id
      #
      # @see https://developer.github.com/v3/reactions/#list-reactions-for-an-issue-comment
      #
      # @example
      #   @client.issue_comment_reactions("octokit/octokit.rb", 1)
      #
      # @return [Array<Sawyer::Resource>] Array of Hashes representing the reactions.
      def issue_comment_reactions(repo, id, options = {})
        get "#{Repository.path repo}/issues/comments/#{id}/reactions", options
      end

      # Create reaction for an issue comment
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param id [Integer] The Issue comment id
      # @param reaction [String] The Reaction
      #
      # @see https://developer.github.com/v3/reactions/#create-reaction-for-an-issue-comment
      # @see https://developer.github.com/v3/reactions/#reaction-types
      #
      # @example
      #   @client.create_issue_comment_reaction("octokit/octokit.rb", 1)
      #
      # @return [<Sawyer::Resource>] Hashes representing the reaction.
      def create_issue_comment_reaction(repo, id, reaction, options = {})
        options = options.merge(content: reaction)
        post "#{Repository.path repo}/issues/comments/#{id}/reactions", options
      end

      # List reactions for a pull request review comment
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param id [Integer] The Issue comment id
      #
      # @see https://developer.github.com/v3/reactions/#list-reactions-for-a-pull-request-review-comment
      #
      # @example
      #   @client.pull_request_review_comment_reactions("octokit/octokit.rb", 1)
      #
      # @return [Array<Sawyer::Resource>] Array of Hashes representing the reactions.
      def pull_request_review_comment_reactions(repo, id, options = {})
        get "#{Repository.path repo}/pulls/comments/#{id}/reactions", options
      end

      # Create reaction for a pull request review comment
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param id [Integer] The Issue comment id
      # @param reaction [String] The Reaction
      #
      # @see https://developer.github.com/v3/reactions/#create-reaction-for-a-pull-request-review-comment
      # @see https://developer.github.com/v3/reactions/#reaction-types
      #
      # @example
      #   @client.create_pull_request_reiew_comment_reaction("octokit/octokit.rb", 1)
      #
      # @return [<Sawyer::Resource>] Hash representing the reaction.
      def create_pull_request_review_comment_reaction(repo, id, reaction, options = {})
        options = options.merge(content: reaction)
        post "#{Repository.path repo}/pulls/comments/#{id}/reactions", options
      end

      # Delete a reaction
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param issue_id [Integer] The Issue comment id
      # @param reaction_id [Integer] The Reaction id
      #
      # @see https://docs.github.com/en/rest/reactions/reactions#delete-an-issue-reaction
      #
      # @example
      #   @client.delete_issue_reaction("octokit/octokit.rb", 1, 2)
      #
      # @return [Boolean] Return true if reaction was deleted, false otherwise.
      def delete_issue_reaction(repo, issue_id, reaction_id, options = {})
        boolean_from_response :delete, "#{Repository.path repo}/issues/#{issue_id}/reactions/#{reaction_id}", options
      end

      # List reactions for a release
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param id [Integer] The Release id
      #
      # @see https://docs.github.com/en/free-pro-team@latest/rest/reactions/reactions?apiVersion=2022-11-28#list-reactions-for-a-release
      #
      # @example
      #   @client.release_reactions("octokit/octokit.rb", 1)
      #
      # @return [Array<Sawyer::Resource>] Array of Hashes representing the reactions.
      def release_reactions(repo, release_id, options = {})
        get "#{Repository.path repo}/releases/#{release_id}/reactions", options
      end

      # Create reaction for a release
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param id [Integer] The Release id
      # @param reaction [String] The Reaction
      #
      # @see https://docs.github.com/en/free-pro-team@latest/rest/reactions/reactions?apiVersion=2022-11-28#create-reaction-for-a-release
      # @see https://developer.github.com/v3/reactions/#reaction-types
      #
      # @example
      #   @client.create_release_reaction("octokit/octokit.rb", 1)
      #
      # @return [<Sawyer::Resource>] Hash representing the reaction.
      def create_release_reaction(repo, release_id, reaction, options = {})
        options = options.merge(content: reaction)
        post "#{Repository.path repo}/releases/#{release_id}/reactions", options
      end

      # Delete a reaction for a release
      #
      # @param repo [Integer, String, Hash, Repository] A GitHub repository
      # @param issue_id [Integer] The Release id
      # @param reaction_id [Integer] The Reaction id
      #
      # @see https://docs.github.com/en/free-pro-team@latest/rest/reactions/reactions?apiVersion=2022-11-28#delete-a-release-reaction
      #
      # @example
      #   @client.delete_release_reaction("octokit/octokit.rb", 1, 2)
      #
      # @return [Boolean] Return true if reaction was deleted, false otherwise.
      def delete_release_reaction(repo, release_id, reaction_id, options = {})
        boolean_from_response :delete, "#{Repository.path repo}/releases/#{release_id}/reactions/#{reaction_id}", options
      end
    end
  end
end
