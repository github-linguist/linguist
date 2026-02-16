# frozen_string_literal: true

module Octokit
  # Current major release.
  # @return [Integer]
  MAJOR = 9

  # Current minor release.
  # @return [Integer]
  MINOR = 2

  # Current patch level.
  # @return [Integer]
  PATCH = 0

  # Full release version.
  # @return [String]
  VERSION = [MAJOR, MINOR, PATCH].join('.').freeze
end
