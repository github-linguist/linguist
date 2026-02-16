# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  module Credentials
    # A plain-text username and password credential object.
    class UserPassword
      def initialize(options)
        @username, @password = options[:username], options[:password]
      end

      def call(url, username_from_url, allowed_types)
        self
      end
    end

    # A ssh key credential object that can optionally be passphrase-protected
    class SshKey
      def initialize(options)
        @username, @publickey, @privatekey, @passphrase = options[:username], options[:publickey], options[:privatekey], options[:passphrase]
      end

      def call(url, username_from_url, allowed_types)
        self
      end
    end

    class SshKeyFromAgent
      def initialize(options)
        @username = options[:username]
      end

      def call(url, username_from_url, allowed_types)
        self
      end
    end

    # A "default" credential usable for Negotiate mechanisms like NTLM or
    # Kerberos authentication
    class Default
      def call(url, username_from_url, allowed_types)
        self
      end
    end
  end
end
