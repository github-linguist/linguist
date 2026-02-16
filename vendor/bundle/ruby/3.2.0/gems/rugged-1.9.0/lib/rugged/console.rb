# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

# Loaded by script/console. Land helpers here.

def repo
  Rugged::Repository.new(File.expand_path('../../../', __FILE__))
end

Pry.config.prompt = lambda do |context, nesting, pry|
  "[rugged] #{context}> "
end
