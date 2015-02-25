require 'linguist/blob_helper'
require 'linguist/generated'
require 'linguist/heuristics'
require 'linguist/language'
require 'linguist/repository'
require 'linguist/samples'
require 'linguist/shebang'
require 'linguist/version'

class << Linguist
  attr_accessor :instrumenter

  def instrument(*args, &bk)
    if instrumenter
      instrumenter.instrument(*args, &bk)
    else
      yield
    end
  end
end
