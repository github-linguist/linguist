=begin rdoc
RDoc is documented here[http://www.ruby-doc.org/core/classes/RDoc.html].

This is a class documentation comment.  This text shows at the top of the page
for the class.

Comments can be written inside "=begin rdoc"/"end" blocks or
in normal '#' comment blocks.

There are no '@parameters' like javadoc, but 'name-value' lists can be written:
Author:: Joe Schmoe
Date:: today
=end

class Doc
  # This is a comment for a Constant
  Constant = nil

  # This is a method comment.  Parameters and return values can be named
  # with the "call-seq" directive.
  #
  # call-seq:
  #   a_method(first_arg, second_arg) -> return_value
  #
  def a_method(arg1, arg2='default value')
    do_stuff
  end

  # Class methods will be shown in a separate section of the generated documentation.
  def self.class_method
    Constant
  end
end

# :include:boilerplate.txt
