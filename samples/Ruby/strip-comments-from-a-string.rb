class String
  def strip_comment( markers = ['#',';'] )
    re = Regexp.union( markers ) # construct a regular expression which will match any of the markers
    if index = (self =~ re)
      self[0, index].rstrip      # slice the string where the regular expression matches, and return it.
    else
      rstrip
    end
  end
end

p 'apples, pears # and bananas'.strip_comment
str = 'apples, pears ; and bananas'
p str.strip_comment
str = 'apples, pears and bananas '
p str.strip_comment
p str.strip_comment('and')
p " \t \n ;".strip_comment
p "".strip_comment
