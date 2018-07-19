p 'abcd'.start_with?('ab') #returns true
p 'abcd'.end_with?('ab')   #returns false
p 'abab'.include?('bb')    #returns false
p 'abab'.include?('ab')    #returns true
p 'abab'.index('bb')       #returns nil
p 'abab'.index('ab')       #returns 0
p 'abab'.index('ab', 1)    #returns 2
p 'abab'.rindex('ab')      #returns 2
