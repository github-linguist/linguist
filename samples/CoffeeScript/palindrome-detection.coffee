    String::isPalindrome = ->
        for i in [0...@length / 2] when @[i] isnt @[@length - (i + 1)]
            return no
        yes

    String::stripped = -> @toLowerCase().replace /\W/gi, ''

    console.log "'#{ str }' : #{ str.stripped().isPalindrome() }" for str in [
        'In girum imus nocte et consumimur igni'
        'A man, a plan, a canal: Panama!'
        'There is no spoon.'
    ]
