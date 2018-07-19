import java.util.regex.Pattern
import java.util.regex.Matcher

#The "remove and count the difference" method
def count_substring(pattern:string, source:string)
    (source.length() - source.replace(pattern, "").length()) / pattern.length()
end

puts count_substring("th", "the three truths")      # ==> 3
puts count_substring("abab", "ababababab")          # ==> 2
puts count_substring("a*b", "abaabba*bbaba*bbab")   # ==> 2


# The "split and count" method
def count_substring2(pattern:string, source:string)
    # the result of split() will contain one more element than the delimiter
	# the "-1" second argument makes it not discard trailing empty strings
    source.split(Pattern.quote(pattern), -1).length - 1
end

puts count_substring2("th", "the three truths")      # ==> 3
puts count_substring2("abab", "ababababab")          # ==> 2
puts count_substring2("a*b", "abaabba*bbaba*bbab")   # ==> 2


# This method does a match and counts how many times it matches
def count_substring3(pattern:string, source:string)
    result = 0
    Matcher m = Pattern.compile(Pattern.quote(pattern)).matcher(source);
    while (m.find())
        result = result + 1
    end
    result
end

puts count_substring3("th", "the three truths")      # ==> 3
puts count_substring3("abab", "ababababab")          # ==> 2
puts count_substring3("a*b", "abaabba*bbaba*bbab")   # ==> 2
