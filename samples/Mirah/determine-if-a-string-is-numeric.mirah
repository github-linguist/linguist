import java.text.NumberFormat
import java.text.ParsePosition
import java.util.Scanner

# this first example relies on catching an exception,
# which is bad style and poorly performing in Java
def is_numeric?(s:string)
    begin
        Double.parseDouble(s)
        return true
    rescue
        return false
    end
end

puts '123   is numeric' if is_numeric?('123')
puts '-123  is numeric' if is_numeric?('-123')
puts '123.1 is numeric' if is_numeric?('123.1')

puts 'nil   is not numeric' unless is_numeric?(nil)
puts "''    is not numeric" unless is_numeric?('')
puts 'abc   is not numeric' unless is_numeric?('abc')
puts '123-  is not numeric' unless is_numeric?('123-')
puts '1.2.3 is not numeric' unless is_numeric?('1.2.3')


# check every element of the string
def is_numeric2?(s: string)
    if (s == nil || s.isEmpty())
        return false
    end
    if (!s.startsWith('-'))
        if s.contains('-')
            return false
        end
    end

    0.upto(s.length()-1) do |x|
        c = s.charAt(x)
        if ((x == 0) && (c == '-'.charAt(0)))
            # negative number
        elsif (c == '.'.charAt(0))
            if (s.indexOf('.', x) > -1)
                return false # more than one period
            end
        elsif (!Character.isDigit(c))
            return false
        end
    end
    true
end


puts '123   is numeric' if is_numeric2?('123')
puts '-123  is numeric' if is_numeric2?('-123')
puts '123.1 is numeric' if is_numeric2?('123.1')

puts 'nil   is not numeric' unless is_numeric2?(nil)
puts "''    is not numeric" unless is_numeric2?('')
puts 'abc   is not numeric' unless is_numeric2?('abc')
puts '123-  is not numeric' unless is_numeric2?('123-')
puts '1.2.3 is not numeric' unless is_numeric2?('1.2.3')



# use a regular  expression
def is_numeric3?(s:string)
  s == nil || s.matches("[-+]?\\d+(\\.\\d+)?")
end

puts '123   is numeric' if is_numeric3?('123')
puts '-123  is numeric' if is_numeric3?('-123')
puts '123.1 is numeric' if is_numeric3?('123.1')

puts 'nil   is not numeric' unless is_numeric3?(nil)
puts "''    is not numeric" unless is_numeric3?('')
puts 'abc   is not numeric' unless is_numeric3?('abc')
puts '123-  is not numeric' unless is_numeric3?('123-')
puts '1.2.3 is not numeric' unless is_numeric3?('1.2.3')


#  use the positional parser in the java.text.NumberFormat object
# (a more robust solution). If, after parsing, the parse position is at
# the end of the string, we can deduce that the entire string was a
# valid number.
def is_numeric4?(s:string)
    return false if s == nil
    formatter = NumberFormat.getInstance()
    pos = ParsePosition.new(0)
    formatter.parse(s, pos)
    s.length() == pos.getIndex()
end


puts '123   is numeric' if is_numeric4?('123')
puts '-123  is numeric' if is_numeric4?('-123')
puts '123.1 is numeric' if is_numeric4?('123.1')

puts 'nil   is not numeric' unless is_numeric4?(nil)
puts "''    is not numeric" unless is_numeric4?('')
puts 'abc   is not numeric' unless is_numeric4?('abc')
puts '123-  is not numeric' unless is_numeric4?('123-')
puts '1.2.3 is not numeric' unless is_numeric4?('1.2.3')


# use the java.util.Scanner object. Very useful if you have to
# scan multiple entries. Scanner also has similar methods for longs,
# shorts, bytes, doubles, floats, BigIntegers, and BigDecimals as well
# as methods for integral types where you may input a base/radix other than
# 10 (10 is the default, which can be changed using the useRadix method).
def is_numeric5?(s:string)
    return false if s == nil
    Scanner sc = Scanner.new(s)
    sc.hasNextDouble()
end

puts '123   is numeric' if is_numeric5?('123')
puts '-123  is numeric' if is_numeric5?('-123')
puts '123.1 is numeric' if is_numeric5?('123.1')

puts 'nil   is not numeric' unless is_numeric5?(nil)
puts "''    is not numeric" unless is_numeric5?('')
puts 'abc   is not numeric' unless is_numeric5?('abc')
puts '123-  is not numeric' unless is_numeric5?('123-')
puts '1.2.3 is not numeric' unless is_numeric5?('1.2.3')
