# Based on http://johncarrino.net/blog/2006/08/11/powerset-in-ruby/
# See the link if you want a shorter version. This was intended to show the reader how the method works.
class Array
  # Adds a power_set method to every array, i.e.: [1, 2].power_set
  def power_set

    # Injects into a blank array of arrays.
    # acc is what we're injecting into
    # you is each element of the array
    inject([[]]) do |acc, you|
      ret = []             # Set up a new array to add into
      acc.each do |i|      # For each array in the injected array,
        ret << i           # Add itself into the new array
        ret << i + [you]   # Merge the array with a new array of the current element
      end
      ret       # Return the array we're looking at to inject more.
    end

  end

  # A more functional and even clearer variant.
  def func_power_set
    inject([[]]) { |ps,item|    # for each item in the Array
      ps +                      # take the powerset up to now and add
      ps.map { |e| e + [item] } # it again, with the item appended to each element
    }
  end
end

#A direct translation of the "power array" version above
require 'set'
class Set
  def powerset
    inject(Set[Set[]]) do |ps, item|
      ps.union ps.map {|e| e.union (Set.new [item])}
    end
  end
end

p [1,2,3,4].power_set
p %w(one two three).func_power_set

p Set[1,2,3].powerset
