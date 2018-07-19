class IDVictim

  # Create elements of this man, woman, or child's identification.
  attr_accessor :name, :birthday, :gender, :hometown

  # Allows you to put in a space for anything which is not covered by the
  # preexisting elements.
  def self.new_element(element)
    attr_accessor element
  end

end
