import java.util.ArrayList

class Door
	:state

	def initialize
		@state=false
	end

	def closed?; !@state; end
	def open?; @state; end

	def close; @state=false; end
	def open; @state=true; end

	def toggle
		if closed?
			open
		else
			close
		end
	end

	def toString; Boolean.toString(@state); end
end

doors=ArrayList.new
1.upto(100) do
    doors.add(Door.new)
end

1.upto(100) do |multiplier|
    index = 0
    doors.each do |door|
        Door(door).toggle if (index+1)%multiplier == 0
        index += 1
    end
end

i = 0
doors.each do |door|
    puts "Door #{i+1} is #{door}."
    i+=1
end
