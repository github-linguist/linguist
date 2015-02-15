q = Queue.new()
Queue.push( q, 5 )
Queue.push( q, "abc" )

while not Queue.empty( q ) do
    print( Queue.pop( q ) )
end
