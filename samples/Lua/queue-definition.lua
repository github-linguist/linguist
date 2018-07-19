Queue = {}

function Queue.new()
    return { first = 0, last = -1 }
end

function Queue.push( queue, value )
    queue.last = queue.last + 1
    queue[queue.last] = value
end

function Queue.pop( queue )
    if queue.first > queue.last then
        return nil
    end

    local val = queue[queue.first]
    queue[queue.first] = nil
    queue.first = queue.first + 1
    return val
end

function Queue.empty( queue )
    return queue.first > queue.last
end
