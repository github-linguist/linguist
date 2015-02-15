def step_up
  start_position = $position
  step until ($position == start_position + 1)
end

# assumptions about the step function:
# - it maintains the current position of the robot "as a side effect"
# - the robot is equally likely to step back as to step up
def step
  if rand < 0.5
    $position -= 1
    p "fall (#$position)" if $DEBUG
    return false
  else
    $position += 1
    p "rise (#$position)" if $DEBUG
    return true
  end
end

$position = 0
step_up
