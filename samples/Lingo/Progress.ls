global numTasks, finishedTasks, numSteps, finishedSteps

on setTasks num
  set ongoingTask = numTasks > 0
  set numTasks = num + ongoingTask
  set finishedTasks = 0
end

on removeTasks num
  set numTasks = max(numTasks - num, 1)
end

on finishTask
  if finishedTasks < numTasks then
    set finishedTasks = finishedTasks + 1
    setSteps(0)
  end if
end

on setSteps num
  set numSteps = num
  set finishedSteps = 0
end

on finishStep
  if finishedSteps < numSteps then set finishedSteps = finishedSteps + 1
end

on getProgress
  set taskProgress = float(finishedSteps) / max(numSteps, 1)
  return (finishedTasks + taskProgress) / max(numTasks, 1)
end

on resetProgress
  set numTasks = 0
  set finishedTasks = 0
  set numSteps = 0
  set finishedSteps = 0
end

on logProgress
  logMsg(finishedTasks && "/" && numTasks && "tasks," && finishedSteps && "/" && numSteps && "steps")
end