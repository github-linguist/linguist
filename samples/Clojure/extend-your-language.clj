(defmacro if2 [[cond1 cond2] bothTrue firstTrue secondTrue else]
  `(let [cond1# ~cond1
         cond2# ~cond2]
     (if cond1# (if cond2# ~bothTrue   ~firstTrue)
                (if cond2# ~secondTrue ~else))))
