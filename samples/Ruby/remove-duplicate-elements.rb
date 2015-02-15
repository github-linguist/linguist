ary = [1,1,2,1,'redundant',[1,2,3],[1,2,3],'redundant']
uniq_ary = ary.uniq
# => [1, 2, "redundant", [1, 2, 3]]
