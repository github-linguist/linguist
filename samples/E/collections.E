? def constList := [1,2,3,4,5]
# value: [1, 2, 3, 4, 5]

? constList.with(6)
# value: [1, 2, 3, 4, 5, 6]

? def flexList := constList.diverge()
# value: [1, 2, 3, 4, 5].diverge()

? flexList.push(6)
? flexList
# value: [1, 2, 3, 4, 5, 6].diverge()

? constList
# value: [1, 2, 3, 4, 5]

? def constMap := [1 => 2, 3 => 4]
# value: [1 => 2, 3 => 4]

? constMap[1]
# value: 2

? def constSet := [1, 2, 3, 2].asSet()
# value: [1, 2, 3].asSet()

? constSet.contains(3)
# value: true
