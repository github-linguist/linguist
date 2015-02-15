NDoors = Origin mimic

NDoors Toggle = Origin mimic do(
  initialize = method(toggled?, @toggled? = toggled?)
  toggle! = method(@toggled? = !toggled?. self)
)

NDoors Doors = Origin mimic do(
  initialize = method(n,
    @n = n
    @doors = {} addKeysAndValues(1..n, (1..n) map(_, NDoors Toggle mimic(false)))
  )
  numsToToggle = method(n, for(x <- (1..@n), (x % n) zero?, x))
  toggleThese = method(nums, nums each(x, @doors[x] = @doors at(x) toggle))
  show = method(@doors filter:dict(value toggled?) keys sort println)
)

; Test code
x = NDoors Doors mimic(100)
(1..100) each(n, x toggleThese(x numsToToggle(n)))
x show
