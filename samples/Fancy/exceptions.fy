# define custom exception class
# StandardError is base class for all exception classes
class MyError : StandardError {
  def initialize: message {
    # forward to StdError's initialize method
    super initialize: message
  }
}

try {
  # raises/throws a new MyError exception within try-block
  MyError new: "my message" . raise!
} catch MyError => e {
  # catch exception
  # this will print "my message"
  e message println
} finally {
  # this will always be executed (as in e.g. Java)
  "This is how exception handling in Fancy works :)" println
}
