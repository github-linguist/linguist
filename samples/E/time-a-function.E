def countTo(x) {
	println("Counting...")
	for _ in 1..x {}
	println("Done!")
}

def MX := <unsafe:java.lang.management.makeManagementFactory>
def threadMX := MX.getThreadMXBean()
require(threadMX.isCurrentThreadCpuTimeSupported())
threadMX.setThreadCpuTimeEnabled(true)

for count in [10000, 100000] {
	def start := threadMX.getCurrentThreadCpuTime()
	countTo(count)
	def finish := threadMX.getCurrentThreadCpuTime()
	println(`Counting to $count takes ${(finish-start)//1000000}ms`)
}
