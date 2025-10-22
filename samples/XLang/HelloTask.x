# Import the 'cantor' module using the 'lrpc:1000' protocol for communication
import cantor thru 'lrpc:1000'

# Create an instance of the Host class from the cantor module
host = cantor.Host()

# Retrieve the name of the current node (local node where this script is running)
nodeName = host.NodeName

# Define a task using the @cantor.Task decorator
# This task will run asynchronously on a remote node where the condition ThinkPad == 1 is satisfied
@cantor.Task(ThinkPad==1)
def hello_task(info):
    # Print the received input on the remote node's console
    print("from task:", info)
    # Show an alert with the received information on the remote node
    alert(info)
    # Return a response message as the task's result
    return "Thanks"

# Run the hello_task on a remote node asynchronously
# The input string includes a greeting and the name of the local node
f = hello_task.run("Hello From:" + nodeName)

# 'f' is a future object that represents the result of the asynchronous task execution
# The future allows the local node to continue processing without blocking while the task runs remotely

# Retrieve the result of the remote task execution
# This blocks until the remote node completes the task and sends back the result
binRet = f.get()

# Convert the binary result (received from the remote node) into a human-readable value
# Implementation of fromBytes is assumed to handle this conversion
ret_value = fromBytes(binRet)

# Print the final status or result locally
# This print happens on the local machine, not on the remote node
print("end")
