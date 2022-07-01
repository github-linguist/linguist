options short circuit

import com
import fgl WebService_api

main
  
  call startServer()

end main

private function startServer() returns ()
  define
    returnCode integer

  call com.WebServiceEngine.RegisterRestService("WebService_api", "api")

  display "Server started"

  call com.WebServiceEngine.Start()

  while true
    let returnCode = com.WebServiceEngine.ProcessServices(-1)
    case returnCode
      when 0
        display "Request processed." 
      when -1
        display "Timeout reached."
      when -2
        display "Disconnected from application server."
        # the application server has closed the connection
        exit program
      when -3
        display "Client connection lost."
      when -4
        display "Server interrupted with ctrl-c."
      when -9
        display "Unsupported operation."
      when -10
        display "Internal server error."
      when -23
        display "Deserialization error."
      when -35
        display "No such REST operation found."
      when -36
        display "Missing REST parameter."
      otherwise 
        display "Unexpected server error " || returnCode || "."
      exit while 
    end case

    if int_flag != 0 then
      let int_flag = 0
      exit while
    end if     

  end while

  display "Server stopped"

end function