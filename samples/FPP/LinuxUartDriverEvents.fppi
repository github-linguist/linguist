@ UART open error
event OpenError(
                    device: string size 40 @< The device
                    error: I32 @< The error code
                    name: string size 40 @< error string
                  ) \
  severity warning high \
  id 0 \
  format "Error opening UART device {}: {} {}"

@ UART config error
event ConfigError(
                      device: string size 40 @< The device
                      error: I32 @< The error code
                    ) \
  severity warning high \
  id 1 \
  format "Error configuring UART device {}: {}"

@ UART write error
event WriteError(
                     device: string size 40 @< The device
                     error: I32 @< The error code
                   ) \
  severity warning high \
  id 2 \
  format "Error writing UART device {}: {}" \
  throttle 5

@ UART read error
event ReadError(
                    device: string size 40 @< The device
                    error: I32 @< The error code
                  ) \
  severity warning high \
  id 3 \
  format "Error reading UART device {}: {}" \
  throttle 5

@ UART port opened event
event PortOpened(
                     device: string size 40 @< The device
                   ) \
  severity activity high \
  id 4 \
  format "UART Device {} configured"

@ UART ran out of buffers
event NoBuffers(
                    device: string size 40 @< The device
                  ) \
  severity warning high \
  id 5 \
  format "UART Device {} ran out of buffers" \
  throttle 20

@ UART ran out of buffers
event BufferTooSmall(
                         device: string size 40 @< The device
                         $size: U32 @< The provided buffer size
                         needed: U32 @< The buffer size needed
                       ) \
  severity warning high \
  id 6 \
  format "UART Device {} target buffer too small. Size: {} Needs: {}"
