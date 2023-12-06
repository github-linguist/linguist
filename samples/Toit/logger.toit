// Copyright (C) 2023 Florian Loitsch
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import certificate-roots
import gpio
import monitor
import uart
import supabase

// After 5s offload the data. Even if the device is not quiet.
MAX-OFFLOAD-DELAY ::= Duration --s=5
// Offload if we accumulate more than 2kb of data.
MAX-BUFFERED-DATA ::= 2000
// Offload if we have not received any data for 500ms.
MAX-QUIET-FOR-OFFLOAD ::= Duration --ms=500

LOGS-TABLE ::= "logs"

class LogForwarder:
  pin_/gpio.Pin
  port_/uart.Port
  buffered_/List := [] // Of ByteArray.
  buffered-size_/int := 0
  offload-task_/Task? := null
  upload_/Lambda

  constructor pin-number/int --upload/Lambda:
    pin_ = gpio.Pin pin-number
    port_ = uart.Port --rx=pin_ --tx=null --baud-rate=115200
    upload_ = upload
    offload-task_ = task::
      offload_

  close:
    if offload-task_:
      offload-task_.cancel
      port_.close
      pin_.close
      offload-task_ = null

  listen:
    while true:
      chunk := port_.read
      buffer_ chunk

  buffer_ data/ByteArray:
    print "Received $data.to-string-non-throwing"
    buffered_.add data
    buffered-size_ += data.size
    if buffered-size_ > MAX-BUFFERED-DATA:
      offload_

  offload_:
    last-offload := Time.now
    while true:
      last-message := Time.now
      old-size := buffered-size_
      while (Duration.since last-message) < MAX-QUIET-FOR-OFFLOAD:
        sleep --ms=20

        if buffered-size_ == 0:
          // Reset the timer.
          last-offload = Time.now
          last-message = last-offload
          continue

        if (Duration.since last-offload) > MAX-OFFLOAD-DELAY:
          break

        if buffered-size_ == old-size:
          continue

        if buffered-size_ > MAX-BUFFERED-DATA:
          print "too much data"
          break

        last-message = Time.now
        old-size = buffered-size_

      print "Offloading"
      total := ByteArray buffered-size_
      offset := 0
      buffered_.do:
        total.replace offset it
        offset += it.size
      to-upload := total.to-string-non-throwing
      buffered_.clear
      buffered-size_ = 0
      print "Uploading: $to-upload"
      upload_.call to-upload

main
    --supabase-project/string
    --supabase-anon/string
    --device-id/string
    --pin-rx1/int
    --pin-rx2/int?:

  client/supabase.Client? := null
  forwarder1/LogForwarder? := null
  forwarder2/LogForwarder? := null

  while true:
    // Trying to work around https://github.com/toitlang/pkg-http/issues/89
    catch --trace:
      client = supabase.Client.tls
          --host="$(supabase-project).supabase.co"
          --anon=supabase-anon
          --root-certificates=[certificate-roots.BALTIMORE-CYBERTRUST-ROOT]

      mutex := monitor.Mutex

      offload := :: | uart-pin/int data/string |
        mutex.do:
          client.rest.insert --no-return-inserted LOGS-TABLE {
            "device_id": device-id,
            "uart_pin": uart-pin,
            "data": data,
          }

      if pin-rx2:
        task::
          forwarder2 = LogForwarder pin-rx2 --upload=:: | data/string |
            offload.call pin-rx2 data
          forwarder2.listen

      forwarder1 = LogForwarder pin-rx1 --upload=:: | data/string |
        offload.call pin-rx1 data
      forwarder1.listen

    if forwarder1: forwarder1.close
    if forwarder2: forwarder2.close
    if client: client.close
