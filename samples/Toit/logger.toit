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

import certificate_roots
import gpio
import monitor
import uart
import supabase

// After 5s offload the data. Even if the device is not quiet.
MAX_OFFLOAD_DELAY ::= Duration --s=5
// Offload if we accumulate more than 2kb of data.
MAX_BUFFERED_DATA ::= 2000
// Offload if we have not received any data for 500ms.
MAX_QUIET_FOR_OFFLOAD ::= Duration --ms=500

LOGS_TABLE ::= "logs"

class LogForwarder:
  pin_/gpio.Pin
  port_/uart.Port
  buffered_/List := [] // Of ByteArray.
  buffered_size_/int := 0
  offload_task_/Task? := null
  upload_/Lambda

  constructor pin_number/int --upload/Lambda:
    pin_ = gpio.Pin pin_number
    port_ = uart.Port --rx=pin_ --tx=null --baud_rate=115200
    upload_ = upload
    offload_task_ = task::
      offload_

  close:
    if offload_task_:
      offload_task_.cancel
      port_.close
      pin_.close
      offload_task_ = null

  listen:
    while true:
      chunk := port_.read
      buffer_ chunk

  buffer_ data/ByteArray:
    print "Received $data.to_string_non_throwing"
    buffered_.add data
    buffered_size_ += data.size
    if buffered_size_ > MAX_BUFFERED_DATA:
      offload_

  offload_:
    last_offload := Time.now
    while true:
      last_message := Time.now
      old_size := buffered_size_
      while (Duration.since last_message) < MAX_QUIET_FOR_OFFLOAD:
        sleep --ms=20

        if buffered_size_ == 0:
          // Reset the timer.
          last_offload = Time.now
          last_message = last_offload
          continue

        if (Duration.since last_offload) > MAX_OFFLOAD_DELAY:
          break

        if buffered_size_ == old_size:
          continue

        if buffered_size_ > MAX_BUFFERED_DATA:
          print "too much data"
          break

        last_message = Time.now
        old_size = buffered_size_

      print "Offloading"
      total := ByteArray buffered_size_
      offset := 0
      buffered_.do:
        total.replace offset it
        offset += it.size
      to_upload := total.to_string_non_throwing
      buffered_.clear
      buffered_size_ = 0
      print "Uploading: $to_upload"
      upload_.call to_upload

main
    --supabase_project/string
    --supabase_anon/string
    --device_id/string
    --pin_rx1/int
    --pin_rx2/int?:

  client/supabase.Client? := null
  forwarder1/LogForwarder? := null
  forwarder2/LogForwarder? := null

  while true:
    // Trying to work around https://github.com/toitlang/pkg-http/issues/89
    catch --trace:
      client = supabase.Client.tls
          --host="$(supabase_project).supabase.co"
          --anon=supabase_anon
          --root_certificates=[certificate_roots.BALTIMORE_CYBERTRUST_ROOT]

      mutex := monitor.Mutex

      offload := :: | uart_pin/int data/string |
        mutex.do:
          client.rest.insert --no-return_inserted LOGS_TABLE {
            "device_id": device_id,
            "uart_pin": uart_pin,
            "data": data,
          }

      if pin_rx2:
        task::
          forwarder2 = LogForwarder pin_rx2 --upload=:: | data/string |
            offload.call pin_rx2 data
          forwarder2.listen

      forwarder1 = LogForwarder pin_rx1 --upload=:: | data/string |
        offload.call pin_rx1 data
      forwarder1.listen

    if forwarder1: forwarder1.close
    if forwarder2: forwarder2.close
    if client: client.close
