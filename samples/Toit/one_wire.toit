// Copyright (C) 2023 Toitware ApS
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
// REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
// INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
// LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
// OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
// PERFORMANCE OF THIS SOFTWARE.

import gpio
import one-wire
import one-wire.family as one-wire

DATA-PIN ::= 32

main:
  bus := one-wire.Bus (gpio.Pin 32)
  print "Listing all devices on bus:"
  bus.do:
    family-id := one-wire.family-id --device-id=it
    print "  $(%x it): $(one-wire.family-to-string family-id)"

  print "Listing only ds18b20 devices on bus:"
  // Only list ds18b20 devices.
  bus.do --family=one-wire.FAMILY-DS18B20:
    print "  $(%x it)"

  print "Demonstrating how to skip families."
  // Skip families.
  bus.do:
    family-id := one-wire.family-id --device-id=it
    print "  Got called with id: $(%x it) - $(one-wire.family-to-string family-id)"
    if family-id == one-wire.FAMILY-DS18B20:
      print "    Skipping remaining devices of this family."
      continue.do one-wire.Bus.SKIP-FAMILY
