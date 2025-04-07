/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/*
THIS FILE WAS MODIFIED FROM ITS ORIGINAL SOURCE!
We removed everything except for the `abstract` type definitions!
*/

module std/time/instant

// A time scale defines how time is measured: the rate and unit of time,
// and how it can be converted to- and from TAI.\
// For time calculations, usually the [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time)
// (international atomic time) time scale (`ts-tai`) is used which is time measured as SI seconds on the Earths geoid.
// Another common time scale is UTC (`std/time/utc/ts-utc`) which also uses SI second time units but can contain leap seconds.
abstract struct timescale
  // Time scale name, "TAI", "UTC", "UNIX", "TT", etc.
  pub name : string
  // SI seconds: usually "TAI", but could be "TCG", "TCB", "TDB" etc.
  unit        : string
  // from duration since `epoch` to timestamp since epoch-y2k
  from-tai    : duration -> timestamp
  // from timestamp since timescale-epoch to duration since `epoch`
  to-tai      : timestamp -> duration
  // `Nothing` for fixed 86400s days, or
  // a function to return the seconds in the day of the given `:dayspan` (for leap seconds timescales only).
  mb-seconds-in-day : maybe<(t:timestamp) -> timespan> = Nothing
  mb-to-mjd2000     : maybe<(t:timestamp,tzdelta:timespan) -> ddouble> = Nothing
  mb-from-mjd2000   : maybe<(days:int,frac:ddouble) -> timestamp> = Nothing

/* Represents a precise instant in time.

Internally, instants are represented in a certain time scale (`:timescale`) for
efficiency and precision. They automatically convert between different time scales
when necessary (for example when comparing instants in time, or calculating durations
between UTC calendar times).
*/
abstract value struct instant
  since : timestamp   // time since the 2000-01-01 in the timescale
  ts    : timescale   // the time scale (TAI, UTC, etc)
