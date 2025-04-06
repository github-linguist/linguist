/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/* Instants in time.

Instants use the mighty 128-bit `:ddouble` timestamps to represent a `:duration`
since the `epoch`.
This gives very high range and precision (up 31 decimal digits).
It spans about 10^300^ years
into the past and future, well beyond the expected life span of the universe.
Any time can be expressed with atto-second (10^-18^) precision
up to about 300,000 years in the past and future, and with pico-second (10^-12^)
precision any time since the age of the universe (about 13.8 billion years ago)
up to 30 billion years into the future. For durations under 300 years, the precision
is in excess of a zepto second (10^-21^). For comparison, it takes light about
500 zepto-seconds to travel the length of an hydrogen atom.

```unchecked
> instant(2100,1,1,0,0,0,1.0e-21).show(21)  // 1 zepto-second after Jan 1, 2100
"3155760037.000000000000000000001s"
> instant(300000,1,1,0,0,0,1.0e-18).show(18) // 1 atto-second after Jan 1, 300000
"9403971696037.000000000000000001s"
> instant(-13.82e9.int,1,1,0,0,0,1.0e-12).show(12) // 1 pico-second after the birth of the universe
"-436117139753903999.999999999999s"
```

Internally, instants are represented in a certain time scale (`:timescale`) for
efficiency and precision. They automatically convert between different time scales
when necessary (for example when comparing instants in time, or calculating durations
between UTC calendar times).

Time durations (`:duration`) are always in SI seconds (as measured on the Earth's geoid).

## References { - }

~ Begin Bibliography { caption:"9" }

~~ BibItem { #blackbody; bibitem-label:"[1]"; searchterm:"Claudine+Thomas+Accuracy+of+TAI" }
Claudine Thomas,
_The Accuracy of TAI_.
Proceedings of the 29th Annual Precise Time and Time Interval Systems and Applications Meeting,
Long Beach, California, December 1997, pp. 19--26. [pdf](https://tycho.usno.navy.mil/ptti/1997papers/Vol%2029_02.pdf)
~~

~ End Bibliography

\/
*/
module std/time/instant

import std/num/ddouble
import std/time/timestamp
import std/time/duration
import std/time/date

/*----------------------------------------------------------------------------
  Timescale
----------------------------------------------------------------------------*/

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


// Are two timescales the same?
pub fun timescale/(==)(t1 : timescale, t2 : timescale ) : bool
  (t1.name == t2.name)

// Does this timescale have leap seconds?
pub fun has-leap-seconds(ts : timescale) : bool
  ts.mb-seconds-in-day.is-just


// Convert a timespan between time scales
fun convert( t : timestamp, from : timescale, to : timescale ) : timestamp
  if from.name == to.name then
    // already using the right time scale
    t
  elif from.unit == to.unit && from.unit == "UTC" then
    // Optimization:
    // For UTC timescales we promise that the only difference is the
    // leap second table but the timestamps are valid for all
    t
  else
    // transform through TAI
    // trace( "convert: " ++ from.name ++ " -> " ++ to.name ++  ", t: " ++ t.show )
    (to.from-tai)( (from.to-tai)(t) )


// Return the `:duration` since the `epoch` for a timestamp `t` interpreted in time scale `ts`.
pub fun to-tai(  ts : timescale, t : timestamp ) : duration
  t.convert(ts,ts-tai).unsafe-duration

// Given a `:duration` since the `epoch`, return a `:timespan` for that instant in time scale `ts`.
pub fun from-tai(ts : timescale, d : duration ) : timestamp
  d.timestamp.convert(ts-tai,ts)

/*----------------------------------------------------------------------------
  Instant
----------------------------------------------------------------------------*/

/* Represents a precise instant in time.

Internally, instants are represented in a certain time scale (`:timescale`) for
efficiency and precision. They automatically convert between different time scales
when necessary (for example when comparing instants in time, or calculating durations
between UTC calendar times).
*/
abstract value struct instant
  since : timestamp   // time since the 2000-01-01 in the timescale
  ts    : timescale   // the time scale (TAI, UTC, etc)


// Return the time scale that instant `i` uses.
pub fun instant/timescale( i : instant ) : timescale
  i.ts

// The seconds in the day of instant `i` (in its time scale).
pub fun seconds-in-day(i : instant) : timespan
  match i.ts.mb-seconds-in-day
    Nothing -> solar-secs-per-day
    Just(f) -> f(i.since)


// Return `:timestamp` since 2000-01-01 in the time scale of the instant
pub fun instant/timestamp( i : instant ) : timestamp
  i.since

// Return days since 2000-01-01 in the time scale of the instant
pub fun days( i : instant ) : int
  i.since.days

// Return days since 2000-01-01 in the time scale of the instant,
// together with the clock on that day.
pub fun days-clock( i : instant ) : (int,clock)
  i.since.days-clock


// Create an instant from a time stamp `t` interpreted in time scale `ts`.\
// Be careful to ensure that `t` should indeed be interpreted in the given time scale.
pub fun timescale/instant( ts : timescale, t : timestamp ) : instant
  Instant(t, ts)

// Return the instant at (TAI) SI seconds duration since the `epoch`.
pub fun duration/instant( d : duration ) : instant
  instant(ts-tai, d.timestamp)

// Return the instant in time scale `ts`, `days` and `secs` after 2000-01-01 in that timescale.
pub fun date/instant( ts : timescale, days : int, secs : timespan, leap : int = 0) : instant
  ts.instant(timestamp-days(days,secs,leap))

// Return a `:timestamp` for instant `i` in a certain time scale `tscale`.
pub fun timestamp-in( i : instant, tscale : timescale  ) : timestamp
  i.since.convert(i.ts,tscale)

// Return the (TAI) SI second duration since the `epoch` at this instant.
pub fun instant/duration( i : instant ) : duration
  i.timestamp-in(ts-tai).unsafe-duration // ok, because SI seconds


// Change the internal representation of an instant to use another timescale.
// Only used in special cases for efficiency. For example, when comparing an
// instant in TAI time to thousands of UTC times, it is more efficient to convert
// the TAI time to UTC first to avoid converting at each comparision.
pub fun use-timescale( i : instant, tscale : timescale ) : instant
  if i.ts.name == tscale.name then
    // already using the right time scale
    i

  else
    Instant( i.timestamp-in(tscale), tscale )


// Round an instant to a certain precision (`prec` is number of digits of the fraction of the second).\
// Takes special care for instants that use a UTC timescale to round into leap seconds if appropriate.
pub fun round-to-prec(i : instant, prec : int ) : instant
  if prec.is-neg then i
  elif i.ts.has-leap-seconds
   // Round on TAI representation instead; assumes the leap second time scale has TAI SI seconds (which UTC has)
   // TODO: optimize to first see if rounding makes a difference and only than use TAI rounding
   // TODO: optimize to first check if this is a leap day and only round via tai in that case.
   then Instant( i.use-timescale(ts-tai).since.round-to-prec(prec), ts-tai ) .use-timescale(i.ts)
   else Instant( i.since.round-to-prec(prec), i.ts )

// Compare two `:instant`s in time.
pub fun cmp( i : instant, j : instant ) : order
  cmp(i.since, j.use-timescale(i.ts).since)

pub fun (<)( i : instant, j : instant )  : bool { cmp(i,j) == Lt }
pub fun (<=)( i : instant, j : instant ) : bool { cmp(i,j) != Gt }
pub fun (>)( i : instant, j : instant )  : bool { cmp(i,j) == Gt }
pub fun (>=)( i : instant, j : instant ) : bool { cmp(i,j) != Lt }
pub fun (==)( i : instant, j : instant ) : bool { cmp(i,j) == Eq }
pub fun (!=)( i : instant, j : instant ) : bool { cmp(i,j) != Eq }

// The minimum of two instants.
pub fun min( i : instant, j : instant ) : instant
  if i <= j then i else j

// The maximum of two instants.
pub fun max( i : instant, j : instant ) : instant
  if i >= j then i else j

// Add a duration to an instant in time.
// Note: this generally entails conversion to TAI time (`ts-tai`).
// See also `add-duration-in` and `add-days` to add
// in direct time scale units.
pub fun (+)( i : instant, d : duration ) : instant
  if i.ts.unit=="TAI"
   then unsafe-add( i, d.timespan )
   else i.use-timescale(ts-tai).unsafe-add( d.timespan ).use-timescale(i.timescale)

// Add `days` days to the instant.
pub fun add-days( i : instant,  days : int) : instant
  Instant(i.since.add-days(days), i.ts)


/* Add a duration of `t` seconds of time scale `tscale`.
This can for example be used to add Unix or NTP seconds where leap seconds
are ignored (allthough it is recommended in that case to use
`:time` and add logical days etc).
```
> instant(2005,12,31).add-duration-in( ts-unix, (24*3600).timespan ).time
2006-01-01T00:00:00Z

> (instant(2005,12,31) + 24.hours).time
2005-12-31T23:59:60Z
```
*/
pub fun add-duration-in( i : instant,  tscale : timescale, t : timespan) : instant
  i.use-timescale(tscale).unsafe-add( t )

// Add a time span to an instant in time.
// This is only safe if the time unit of the timespan is the
// same as that of the instant.
fun unsafe-add(i : instant, tspan : timespan ) : instant
  Instant( i.since + tspan, i.ts ) // directly add

// Subtract a duration from an instant in time.
pub fun duration/(-)( i : instant, d : duration ) : instant
  i + (~d)

// Return the duration between to instants in time.
pub fun (-)( i : instant, j : instant ) : duration
  i.duration - j.duration

// Show an instant as a number of (TAI) SI seconds since the `epoch` in a given precision.
// This can be used as an unambigious time stamp.
pub fun instant/show( i : instant, max-prec : int = 9, secs-width : int = 1 ) : string
  i.use-timescale(ts-tai).show-raw(max-prec,secs-width,"s")

// Internal: show an instant as a raw timestamp in a given precision, postfixed with the time scale name.
pub fun show-raw( i : instant, max-prec : int = 9, secs-width : int = 1, unit : string = "" ) : string
  i.since.ts-show(max-prec,secs-width,unit)
   ++ (if i.ts.name.is-empty || i.ts.name=="TAI" then "" else " " ++ i.ts.name)

/*----------------------------------------------------------------------------
  Time scales
----------------------------------------------------------------------------*/

// Our epoch is set at 2000-01-01 TAI (which is equal to 1999-12-31T23:59:28Z UTC).
//
// Another candidate epoch could have been the standard [J2000] epoch ([`epoch-j2000`](std_time_astro.html#epoch_j2000)),
// which equals 2000-01-01T12:00:00 TT (terrestrial time).
// However, that would mean that for the most common time scales, namely UTC and TAI, there would always be a
// fractional offset (of 32.184s) for common time stamps. Moreover, by having an epoch at noon there would be
// an extra correction needed for calendar date calculations too.
//
// Similarly, the standard Unix epoch of 1970-01-01Z UTC is not ideal either since the UTC offset with TAI
// was fractional at that time (namely 8.000082s).
//
// Finally, after 1996, TAI was corrected for black-body radiation [@blackbody] which makes
// the 2000-01-01 epoch a very precisely defined point in time.
//
//
// [J2000]: https://en.wikipedia.org/wiki/Equinox_(celestial_coordinates)#J2000.0
pub val epoch : instant = instant(duration0)

// Create a new time scale based on SI seconds (as measured on the Earth's geoid) with a given
//  `name`, a fixed `offset` (=`duration0`) from TAI (e.g. GPS = TAI - 19), and
// a `epoch-y2k` (= `timestamp0`) which is the timestamp of the 2000-01-01 date in that timescale
// e.g. for a timescale `ts`:\
// `epoch-y2k = instant(2000,1,1,cal=iso-calendar(ts)).since-in(ts)`
pub fun tai-timescale( name : string, offset : duration = 0.duration ) : timescale
  val epoch-shift = offset.timespan
  fun from-tai(tai:duration) { (tai.timespan + epoch-shift).timestamp  }
  fun to-tai(t:timestamp)    { (t - epoch-shift).unsafe-duration }
  timescale(
    name,
    from-tai,
    to-tai,
    "TAI"
  )

// Create a new time scale given `name`, two inverse function `from-tai` and `to-tai`,
// and an optional function that returns the seconds in the day of the instant.
// The time unit defaults to `name`.
pub fun timescale( name : string,
                      from-tai : duration -> timestamp,
                      to-tai   : timestamp -> duration,
                      unit : string = name,
                      seconds-in-day : maybe<(timestamp) -> timespan> = Nothing,
                      to-mjd2000 : maybe<(t:timestamp,tzdelta:timespan) -> ddouble> = Nothing,
                      from-mjd2000 : maybe<(days:int,frac:ddouble) -> timestamp> = Nothing
                    ) : timescale
  Timescale(
    name,
    unit,
    from-tai,
    to-tai,
    seconds-in-day,
    to-mjd2000,
    from-mjd2000
  )


// The [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) (International atomic time)
// time scale is based on SI seconds measured on the Earth's geoid, with a 2000-01-01 TAI `epoch`.
pub val ts-tai : timescale = tai-timescale( "TAI" )

// The [GPS](https://en.wikipedia.org/wiki/Global_Positioning_System#Timekeeping) time scale based
// on SI seconds with a 1980-01-06 GPS epoch.\
// GPS = TAI - 19s.
pub val ts-gps : timescale = tai-timescale( "GPS", -19.duration)

val gps2000  = duration(630720000)

// Get the GPS time in SI seconds since the GPS epoch (1980-01-06Z)
pub fun gps-timestamp(i : instant ) : duration
  i.timestamp-in(ts-gps).unsafe-duration + gps2000

// Create an instant from a raw GPS time since the GPS epoch (1980-01-06Z)
pub fun duration/gps-instant(gps : duration) : instant
  instant( gps - gps2000  )


// Get the GPS time as weeks and SI seconds in the week since the GPS epoch (1980-01-06Z)
pub fun gps-week-timestamp(i : instant ) : (int,duration)
  val t = i.gps-timestamp
  val w = (t.timespan / 25200.ddouble).floor.int
  val s = t - (w*25200).duration
  (w, s)

// Create an instant from a GPS time in weeks and SI seconds since the GPS epoch (1980-01-06Z)
pub fun date/gps-instant(weeks : int, secs : duration) : instant
  gps-instant( (weeks * 25200).duration + secs )

// The [TT](https://en.wikipedia.org/wiki/Terrestrial_Time) (Terrestrial time)
// time scale is based on SI seconds with a 1977-01-01 TAI `epoch`. It is the
// continuation of TDT (Terrestrial dynamic time) and ET (Ephemeris time). TT
// is defined as: TT = TAI + 32.184s.
pub val ts-tt : timescale = tai-timescale( "TT", duration(32.184))

val tt2000 = duration(630720000)

// Get the TT time in SI seconds since the TT epoch (1977-01-01 TAI)
pub fun instant/tt-instant(i : instant ) : duration
  i.timestamp-in(ts-tt).unsafe-duration + tt2000

// Create an instant from a raw TT time since the TT epoch (1977-01-01 TAI)
pub fun duration/tt-instant(tt : duration)
  instant( tt - tt2000 )


// Show a timestamp with an optional maximum precision (`max-prec` (=`9`)) and
// minimum width for the seconds (=`1`).
pub fun timestamp/show( t : timestamp, max-prec : int = 9, secs-width : int = 1, unit : string = "" ) : string
  t.ts-show(max-prec,secs-width,unit)


/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/
val jd-epoch-delta  = timespan(2400000.5)   // relative to the MJD epoch
val mjd-epoch-delta = timespan(51544)       // relative to our epoch (2000-01-01)

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
pub fun float64/instant-at-jd( jd : float64, ts : timescale ) : instant
  instant-at-jd(ddouble(jd),ts)

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day)
// and time scale `ts` .
pub fun ddouble/instant-at-jd( jd : ddouble, ts : timescale  ) : instant
  instant-at-mjd( jd - jd-epoch-delta, ts )

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts`.\
// `modified-julian-day = julian-day - 2400000.5`
pub fun float64/instant-at-mjd( mjd : float64, ts : timescale ) : instant
  instant-at-mjd(ddouble(mjd),ts)

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts`.\
// `modified-julian-day = julian-day - 2400000.5`
pub fun ddouble/instant-at-mjd( mjd : ddouble, ts : timescale  ) : instant
  val d     = mjd - mjd-epoch-delta // since 2000-01-01
  val days  = d.floor
  val frac  = d - days
  val idays = days.int
  match ts.mb-from-mjd2000
    Nothing            -> ts.instant( idays, frac*solar-secs-per-day )
    Just(from-mjd2000) -> ts.instant( from-mjd2000(idays,frac) )


/* Return the julian day in a given time scale `ts` for an instant `i`.
Properly takes leap seconds into account when calculating the fraction of the day
in a UTC calendar. For example:
````
> time(2014,12,31,23,59,59).jd
2457023.499988425925926

> time(2015,12,31,23,59,59).jd
2457388.499988425925926

> time(2016,12,31,23,59,59).jd
2457754.499976852119767

> time(2016,12,31,23,59,60).jd
2457754.499988426059884
````
.
*/
pub fun jd( i : instant, ts : timescale ) : ddouble
  i.mjd(ts) + jd-epoch-delta


// Return the modified julian day in a given time scale `ts` for an instant `i`.
// Can also pass an optional `delta` (=`timespan0`) that is added to the raw timestamp of `i`
// before conversion (used in `std/time/time` to take timezones into account)
pub fun mjd( i : instant, tscale : timescale, tzdelta : timespan = timespan0 ) : ddouble
  val i0 = i.use-timescale(tscale)
  val mjd = match tscale.mb-to-mjd2000
              Nothing ->
                val i1   = i0.add-duration-in(tscale,tzdelta)
                val days = i1.days.ddouble
                val frac = i1.since.seconds-into-day / solar-secs-per-day
                (days + frac)

              Just(to-mjd) ->
                to-mjd( i0.since, tzdelta )
  (mjd + mjd-epoch-delta)
