open Parse.String

val digit = satisfy isdigit

val decimal_of_len n =
    ds <- count n digit;
    return (List.foldl (fn d acc => 10*acc + ((ord d)-(ord #"0"))) 0 ds)

val date =
    y <- decimal_of_len 4;
    char' #"-";
    m <- decimal_of_len 2;
    char' #"-";
    d <- decimal_of_len 2;
    if m > 0 && m <= 12 then
        return {Year=y, Month=(Datetime.intToMonth (m-1)), Day=d}
    else
        fail

(* We parse fractions of a second, but ignore them since Datetime
   doesn't permit representing them. *)
val time =
    h <- decimal_of_len 2;
    char' #":";
    m <- decimal_of_len 2;
    s <- maybe (char' #":";
                s <- decimal_of_len 2;
                maybe' (char' #"."; skipWhile isdigit);
                return s);
    return {Hour=h, Minute=m, Second=Option.get 0 s}

val timezone_offset =
    let val zulu = char' #"Z"; return 0
        val digits = decimal_of_len 2
        val sign = or (char' #"+"; return 1)
                      (char' #"-"; return (-1))
    in
        zulu `or` (s <- sign;
                   h <- digits;
                   m <- (maybe' (char' #":"); or digits (return 0));
                   return (s*(h*60+m)))
    end

val datetime_with_tz =
    d <- date; char' #"T"; t <- time;
    tz <- timezone_offset;
    return (d ++ t ++ {TZOffsetMinutes=tz})

val datetime =
    d <- datetime_with_tz;
    return (d -- #TZOffsetMinutes)

fun process v =
    case parse (d <- datetime_with_tz; eof; return d) v of
        Some r =>
        let
            val {Year=year,Month=month,Day=day,
                 Hour=hour,Minute=minute,Second=second} =
                Datetime.addMinutes (r.TZOffsetMinutes) (r -- #TZOffsetMinutes)
            fun pad x =
                if x < 10 then "0" `strcat` show x else show x
        in
            <xml>{[pad hour]}:{[pad minute]}:{[pad second]} {[month]} {[day]}, {[year]}</xml>
        end
      | None => <xml>none</xml>

fun main () : transaction page =
    input <- source "2012-01-01T01:10:42Z";
    return <xml>
      <body>
        <label>
          Enter an
          <a href="https://en.wikipedia.org/wiki/ISO_8601">ISO 8601</a>
          datetime here:
          <ctextbox source={input} />
        </label>
        <ul><dyn signal={v <- signal input; return (process v)} /></ul>
      </body>
    </xml>
