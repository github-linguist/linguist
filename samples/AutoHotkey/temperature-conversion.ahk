MsgBox, % "Kelvin:`t`t 21.00 K`n"
        . "Celsius:`t`t" kelvinToCelsius(21) " C`n"
        . "Fahrenheit:`t" kelvinToFahrenheit(21) " F`n"
        . "Rankine:`t`t" kelvinToRankine(21) " R`n"

kelvinToCelsius(k)
{
    return, round(k - 273.15, 2)
}
kelvinToFahrenheit(k)
{
    return, round(k * 1.8 - 459.67, 2)
}
kelvinToRankine(k)
{
    return, round(k * 1.8, 2)
}
