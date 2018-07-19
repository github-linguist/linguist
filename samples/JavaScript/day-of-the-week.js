for (var year = 2008; year <= 2121; year++){
    var xmas = new Date(year, 11, 25)
    if ( xmas.getDay() === 0 )
        console.log(year)
}
