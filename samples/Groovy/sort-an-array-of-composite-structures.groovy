class Holiday {
    def date
    def name
    Holiday(dateStr, name) { this.name = name; this.date = format.parse(dateStr) }
    String toString() { "${format.format date}: ${name}" }
    static format = new java.text.SimpleDateFormat("yyyy-MM-dd")
}

def holidays = [ new Holiday("2009-12-25", "Christmas Day"),
                 new Holiday("2009-04-22", "Earth Day"),
                 new Holiday("2009-09-07", "Labor Day"),
                 new Holiday("2009-07-04", "Independence Day"),
                 new Holiday("2009-10-31", "Halloween"),
                 new Holiday("2009-05-25", "Memorial Day"),
                 new Holiday("2009-03-14", "PI Day"),
                 new Holiday("2009-01-01", "New Year's Day"),
                 new Holiday("2009-12-31", "New Year's Eve"),
                 new Holiday("2009-11-26", "Thanksgiving"),
                 new Holiday("2009-02-14", "St. Valentine's Day"),
                 new Holiday("2009-03-17", "St. Patrick's Day"),
                 new Holiday("2009-01-19", "Martin Luther King Day"),
                 new Holiday("2009-02-16", "President's Day") ]

holidays.sort { x, y -> x.date <=> y.date }
holidays.each { println it }
