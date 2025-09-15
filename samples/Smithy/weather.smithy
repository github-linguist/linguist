$version: "2"
namespace example.weather

/// Provides weather forecasts.
service Weather {
    version: "2006-03-01"
    resources: [City]
}

resource City {
    identifiers: { cityId: CityId }
    read: GetCity
    list: ListCities
}

// "pattern" is a trait.
@pattern("^[A-Za-z0-9 ]+$")
string CityId
