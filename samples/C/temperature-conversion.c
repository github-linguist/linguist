#include <stdio.h>
#include <stdlib.h>

double kelvinToCelsius(double k){
    return k - 273.15;
}

double kelvinToFahrenheit(double k){
    return k * 1.8 - 459.67;
}

double kelvinToRankine(double k){
    return k * 1.8;
}
void convertKelvin(double kelvin) {
    printf("K %.2f\n", kelvin);
    printf("C %.2f\n", kelvinToCelsius(kelvin));
    printf("F %.2f\n", kelvinToFahrenheit(kelvin));
    printf("R %.2f", kelvinToRankine(kelvin));
}

int main(int argc, const char * argv[])
{
    if (argc > 1) {
        double kelvin = atof(argv[1]);
        convertKelvin(kelvin);
    }
    return 0;
}
