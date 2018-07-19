+ (double) distanceBetweenLat1:(double)lat1 lon1:(double)lon1
                          lat2:(double)lat2 lon2:(double)lon2 {
    //degrees to radians
    double lat1rad = lat1 * M_PI/180;
    double lon1rad = lon1 * M_PI/180;
    double lat2rad = lat2 * M_PI/180;
    double lon2rad = lon2 * M_PI/180;

    //deltas
    double dLat = lat2rad - lat1rad;
    double dLon = lon2rad - lon1rad;

    double a = sin(dLat/2) * sin(dLat/2) + sin(dLon/2) * sin(dLon/2) * cos(lat1rad) * cos(lat2rad);
    double c = 2 * asin(sqrt(a));
    double R = 6372.8;
    return R * c;
}
