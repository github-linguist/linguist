% Implementer by Arjun Sunel
-module(haversine).
-export([main/0]).

main() ->
	haversine(36.12, -86.67, 33.94, -118.40).

haversine(Lat1, Long1, Lat2, Long2) ->
	V 	         =   math:pi()/180,
	R 		 =   6372.8, 	% In kilometers
	Diff_Lat 	 =   (Lat2 - Lat1)*V ,	
	Diff_Long	 =   (Long2 - Long1)*V,	
	NLat 		 =   Lat1*V,
	NLong 		 =   Lat2*V,
	A 		 =   math:sin(Diff_Lat/2) * math:sin(Diff_Lat/2) + math:sin(Diff_Long/2) * math:sin(Diff_Long/2) * math:cos(NLat) * math:cos(NLong),
	C 		 =   2 * math:asin(math:sqrt(A)),
	R*C.
