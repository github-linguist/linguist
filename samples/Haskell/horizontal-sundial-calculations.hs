module Rosetta.HorSunDial where

roundDec :: Int -> Double -> Double
roundDec d = (/10.0^d). fromIntegral. round. (*10.0^d)

radToDegr = ((180/pi)*)
degrToRad = ((pi/180)*)

main = do
  let lat        = -4.95
      long       = -150.5
      legalMerid = -150
      sinOfLat   = sin $ degrToRad lat
      diff       = legalMerid - long

  putStrLn $ "Latitude         " ++ show lat
  putStrLn $ "Longitude        " ++ show long
  putStrLn $ "Legal meridian   " ++ show legalMerid
  putStrLn $ "Sine of latitude " ++ show (roundDec 6 sinOfLat)
  putStrLn $ "Diff longitude   " ++ show (-diff)
  putStrLn "hour   sun hour angle   dial hour  line angle"
  mapM_ (\h ->
	  let sha  = diff + 15*h
	      dhla = radToDegr . atan. (sinOfLat *). tan $ degrToRad sha
	  in  putStrLn $ take 7 (show h ++ repeat ' ')
			    ++ take 16 (show (roundDec 3 sha) ++ repeat ' ' )
			    ++ " " ++ show (roundDec 3 dhla)
	  ) [-6,-5..6]
