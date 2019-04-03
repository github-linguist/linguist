ROUTINE TestCoverage [Type=INC]
#define StartTimer(%msg) If (..Display["log") { Write !,%msg,": " Set tStartTime = $zh }
#define StopTimer If (..Display["log") { Write ($zh-tStartTime)," seconds" }
#define METRICS "RtnLine","Time","TotalTime"
#define TestPathAllTests "all tests"
