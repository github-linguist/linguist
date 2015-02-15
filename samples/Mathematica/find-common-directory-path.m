FindCommonDirectory[x_] := If[StringTake[#, -1] != "/", StringTake[#, Max[StringPosition[#, "/"]]], #] &
 [Fold[LongestCommonSubsequence, First[x] , Rest[x]]]

FindCommonDirectory[{"/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members"}]
->"/home/user1/tmp/"
