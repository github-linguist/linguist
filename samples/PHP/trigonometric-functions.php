$radians = M_PI / 4;
$degrees = 45 * M_PI / 180;
echo sin($radians) . " " . sin($degrees);
echo cos($radians) . " " . cos($degrees);
echo tan($radians) . " " . tan($degrees);
echo asin(sin($radians)) . " " . asin(sin($radians)) * 180 / M_PI;
echo acos(cos($radians)) . " " . acos(cos($radians)) * 180 / M_PI;
echo atan(tan($radians)) . " " . atan(tan($radians)) * 180 / M_PI;
