ColorCodes := DATASET([ {'Black', 0 },
                        {'Brown', 1 },
                        {'Red', 2 },
                        {'Orange', 3 },
                        {'Yellow', 4 },
                        {'Green', 5 },
                        {'Blue', 6 },
                        {'Violet', 7 },
                        {'Grey', 8 },
                        {'White', 9 }], {STRING color, UNSIGNED1 code});

ColorCodesDCT := DICTIONARY(ColorCodes, {Color, Code});     //multi-field key
ColorCodeDCT  := DICTIONARY(ColorCodes, {Color => Code});   //payload field
CodeColorDCT  := DICTIONARY(ColorCodes, {Code => Color});

ColorCodesDCT;
ColorCodeDCT;
CodeColorDCT;

//mapping examples
MapCode2Color(UNSIGNED1 code)   := CodeColorDCT[code].color;
MapColor2Code(STRING color)     := ColorCodeDCT[color].code;

OUTPUT(MapColor2Code('Red'));   //2
OUTPUT(MapCode2Color(4));       //yellow

//Search term examples
OUTPUT('Green' IN ColorCodeDCT);    //true
OUTPUT(6 IN CodeColorDCT);          //true
OUTPUT(ROW({'Red', 2}, RECORDOF(ColorCodes)) IN ColorCodesDCT); //multi-field key, true 
