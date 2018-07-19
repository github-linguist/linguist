use Acme::AGMorse qw(SetMorseVals SendMorseMsg);
SetMorseVals(20,30,400);
SendMorseMsg('Hello World! abcdefg @\;');  # note, caps are ingnored in Morse Code
exit;
