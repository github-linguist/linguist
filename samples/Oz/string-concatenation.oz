declare
S = "hello"
{System.showInfo S#" literal"} %% virtual strings are constructed with "#"
S1 = {Append S " literal"}
{System.showInfo S1}
