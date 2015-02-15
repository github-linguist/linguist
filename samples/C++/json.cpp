#include "Core/Core.h"

using namespace Upp;

CONSOLE_APP_MAIN
{
	JsonArray a;
	a << Json("name", "John")("phone", "1234567") << Json("name", "Susan")("phone", "654321");
	String txt = ~a;
	Cout() << txt << '\n';
	Value v = ParseJSON(txt);
	for(int i = 0; i < v.GetCount(); i++)
		Cout() << v[i]["name"] << ' ' << v[i]["phone"] << '\n';
}
