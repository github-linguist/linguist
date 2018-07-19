var A:Node;
//...
for(var i:Node = A; i != null; i = i.link)
{
	doStuff(i);
}
