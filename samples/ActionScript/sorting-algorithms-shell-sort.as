function shellSort(data:Array):Array
{
	var inc:uint = data.length/2;
	while(inc > 0)
	{
		for(var i:uint = inc; i< data.length; i++)
		{
			var tmp:Object = data[i];
			for(var j:uint = i; j >= inc && data[j-inc] > tmp; j -=inc)
			{
				data[j] = data[j-inc];
			}
			data[j] = tmp;
		}
		inc = Math.round(inc/2.2);
	}
	return data;
}
