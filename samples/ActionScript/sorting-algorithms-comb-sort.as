function combSort(input:Array)
{
	var gap:uint = input.length;
	var swapped:Boolean = false;
	while(gap > 1 || swapped)
	{
		gap /= 1.25;
		swapped = false;
		for(var i:uint = 0; i + gap < input.length; i++)
		{
			if(input[i] > input[i+gap])
			{
				var tmp = input[i];
				input[i] = input[i+gap];
				input[i+gap]=tmp;
				swapped = true;
			}
		}
	}
	return input;
}
