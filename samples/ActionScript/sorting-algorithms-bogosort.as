public function bogoSort(arr:Array):Array
{
    while (!sorted(arr))
    {
        shuffle(arr);
    }
	
    return arr;
}

public function shuffle(arr:Array):void
{
    for (var i:int = 0; i < arr.length; i++)
    {
        var rand:int = Math.floor(Math.random() * arr.length);
        var tmp:* = arr[i];
        arr[i] = arr[rand];
        arr[rand] = tmp;
    }
}

public function sorted(arr:Array):Boolean
{
    var last:int = arr[0];

    for (var i:int = 1; i < arr.length; i++)
    {
        if (arr[i] < last)
        {
            return false;
        }

        last = arr[i];
    }

    return true;
}
