//Comparison function must returns Numbers even though it deals with integers.
function compare(x:int, y:int):Number
{
	return Number(x-y);
}
var nums:Vector.<int> = Vector.<int>([5,12,3,612,31,523,1,234,2]);
nums.sort(compare);
