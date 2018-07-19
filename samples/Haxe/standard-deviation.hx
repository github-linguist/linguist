using Lambda;

class Main {
	static function main():Void {
		var nums = [2, 4, 4, 4, 5, 5, 7, 9];
		for (i in 1...nums.length+1)			
			Sys.println(sdev(nums.slice(0, i)));
	}
	
	static function average<T:Float>(nums:Array<T>):Float {
		return nums.fold(function(n, t) return n + t, 0) / nums.length;
	}

	static function sdev<T:Float>(nums:Array<T>):Float {
		var store = [];
		var avg = average(nums);
		for (n in nums) {
			store.push((n - avg) * (n - avg));
		}
		
		return Math.sqrt(average(store));
	}
}
