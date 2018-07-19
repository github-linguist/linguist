public static void sort(int[] nums){
	for(int currentPlace = 0;currentPlace<nums.length-1;currentPlace++){
		int smallest = Integer.MAX_VALUE;
		int smallestAt = currentPlace+1;
		for(int check = currentPlace; check<nums.length;check++){
			if(nums[check]<smallest){
				smallestAt = check;
				smallest = nums[check];
			}
		}
		int temp = nums[currentPlace];
		nums[currentPlace] = nums[smallestAt];
		nums[smallestAt] = temp;
	}
}
