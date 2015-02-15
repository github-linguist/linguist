function heapSort(data:Vector.<int>):Vector.<int> {
	for (var start:int = (data.length-2)/2; start >= 0; start--) {
		siftDown(data, start, data.length);
	}
	for (var end:int = data.length - 1; end > 0; end--) {
		var tmp:int=data[0];
		data[0]=data[end];
		data[end]=tmp;
		siftDown(data, 0, end);
	}
	return data;
}
function siftDown(data:Vector.<int>, start:int, end:int):void {
	var heapRoot:int=start;
	while (heapRoot * 2+1 < end) {
		var child:int=heapRoot*2+1;
		if (child+1<end&&data[child]<data[child+1]) {
			child++;
		}
		if (data[heapRoot]<data[child]) {
			var tmp:int=data[heapRoot];
			data[heapRoot]=data[child];
			data[child]=tmp;
			heapRoot=child;
		} else {
			return;
		}
	}
}
