function transpose( m:Array):Array
{
	//Assume each element in m is an array. (If this were production code, use typeof to be sure)

	//Each element in m is a row, so this gets the length of a row in m,
	//which is the same as the number of rows in m transpose.
	var mTranspose = new Array(m[0].length);
	for(var i:uint = 0; i < mTranspose.length; i++)
	{
                //create a row
		mTranspose[i] = new Array(m.length);
                //set the row to the appropriate values
		for(var j:uint = 0; j < mTranspose[i].length; j++)
			mTranspose[i][j] = m[j][i];
	}
	return mTranspose;
}
var m:Array = [[1, 2, 3, 10],
	       [4, 5, 6, 11],
	       [7, 8, 9, 12]];
var M:Array = transpose(m);
for(var i:uint = 0; i < M.length; i++)
	trace(M[i]);
