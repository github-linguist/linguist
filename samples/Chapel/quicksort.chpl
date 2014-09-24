//
// An example of a parallel quick sort implementation that uses
// "cobegin" to make each recursive call in parallel and "serial" to
// limit the number of threads.
//

use Random, Time; // for random number generation and the Timer class

var timer: Timer; // to time the sort

config var n: int = 2**15;      // the size of the array to be sorted
config var thresh: int = 1;     // the recursive depth to serialize
config var verbose: int = 0;    // print out this many elements in array
config var timing: bool = true; // set timing to false to disable timer

var A: [1..n] real; // array of real numbers

//
// initialize array with random numbers
//
fillRandom(A);

//
// print out front of array if verbose flag is set
//
if verbose > 0 then
  writeln("A[1..", verbose, "] = ", A[1..verbose]);

//
// start timer, call parallel quick sort routine, stop timer
//
if timing then timer.start();
pqsort(A, thresh);
if timing then timer.stop();

//
// report sort time
//
if timing then writeln("sorted in ", timer.elapsed(), " seconds");

//
// print out front of array if verbose flag is set
//   values should now be in sorted order
//
if verbose > 0 then
  writeln("A[1..", verbose, "] = ", A[1..verbose]);

//
// verify that array is sorted or halt
//
for i in 2..n do
  if A(i) < A(i-1) then
    halt("A(", i-1, ") == ", A(i-1), " > A(", i, ") == ", A(i));

writeln("verification success");

//
// pqsort -- parallel quick sort
//
//   arr: generic 1D array of values (real, int, ...)
//   thresh: number of recursive calls to make before serializing
//   low: lower bound of array to start sort at, defaults to whole array
//   high: upper bound of array to stop sort at, defaults to whole array
//
proc pqsort(arr: [],
           thresh: int,
           low: int = arr.domain.low,
           high: int = arr.domain.high) where arr.rank == 1 {

  //
  // base case: arr[low..high] is small enough to bubble sort
  //
  if high - low < 8 {
    bubbleSort(arr, low, high);
    return;
  }

  //
  // determine pivot and partition arr[low..high]
  //
  const pivotVal = findPivot();
  const pivotLoc = partition(pivotVal);

  //
  // make recursive calls to parallel quick sort each unsorted half of
  // the array; if thresh is 0 or less, start executing conquer tasks
  // serially
  //
  serial thresh <= 0 do cobegin {
    pqsort(arr, thresh-1, low, pivotLoc-1);
    pqsort(arr, thresh-1, pivotLoc+1, high);
  }

  //
  // findPivot -- helper routine to find pivot value using simple
  //              median-of-3 method, returns pivot value
  //
  proc findPivot() {
    const mid = low + (high-low+1) / 2;

    if arr(mid) < arr(low) then arr(mid) <=> arr(low);
    if arr(high) < arr(low) then arr(high) <=> arr(low);
    if arr(high) < arr(mid) then arr(high) <=> arr(mid);

    const pivotVal = arr(mid);
    arr(mid) = arr(high-1);
    arr(high-1) = pivotVal;

    return pivotVal;
  }

  //
  // partition -- helper routine to partition array such that all
  //              values less than pivot are to its left and all
  //              values greater than pivot are to its right, returns
  //              pivot location
  //
  proc partition(pivotVal) {
    var ilo = low, ihi = high-1;
    while (ilo < ihi) {
      do { ilo += 1; } while arr(ilo) < pivotVal;
      do { ihi -= 1; } while pivotVal < arr(ihi);
      if (ilo < ihi) {
        arr(ilo) <=> arr(ihi);
      }
    }
    arr(high-1) = arr(ilo);
    arr(ilo) = pivotVal;
    return ilo;
  }
}

//
// bubbleSort -- bubble sort for base case of quick sort
//
//   arr: generic 1D array of values (real, int, ...)
//   low: lower bound of array to start sort at
//   high: upper bound of array to stop sort at
//
proc bubbleSort(arr: [], low: int, high: int) where arr.rank == 1 {
  for i in low..high do
    for j in low..high-1 do
      if arr(j) > arr(j+1) then
        arr(j) <=> arr(j+1);
}
