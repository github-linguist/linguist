function distance(p1, p2) {
  var dx = Math.abs(p1.x - p2.x);
  var dy = Math.abs(p1.y - p2.y);
  return Math.sqrt(dx*dx + dy*dy);
}

function bruteforceClosestPair(arr) {
  if (arr.length < 2) {
    return Infinity;
  } else {
    var minDist = distance(arr[0], arr[1]);
    var minPoints = arr.slice(0, 2);

    for (var i=0; i<arr.length-1; i++) {
      for (var j=i+1; j<arr.length; j++) {
        if (distance(arr[i], arr[j]) < minDist) {
          minDist = distance(arr[i], arr[j]);
          minPoints = [ arr[i], arr[j] ];
        }
      }
    }
    return {
      distance: minDist,
      points: minPoints
    };
  }
}
