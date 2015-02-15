function pangram(k:string):Boolean {
  var lowerK:String = k.toLowerCase();
  var has:Object = {}

  for (var i:Number=0; i<=k.length-1; i++) {
    has[lowerK.charAt(i)] = true;
  }

  var result:Boolean = true;

  for (var ch:String='a'; ch <= 'z'; ch=String.fromCharCode(ch.charCodeAt(0)+1)) {
      result = result && has[ch]
  }

  return result || false;
}
