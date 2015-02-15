def MD5( s:String ) : String = {
  // Besides "MD5", "SHA-256", and other hashes are available
  val m = java.security.MessageDigest.getInstance("MD5").digest(s.getBytes("UTF-8"));
  m map {c => (c & 0xff) toHexString} mkString
}

MD5("The quick brown fox jumped over the lazy dog's back")
