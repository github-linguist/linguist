class Caesar {
  int _key;

  Caesar(this._key);

  int _toCharCode(String s) {
    return s.charCodeAt(0);
  }

  String _fromCharCode(int ch) {
    return new String.fromCharCodes([ch]);
  }

  String _process(String msg, int offset) {
    StringBuffer sb=new StringBuffer();
    for(int i=0;i<msg.length;i++) {
      int ch=msg.charCodeAt(i);
      if(ch>=_toCharCode('A')&&ch<=_toCharCode('Z')) {
        sb.add(_fromCharCode(_toCharCode("A")+(ch-_toCharCode("A")+offset)%26));
      }
      else if(ch>=_toCharCode('a')&&ch<=_toCharCode('z')) {
        sb.add(_fromCharCode(_toCharCode("a")+(ch-_toCharCode("a")+offset)%26));
      } else {
        sb.add(msg[i]);
      }
    }
    return sb.toString();
  }

  String encrypt(String msg) {
    return _process(msg, _key);
  }

  String decrypt(String msg) {
    return _process(msg, 26-_key);
   }
}

void trip(String msg) {
  Caesar cipher=new Caesar(10);

  String enc=cipher.encrypt(msg);
  String dec=cipher.decrypt(enc);
  print("\"$msg\" encrypts to:");
  print("\"$enc\" decrypts to:");
  print("\"$dec\"");
  Expect.equals(msg,dec);
}

main() {
  Caesar c2=new Caesar(2);
  print(c2.encrypt("HI"));
  Caesar c20=new Caesar(20);
  print(c20.encrypt("HI"));

  // try a few roundtrips

  trip("");
  trip("A");
  trip("z");
  trip("Caesar cipher");
  trip(".-:/\"\\!");
  trip("The Quick Brown Fox Jumps Over The Lazy Dog.");
}
