object CaesarDeCipher {
  val alphabet = ('a' to 'z').toList

  def encipher(s:String, n:Int):String = {
    var out:String = new String
    s.toCharArray.toStream.foreach(a=> if (alphabet.indexOf(a) != -1)  out += alphabet(((alphabet.indexOf(a) + (n))%(alphabet.length))) else out += a)
    out
  }

  val encipher1: (String, Int) => String = (s: String, n:Int) => {
    var out:String = new String;
    s.toCharArray.toStream.foreach(a=> if (alphabet.indexOf(a) != -1)  out += alphabet(((alphabet.indexOf(a) + (n))%(alphabet.length))) else out += a)
    out
  }

  def main(args: Array[String]): Unit = {
    println(encipher("aaa aaa zzz zzz xxx xxx yyy yyy frank zappa 1", 1))
    println(encipher1("aaa aaa zzz zzz xxx xxx yyy yyy frank zappa 1", 1))
  }

}
