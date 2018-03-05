object CaesarDeCipher {

  def encipher(s:String, n:Int):String = {
    val alphabet = ('a' to 'z').toList
    var out:String = ""
    s.toCharArray.toStream.foreach(a=> if (alphabet.indexOf(a) != -1)  out += alphabet(((alphabet.indexOf(a) + (n))%(alphabet.length))) else out += a)
    return out
  }

  def main(args: Array[String]): Unit = {
    print(encipher("aaa aaa zzz zzz xxx xxx yyy yyy frank zappa 1", 1))
  }

}
