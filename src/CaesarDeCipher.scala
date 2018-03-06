object CaesarDeCipher {
  val alphabet = ('a' to 'z').toList

  def encipher(s:String, n:Int):String = {
    var out:String = new String;

    s.toCharArray.toStream.foreach(a=> {
      var isUpper = a.isUpper
      if (alphabet.indexOf(a.toLower) != -1)
        out += getChar(isUpper, alphabet(((alphabet.indexOf(a.toLower) + (n))%(alphabet.length))))
      else out += a
    })

    def getChar(isUpper:Boolean, c:Char):Char = {
      if (isUpper) c.toUpper else c
    }
    out
  }

  val encipher1: (String, Int) => String = (s: String, n:Int) => {
    var out:String = new String;

    s.toCharArray.toStream.foreach(a=> {
      var isUpper = a.isUpper
      if (alphabet.indexOf(a.toLower) != -1)
        out += getChar(isUpper, alphabet(((alphabet.indexOf(a.toLower) + (n))%(alphabet.length))))
      else out += a
    })

    def getChar(isUpper:Boolean, c:Char):Char = {
      if (isUpper) c.toUpper else c
    }
    out
  }

  def decipher(s:String) {

    //def getDecypher
  }

  def main(args: Array[String]): Unit = {
    println(encipher1("Deep Purple", 1))
  }

}
