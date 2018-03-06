class CaesarDeCipher {

  private def getDecodedString(s:String, n:Int):String = {
    val alphabet = ('a' to 'z').toList
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

  private def decode(f:(String, Int) => String, s:String, n:Int):String = f(s, n)

  def encipher(s:String, n:Int): String = {
    decode(getDecodedString, s, n)
  }

  def decipher(s:String) {
    for (i <- 1 to 25) getScore(decode(getDecodedString, s, i))

    def getScore (in:String): Unit ={
      println(in)
    }
  }

  def main(args: Array[String]): Unit = {
    println(encipher("Deep Purple", 1))
    println(decipher("Pqqb Bgdbxq"))
  }

}

/* backup
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
 */