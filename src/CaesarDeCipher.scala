object CaesarDeCipher {

  private def getDecodedString(s:String, n:Int):String = {
    val alphabet = ('a' to 'z').toList
    var out:String = new String

    s.toCharArray.toStream.foreach(a=> {
      var isUpper = a.isUpper
      if (alphabet.indexOf(a.toLower) != -1)
        out += getChar(isUpper, alphabet((alphabet.indexOf(a.toLower) + n)%alphabet.length))
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
    var score:Double = 0
    for (i <- 1 to 25) getScore(decode(getDecodedString, s, i))

    def getScore (in:String): Unit ={
      var x:Double = testChi.evaluate(in)
      if (x < score) score = x
      if (x<5) println(x + " " + in)
    }
  }

  def main(args: Array[String]): Unit = {
    println(encipher("goodbye bule sky", 13))
    println(decipher("tbbqolr ohyr fxl"))
  }

}
