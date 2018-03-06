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

  def decipher(s:String):String = {
    var out:String = decode(getDecodedString, s, 1)
    var score:BigDecimal = 100D
    score = getScore(decode(getDecodedString, s, 1))

    for (i <- 2 to 25) {
      if (getScore(decode(getDecodedString, s, i))<score) {
        score = getScore(decode(getDecodedString, s, i))
        out = decode(getDecodedString, s, i)
      }
    }

    def getScore(in: String): BigDecimal = {
      LetterFreqChiTest.evaluate(in)
    }

    out
  }


  def main(args: Array[String]): Unit = {
    println(encipher("An education is what remains after we forget everything we have learned.", 7))
    println(decipher("Hu lkbjhapvu pz doha ylthpuz hmaly dl mvynla clyfaopun dl ohcl slhyulk."))
    println(decipher("Bzdrzq bhogdq? H oqdedq Bzdrzq rzkzc."))
  }

}
