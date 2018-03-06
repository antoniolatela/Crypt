import org.scalatest.FunSuite

class CaesarDeCipherTest extends FunSuite {

  test("testEncipher") {
    assert(CaesarDeCipher.encipher("aaA",1).equals("bbB"))
    assert(CaesarDeCipher.encipher("zZz",1).equals("aAa"))
  }


}
