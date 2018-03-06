import org.scalatest.FunSuite

class CaesarDeCipherTest extends FunSuite {

  test("testEncipher") {
    assert(CaesarDeCipher.encipher("aaA",1).equals("bbB"))
  }

  test("testEncipher1") {
    assert(CaesarDeCipher.encipher1("zZz",1).equals("aAa"))
  }

}
