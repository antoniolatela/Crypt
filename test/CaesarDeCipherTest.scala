import org.scalatest.FunSuite

class CaesarDeCipherTest extends FunSuite {

  test("testEncipher") {
    assert(CaesarDeCipher.encipher("aaa",1).equals("bbb"))
  }

  test("testEncipher1") {
    assert(CaesarDeCipher.encipher1("zzz",1).equals("aaa"))
  }

}
