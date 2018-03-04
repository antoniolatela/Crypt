import java.io._
import scala.io.Source

class Crypt (method:String, key:String, fin:String, fout:String) {
  private val ABC = ('A' to 'z').toStream.mkString
  private var encABC = ""

  if (!key.isEmpty) (
    key.
      foreach(
      a=> if(!encABC.contains(a)) encABC+=a.toString)
      + ('A' to 'z')
      .reverse
      .foreach(a => if (encABC.length <= 57 && !encABC.contains(a)) encABC+=a.toString)
      .toString)
  else throw new CryptException("[ERROR]: key could not be empty")

  method match {
    case "-e"  => CryptDecrypt(new CryptoImplementation("encripted").CryptDecrypt, ABC, encABC, fin, fout)
    case "-d"  => CryptDecrypt(new CryptoImplementation("decrypted").CryptDecrypt, encABC, ABC, fin, fout)
    case _ => throw new CryptException("[ERROR]: Method not recognized -> use -e to encrypt and -d to decrypt")
  }

  def CryptDecrypt(f: (String, String, String, String)
    => Unit, v0: String, v1: String, v3: String, v4: String):Unit = f(v0, v1, v3, v4)
}

class CryptoImplementation(e:String) {
  def CryptDecrypt(s0:String, s1:String, s3:String, s4:String) = {
    val out = new PrintWriter(new File(s4))
    Source
      .fromFile(s3)
      .getLines()
      .foreach(line => {line
        .foreach(a=>{if (s0.indexOf(a)!=(-1)) out.write(s1(s0.indexOf(a))) else out.write(a)})
        out.write("\n")})
    out.close()
    print(s"File %s $e in file %s".format(s3, s4))
  }
}

private class CryptException(exMsg:String) extends Exception(exMsg)

object Crypt extends App {
  override def main(args: Array[String]): Unit = {
    var c = new Crypt(args(0), args(1).substring(2,args(1).length()), args(2), args(3));
  }
}