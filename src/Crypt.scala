import java.io._
import scala.io.Source

class Crypt (method:String, key:String, fin:String, fout:String) {
  private val ABC = ('A' to 'z').toStream.toList
  print(ABC)
  private var encABC: String = ""
  if (!key.isEmpty) (
    key.
    map(a=>if(!encABC.contains(a)) encABC+=a.toString)
      + (('A' to 'z')
    .reverse
    .toStream
    .foreach(a=>if (encABC.length<=57 && !encABC.contains(a)) encABC+=a.toString))
    .toString)
  else throw new CryptException("[ERROR]: key could not be empty")

  method match {
    case "-e"  => enc
    case "-d"  => dec
    case _ => throw new CryptException("[ERROR]: Method not recognized -> use -e to encrypt and -d to decrypt")
  }

  def enc = {
    val out = new PrintWriter(new File(fout))
    Source
      .fromFile(fin)
      .getLines()
      .foreach(line => {line
        .foreach(a=>{if (ABC.indexOf(a)!=(-1)) out.write(encABC(ABC.indexOf(a))) else out.write(a)})
        out.write("\n")})
    out.close()
    print("File %s encrypted in file %s".format(fin, fout))
  }

  def dec = {
    val out = new PrintWriter(new File(fout))
    Source
      .fromFile(fin)
      .getLines()
      .foreach(line => {line
        .foreach(a=>{if (encABC.indexOf(a)!=(-1)) out.write(ABC(encABC.indexOf(a))) else out.write(a)})
        out.write("\n")})
    out.close()
    print("File %s decrypted in file %s".format(fin, fout))
  }

}

private class CryptException(exMsg:String) extends Exception(exMsg)

object Crypt extends App {
  override def main(args: Array[String]): Unit = {
    var c = new Crypt(args(0), args(1).substring(2,args(1).length()), args(2), args(3));
  }
}