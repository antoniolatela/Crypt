import java.io._
import scala.io.Source

class Crypt (method:String, key:String, fin:String, fout:String) {

  private var alphabet: String = ""
  if (!key.isEmpty) (
    key.
    map(a=>if(!alphabet.contains(a)) alphabet+=a.toString)
      + (('a' to 'z')
    .reverse
    .toStream
    .foreach(a=>if (alphabet.length<=25 && !alphabet.contains(a)) alphabet+=a.toString))
    .toString)
  else throw new CryptException("[ERROR]: key could not be empty")

  method match {
    case "-e"  => enc
    case "-d"  => dec
    case _ => throw new CryptException("[ERROR]: Method not recognized -> use -e to encrypt and -d to decrypt")
  }

  def enc = {
    val out = new PrintWriter(new File(fout))
    val AtoZ = ('a' to 'z').toStream.toList
    Source
      .fromFile(fin)
      .getLines()
      .foreach(line => {line
        .foreach(a=>{if (AtoZ.indexOf(a)!=(-1)) out.write(alphabet(AtoZ.indexOf(a))) else out.write(a)})
        out.write("\n")})
    out.close()
  }

  def dec = {
    val out = new PrintWriter(new File(fout))
    val AtoZ = ('a' to 'z').toStream.toList
    print(AtoZ.indexOf('z'))
    Source
      .fromFile(fin)
      .getLines()
      .foreach(line => {line
        .foreach(a=>{if (alphabet.indexOf(a)!=(-1)) out.write(AtoZ(alphabet.indexOf(a))) else out.write(a)})
        out.write("\n")})
    out.close()
  }

}

private class CryptException(exMsg:String) extends Exception(exMsg)


object Crypt extends App {
  override def main(args: Array[String]): Unit = {
    var c = new Crypt(args(0), args(1).substring(2,args(1).length()), args(2), args(3));
  }
}