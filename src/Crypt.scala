import scala.io.Source

class Crypt (method:String, key:String, in:String, out:String) {

  private var k = ""
  if (key.map(a=>if(!k.contains(a)) k+=a.toString).isEmpty) throw new CryptException("[ERROR]: key could not be empty")

  print(k)
  method match {
    case "-e"  => println("-e")
    case "-d"  => println("-d")
    case _  => throw new CryptException("[ERROR]: Method not recognized -> use -e to encrypt and -d to decrypt")
  }

  Source.fromFile(in).getLines().foreach(line=>line.toCharArray.foreach(a=>a.toLower))


}

private class CryptException(exMsg:String) extends Exception(exMsg)


object Crypt extends App {
  override def main(args: Array[String]): Unit = {
    var c = new Crypt(args(0), args(1).substring(2,args(1).length()), args(2), args(3));

  }
}