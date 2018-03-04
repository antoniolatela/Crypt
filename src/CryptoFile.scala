import java.io._
import scala.io.Source

class CryptoFile (method:String, key:String, fin:String, fout:String) {
  //constructor
  private val ABC = ('A' to 'z').toStream.mkString
  private var encABC = ""

  (key
    .foreach( //for each element of key remove from key any duplicate char (eg. antonio became antoi)
    a=> if(!encABC.contains(a)) encABC+=a.toString)
    + ('A' to 'z') //add to key the other piece of an alphabet from z to a creating a new alpabet (example, if key=antonio the alphabet will be antoizyxwvusrqpmlkjhgfedcb)
    .reverse
    .foreach(a => if (encABC.length <= 57 && !encABC.contains(a)) encABC+=a.toString)
    .toString)

  method match {
    case "-e"  => CryptDecrypt(new CryptoImplementation("encrypted").CryptDecrypt, ABC, encABC, fin, fout) //call lambda passing CryptoImplementation class
    case "-d"  => CryptDecrypt(new CryptoImplementation("decrypted").CryptDecrypt, encABC, ABC, fin, fout) //call lambda passing CryptoImplementation class
    case _ => throw new CryptException("[ERROR]: Method not recognized -> use -e to encrypt and -d to decrypt")
  }
  //end of constructor

  def CryptDecrypt(f: (String, String, String, String) //lambda function to call the decrypt method
    => Unit, v0: String, v1: String, v3: String, v4: String):Unit = f(v0, v1, v3, v4)
}

class CryptoImplementation(e:String){//constructor take method description type
  def CryptDecrypt(s0:String, s1:String, s3:String, s4:String):Boolean = {//depending on encrypt\decrypt the method takes alphabet, encrypted alphabet, input file, output file
    try {                                                                 //or encrypted alphabet, alphabet, input file, output file
      val out = new PrintWriter(new File(s4)) //open file to write
      Source.fromFile(s3).getLines().foreach(line => {line.foreach(a => { //for each line perform conversion from one alphabet to another
              if (s0.indexOf(a) != (-1)) out.write(s1(s0.indexOf(a))) else out.write(a)}) //write decoded char
          out.write("\n")})
      out.close()
      println(s"File %s $e in file %s".format(s3, s4)) //write output
      true //return if ok
    } catch {
      case _: Exception => throw new CryptException("[ERROR]: Problem converting file")
        false //return if ko
    }
  }
}

private class CryptException(exMsg:String) extends Exception(exMsg)

object CryptoFile extends App {
  override def main(args: Array[String]): Unit = {
    if (args.length<4) throw new CryptException("[ERROR]: Missing attribute. Exec: Crypt [-d; -e] -k[key_value] input_file output_file")
    new CryptoFile(args(0), args(1).substring(2,args(1).length()), args(2), args(3));
  }
}