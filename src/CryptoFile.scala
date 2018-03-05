import java.io._
import scala.io.Source

class CryptoFile (method:String, key:String, fin:String, fout:String) {
  //constructor
  private val ABC = ('a' to 'z').toStream.mkString
  private var encABC = ""

  (key
    .foreach( //for each element of key remove from key any duplicate char (eg. antonio became antoi)
    a=> if(!encABC.contains(a)) encABC+=a.toString)
    + ('a' to 'z') //add to key the other piece of an alphabet from z to a creating a new alpabet (example, if key=antonio the alphabet will be antoizyxwvusrqpmlkjhgfedcb)
    .reverse
    .foreach(a => if (encABC.length <= ABC.length && !encABC.contains(a)) encABC+=a.toString)
    .toString)

  method match {
    case "-e"  => CryptDecrypt(new CryptoFileImplementation("encrypted").CryptDecrypt, ABC, encABC, fin, fout) //call lambda passing CryptoImplementation class
    case "-d"  => CryptDecrypt(new CryptoFileImplementation("decrypted").CryptDecrypt, encABC, ABC, fin, fout) //call lambda passing CryptoImplementation class
    case _ => throw new CryptoFileException("[ERROR]: Method not recognized -> use -e to encrypt and -d to decrypt")
  }
  //end of constructor

  //lambda function to call the decrypt method
  def CryptDecrypt(f:(String*) => Boolean, s:String*):Boolean = f(s(0), s(1), s(2), s(3))
}

class CryptoFileImplementation(e:String){//constructor take method description type
  def CryptDecrypt(s:String*):Boolean = {//depending on encrypt\decrypt the method takes alphabet, encrypted alphabet, input file, output file //or encrypted alphabet, alphabet, input file, output file
    val out = new PrintWriter(new File(s(3))) //open file to write
      try {
        Source.fromFile(s(2)).getLines().foreach(line => {line.foreach(a => {
                val isUpper = a.isUpper//for each line perform conversion from one alphabet to another
                if (s(0).indexOf(a.toLower) != (-1)) { //check if char is in alphabet
                  if (isUpper) {out.write(s(1)(s(0).indexOf(a.toLower)).toUpper)} //write decoded uppercase char
                  else {out.write(s(1)(s(0).indexOf(a)))}} //write decoded lowercase char
                else out.write(a)}) //write decoded unrecognized char
            out.write("\n")})
        println(s"[INFO]: File %s $e in file %s".format(s(2), s(3))) //write output
        true //return if ok
      } catch {
        case e: Exception => throw new CryptoFileException("[ERROR]: Problem converting file -> " + e.getMessage)
          false //return if ko
      } finally { out.close() }
  }
}

private class CryptoFileException(exMsg:String) extends Exception(exMsg)

object CryptoFile extends App {
  override def main(args: Array[String]): Unit = {
    if (args.length<4) throw new CryptoFileException("[ERROR]: Missing attribute. Exec: Crypt [-d; -e] -k[key_value] input_file output_file")
    new CryptoFile(args(0), args(1).substring(2,args(1).length()), args(2), args(3));
  }
}