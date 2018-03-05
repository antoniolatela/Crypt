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
    case "-e"  => CryptDecrypt(new CryptoImplementation("encrypted").CryptDecrypt, ABC, encABC, fin, fout) //call lambda passing CryptoImplementation class
    case "-d"  => CryptDecrypt(new CryptoImplementation("decrypted").CryptDecrypt, encABC, ABC, fin, fout) //call lambda passing CryptoImplementation class
    case _ => throw new CryptoFileException("[ERROR]: Method not recognized -> use -e to encrypt and -d to decrypt")
  }
  //end of constructor

  def CryptDecrypt(f: (String, String, String, String) //lambda function to call the decrypt method
    => Unit, v0: String, v1: String, v3: String, v4: String):Unit = f(v0, v1, v3, v4)
}

class CryptoImplementation(e:String){//constructor take method description type
  def CryptDecrypt(s0:String, s1:String, s3:String, s4:String):Boolean = {//depending on encrypt\decrypt the method takes alphabet, encrypted alphabet, input file, output file //or encrypted alphabet, alphabet, input file, output file
    val out = new PrintWriter(new File(s4)) //open file to write
      try {
        Source.fromFile(s3).getLines().foreach(line => {line.foreach(a => {
                val isUpper = a.isUpper//for each line perform conversion from one alphabet to another
                if (s0.indexOf(a.toLower) != (-1)) { //check if char is in alphabet
                  if (isUpper) {out.write(s1(s0.indexOf(a.toLower)).toUpper)} //write decoded uppercase char
                  else {out.write(s1(s0.indexOf(a)))}} //write decoded lowercase char
                else out.write(a)}) //write decoded unrecognized char
            out.write("\n")})
        println(s"[INFO]: File %s $e in file %s".format(s3, s4)) //write output
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