package net.pocorall.jogak

import java.io.{BufferedReader, Reader, InputStream}
import javax.swing.JFileChooser

object SimpleFunctions {

  def simpleInputStreamToHexString(in: InputStream) = {
    val out = new StringBuilder()
    var ind: Int = 0
    var remained: Boolean = false
    Stream.continually(in.read()).takeWhile {
      v => remained = (v != -1)
      remained
    }.take(1000).foreach {
      v =>
        if (ind % 16 == 0) {
          if (ind != 0) {
            out.append("\n")
          }
          out.append("0x%08X \t".format(ind))
        }
        out.append("%02X ".format(v))
        ind += 1
    }
    if (remained) {
      out.append("...\tmore content is remaining")
    }

    out.toString()
  }


  def simpleReaderToString(in: Reader) = {
    val bin = new BufferedReader(in)
    val out = new StringBuilder()
    var remained: Boolean = false
    Stream.continually(bin.readLine()).takeWhile {
      v => remained = (v != null)
      remained
    }.take(1000).foreach(out.append(_).append("\n"))
    if (remained) {
      out.append("...\tmore content is remaining")
    }
    out.toString()
  }


  def saveStringAs(in: String) {
    val fc = new JFileChooser();
    val returnVal = fc.showOpenDialog(null);

    if (returnVal == JFileChooser.APPROVE_OPTION) {
      val file = fc.getSelectedFile();
      //This is where a real application would open the file.
      //      log.append("Opening: " + file.getName() + "." + newline);
    } else {
      // canceled
    }
  }
}

