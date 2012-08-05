package net.pocorall.jogak

import java.io.{InputStreamReader, BufferedReader, Reader, InputStream}
import javax.swing.JFileChooser
import javax.imageio.ImageIO
import java.awt.Desktop
import java.awt.image.BufferedImage
import viewer.{SimpleStringViewer, SimpleImageViewer}

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

  val toBufferedImage = new Filter[NamedInputStream]("to BufferedImage", is => ImageIO.read(is.inputStream))
  val toReader = new Filter[NamedInputStream]("to Reader", is => new InputStreamReader(is.inputStream))
  val open = new Command[java.io.File]("open", fi => Desktop.getDesktop().open(fi))
  val edit = new Command[java.io.File]("edit", fi => Desktop.getDesktop().edit(fi))
  val print = new Command[java.io.File]("print", fi => Desktop.getDesktop().print(fi))
  val simpleImageViewer = new Filter[BufferedImage]("SimpleImageViewer", new SimpleImageViewer(_))
  val simpleToString = new Filter[Reader]("to String (head)", simpleReaderToString)
  val simpleToHexString = new Filter[InputStream]("to hex String (head)", simpleInputStreamToHexString)
  val simpleStringViewer = new Filter[String]("SimpleStringViewer", new SimpleStringViewer(_))
  val toLowerCase = new Command[String]("to lowercase", _.toLowerCase)
  val toUpperCase = new Command[String]("to uppercase", _.toUpperCase)
  val trim = new Command[String]("trim", _.trim)
  val saveStrAs = new Command[String]("save as...", saveStringAs)
  val toInputStream = new Filter[NamedInputStream]("to InputStream", _.inputStream)
}
