package net.pocorall.jogak

import java.io.{InputStreamReader, BufferedReader, Reader, InputStream}
import javax.swing._
import javax.imageio.ImageIO
import java.awt.Desktop
import java.awt.image.{RescaleOp, BufferedImage}
import viewer.{TreeViewer, SimpleStringViewer, SimpleImageViewer}
import java.awt.event.{ActionListener, ActionEvent}

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

  implicit def toActionListener(f: ActionEvent => Unit) = new ActionListener {
    def actionPerformed(e: ActionEvent) {
      f(e)
    }
  }

  def addMenuItem(menu: JComponent, name: String, actionListener: ActionEvent => Unit) {
    val item = new JMenuItem(name)
    item.addActionListener(actionListener)
    menu.add(item)
  }

  val cr = new SimpleStaticCommandRegistry()

  def buildMenu(obj: Any, menu: JComponent, showView: Viewer => Unit) {
    cr.lookup(obj).foreach {
      com => com match {
        case c: Filter[Nothing] =>
          try {
            val result = com.execute(obj)
            result match {
              case f: Viewer =>
                addMenuItem(menu, com.name, _ => showView(f))
              case _ =>
                val m = new JMenu(com.name)
                //                  println(result.getClass().toString)
                buildMenu(result, m, showView)
                menu.add(m)
            }
          } catch {
            case e: Any => //swallow
          }
        case c: Command[Nothing] =>
          addMenuItem(menu, com.name, _ => showView((new SimpleStaticCommandRegistry).getDefaultViewer(com.execute(obj))))
        case _ =>
      }
    }
  }

  val file = new Filter[File]("File", _.file)

  val toBufferedImage = new Filter[NamedInputStream]("to BufferedImage",
    is => ImageIO.read(is.inputStream),
    is => Extensions.images contains Extensions.getExtension(is.name))
  val toReader = new Filter[NamedInputStream]("to Reader",
    is => new InputStreamReader(is.inputStream),
    is => Extensions.texts contains Extensions.getExtension(is.name))

  val toInputStream = new Filter[NamedInputStream]("to InputStream", _.inputStream)

  val open = new Command[java.io.File]("open", fi => Desktop.getDesktop().open(fi))
  val edit = new Command[java.io.File]("edit", fi => Desktop.getDesktop().edit(fi))
  val print = new Command[java.io.File]("print", fi => Desktop.getDesktop().print(fi))
  val namedFileInputStream = new Filter[java.io.File]("FileInputStream",
    fi => new NamedFileInputStream(fi),
    fi => !fi.isDirectory)
  val explore = new Filter[java.io.File]("Explore directory",
    fi => new TreeViewer(new File(fi), new SimpleStaticCommandRegistry),
    fi => fi.isDirectory)

  val simpleImageViewer = new Filter[BufferedImage]("SimpleImageViewer", new SimpleImageViewer(_))

  def noIndexed(img: BufferedImage): Boolean = {
    val imgType = img.getType
    import BufferedImage._
    (imgType != TYPE_BYTE_INDEXED && imgType != TYPE_BYTE_BINARY)
  }

  val darken = new Command[BufferedImage]("darken", {
    img =>
      val scaleFactor = .9f
      val op = new RescaleOp(scaleFactor, 0, null)
      op.filter(img, null)
  }, noIndexed)

  val brighten = new Command[BufferedImage]("brighten", {
    img =>
      val scaleFactor = 1.3f
      val op = new RescaleOp(scaleFactor, 0, null)
      op.filter(img, null)
  }, noIndexed)

  def changeColorspace(img: BufferedImage, imgType: Int): BufferedImage = {
    val image = new BufferedImage(img.getWidth, img.getHeight, imgType)
    val g = image.getGraphics
    g.drawImage(img, 0, 0, null)
    g.dispose()
    image
  }

  val grayscaleSpace = new Command[BufferedImage]("grayscale colorspace",
    changeColorspace(_, BufferedImage.TYPE_BYTE_GRAY)
  )

  val rgbaSpace = new Command[BufferedImage]("RGB colorspace",
    changeColorspace(_, BufferedImage.TYPE_INT_RGB)
  )

  val simpleToString = new Filter[Reader]("to String (head)", simpleReaderToString)
  val simpleToHexString = new Filter[InputStream]("to hex String (head)", simpleInputStreamToHexString)

  val simpleStringViewer = new Filter[String]("SimpleStringViewer", new SimpleStringViewer(_))
  val toLowerCase = new Command[String]("to lowercase", _.toLowerCase)
  val toUpperCase = new Command[String]("to uppercase", _.toUpperCase)
  val trim = new Command[String]("trim", _.trim)
  val saveStrAs = new Command[String]("save as...", saveStringAs)

  val toStringCommand = new Command[java.lang.Object]("Object.toString", _.toString)
  val getClassCommand = new Command[java.lang.Object]("Object.getClass", _.getClass)

}
