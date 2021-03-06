package net.pocorall.jogak

import java.io.{InputStreamReader, BufferedReader, Reader, InputStream}
import javax.swing._
import javax.imageio.ImageIO
import java.awt.{Color, Desktop}
import java.awt.image.{RescaleOp, BufferedImage}
import viewer.{ListViewer, TreeViewer, SimpleStringViewer, SimpleImageViewer}
import java.awt.event.{ActionListener, ActionEvent}
import org.apache.poi.hslf.usermodel.SlideShow
import java.awt.geom.Rectangle2D
import org.apache.pdfbox.pdmodel.{PDPage, PDDocument}


object SimpleFunctions {

  def simpleInputStreamToHexString(in: InputStream, formatStr: String = "%02X ") = {
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
        out.append(formatStr.format(v))
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


  def buildMenu(obj: Any, menu: JComponent, showView: Viewer => Unit)(implicit commandRegistry: CommandRegistry) {
    commandRegistry.lookup(obj).foreach {
      com => com match {
        case c: Filter[Nothing] =>
          try {
            val result = com.execute(obj)
            result match {
              case f: Viewer =>
                addMenuItem(menu, com.name, _ => showView(f))
              case _ =>
                val m = new JMenu(com.name)
                buildMenu(result, m, showView)
                menu.add(m)
            }
          } catch {
            case e: Throwable => e.printStackTrace() //swallow
          }
        case c: Command[Nothing] =>
          addMenuItem(menu, com.name, _ => showView(commandRegistry.getDefaultViewer(com.execute(obj))))
        case _ =>
      }
    }
  }

  implicit val commandRegistry = new IntrospectionCommandRegistry

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
  val namedFileInputStream = new Filter[java.io.File]("to NamedInputStream",
    fi => new NamedFileInputStream(fi),
    fi => !fi.isDirectory)
  val explore = new Filter[java.io.File]("Explore directory",
    fi => new TreeViewer(new File(fi)),
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

  val simpleToString = new Command[Reader]("to String (head)", simpleReaderToString)
  val simpleToHexString = new Command[InputStream]("to hex String (head)", simpleInputStreamToHexString(_))
  val simpleToByteHexString = new Command[InputStream]("to byte hex String (head)", simpleInputStreamToHexString(_, "%d "))

  val simpleStringViewer = new Filter[String]("SimpleStringViewer", new SimpleStringViewer(_))
  val trim = new Command[String]("split lines", _.split("\n").toList)
  val listView = new Filter[List[Any]]("ListViewer", new ListViewer(_))
  val saveStrAs = new Command[String]("save as...", saveStringAs)

  val PPTtoImageList = new Command[InputStream]("pptToImageList", {
    (is: InputStream) =>
      val ppt = new SlideShow(is)
      val pgsize = ppt.getPageSize
      val slides = ppt.getSlides

      slides.map {
        slide =>
          val img = new BufferedImage(pgsize.width, pgsize.height, BufferedImage.TYPE_INT_RGB)
          val graphics = img.createGraphics()
          graphics.setPaint(Color.white)
          graphics.fill(new Rectangle2D.Float(0, 0, pgsize.width, pgsize.height))
          slide.draw(graphics)
          img
      }.toList
  })

  import scala.collection.JavaConversions._

  val NIStoPDFBox = new Command[NamedInputStream]("to PDFBox document",
    is => PDDocument.load(is.inputStream),
    is => ".pdf" equals Extensions.getExtension(is.name)
  )

  val pdPages = new Command[PDDocument]("get all pages",
    _.getDocumentCatalog.getAllPages.asInstanceOf[java.util.List[PDPage]].toList
  )

  val pdPageToImage = new Command[PDPage]("to BufferedImage", _.convertToImage(BufferedImage.TYPE_INT_RGB, 100))
}
