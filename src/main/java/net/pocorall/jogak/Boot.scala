package net.pocorall.jogak

import javax.swing._
import java.io
import io._
import viewer.{TreeViewer, SimpleImageViewer, SimpleStringViewer}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import collection.mutable

trait TreeNodeModel {
  def parent(): TreeNodeModel

  def child(): Array[TreeNodeModel]
}

trait NamedInputStream {
  def name: String

  def inputStream: InputStream
}

class File(val file: java.io.File = new java.io.File("/")) extends TreeNodeModel with NamedInputStream {
  def parent() = if (file.getParent == null) null else new File(file.getParentFile)

  def child() = if (file.isDirectory && file.listFiles != null) file.listFiles.map(new File(_)) else Array[TreeNodeModel]()

  override def toString() = file.toString

  def name = file.getName

  def inputStream = new BufferedInputStream(new FileInputStream(file))
}

object Extensions {
  val images = Array(".jpg", ".gif", ".png", ".bmp")
  val texts = Array(".txt", ".csv", ".pem", ".xml", ".htm", ".php", ".java", ".html", ".cpp", ".bat", ".h")
}

import net.pocorall.jogak.SimpleFunctions._

class CommandRegistry {
  def lookup(thing: Any): Array[Command[Nothing]] = {
    val result = new mutable.MutableList[Command[Nothing]]
    thing match {
      case f: File => result += new Filter[File]("File", _.file)
      case f: java.io.File if (f.isDirectory) => result += new Filter[java.io.File]("Explore directory", fi => new TreeViewer(new File(fi), new SimpleViewerRegistry))
      case _ =>
    }

    thing match {
      case f: java.io.File =>
        result += open
        result += edit
        result += print
      case _ =>
    }
    thing match {
      case n: NamedInputStream => {
        val name = n.name.toLowerCase
        val dot = name.lastIndexOf('.')
        val ext = if (dot > 0) name.substring(dot) else name

        ext match {
          case e if (Extensions.images contains e) => result += toBufferedImage
          case e if (Extensions.texts contains e) => result += toReader
          case _ =>
        }

        result += toInputStream
      }
      case _ =>
    }

    thing match {
      case f: InputStream => result += simpleToHexString
      case _ =>
    }
    thing match {
      case f: Reader => result += simpleToString
      case _ =>
    }
    thing match {
      case f: BufferedImage => result += simpleImageViewer
      case _ =>
    }
    thing match {
      case s: String =>
        result += simpleStringViewer
        result += toLowerCase
        result += toUpperCase
        result += trim
        result += saveStrAs

      case _ =>
    }

    result.toArray
  }
}

class SimpleViewerRegistry extends ViewerRegistry {
  override def lookup(thing: Any): Viewer = (thing) match {
    case f: File if (f.file.isDirectory) => new TreeViewer(f, this)

    case n: NamedInputStream => {
      val name = n.name.toLowerCase
      val dot = name.lastIndexOf('.')
      val ext = if (dot > 0) name.substring(dot) else name

      ext match {
        case e if (Extensions.images contains e) => lookup(ImageIO.read(n.inputStream))
        case e if (Extensions.texts contains e) => lookup(new InputStreamReader(n.inputStream))

        case _ => lookup(n.inputStream)
      }
    }

    case f: InputStream => lookup(simpleInputStreamToHexString(f))
    case f: Reader => lookup(simpleReaderToString(f))

    case f: BufferedImage => new SimpleImageViewer(f)
    case f: String => new SimpleStringViewer(f)

    case f => lookup(f.toString)
  }
}

object Boot {
  def showDesktop() {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      def run() {
        val frame = new JFrame("Jogak")
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        implicit val viewerRegistry = new SimpleViewerRegistry

        val label = new TreeViewer(new File(new io.File("C:\\")), viewerRegistry)
        frame.getContentPane().add(label)

        frame.pack()
        frame.setVisible(true)
      }
    })
  }

  def main(args: Array[String]) {
    showDesktop()
  }
}