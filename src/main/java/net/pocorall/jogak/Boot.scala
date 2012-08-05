package net.pocorall.jogak

import javax.swing._
import java.io
import io._
import java.awt.Desktop
import viewer.{TreeViewer, SimpleImageViewer, SimpleStringViewer}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import collection.mutable
import java.util.Date

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
      case f: File if (f.file.isDirectory) => result += new Filter[File]("Explore directory", new TreeViewer(_, new SimpleViewerRegistry))

      case _ =>
        thing match {
          case f: File =>
            result += new Command[File]("open", fi => Desktop.getDesktop().open(fi.file))
            result += new Command[File]("edit", fi => Desktop.getDesktop().edit(fi.file))
            result += new Command[File]("print", fi => Desktop.getDesktop().print(fi.file))
          case _ =>
        }
        thing match {
          case n: NamedInputStream => {
            val name = n.name.toLowerCase
            val dot = name.lastIndexOf('.')
            val ext = if (dot > 0) name.substring(dot) else name

            ext match {
              case e if (Extensions.images contains e) => result += new Filter[NamedInputStream]("to BufferedImage", is => ImageIO.read(is.inputStream))
              case e if (Extensions.texts contains e) => result += new Filter[NamedInputStream]("to Reader", is => new InputStreamReader(is.inputStream))
              case _ =>
            }

            result += new Filter[NamedInputStream]("to InputStream", _.inputStream)
          }
          case _ =>
        }
    }

    thing match {
      case f: InputStream => result += new Filter[InputStream]("to hex String", simpleInputStreamToHexString)
      case _ =>
    }
    thing match {
      case f: Reader => result += new Filter[Reader]("to String", simpleReaderToString)
      case _ =>
    }
    thing match {
      case f: BufferedImage => result += new Filter[BufferedImage]("SimpleImageViewer", new SimpleImageViewer(_))
      case _ =>
    }
    thing match {
      case s: String =>
        result += new Filter[String]("SimpleStringViewer", new SimpleStringViewer(_))
        result += new Command[String]("to lowercase", _.toLowerCase)
        result += new Command[String]("to uppercase", _.toUpperCase)
        result += new Command[String]("trim", _.trim)
        result += new Command[String]("save as...", saveStringAs)

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

object TTest {
  def main(args: Array[String]) {
    class MyCommand[-T: Manifest](val name: String, val execute: T => Any) {
      val fromClass = manifest[T].erasure
    }
    // prepare two commands
    val commands = new mutable.MutableList[MyCommand[Nothing]]
    commands += new MyCommand[String]("lower", _.toLowerCase)
    commands += new MyCommand[Date]("time", _.getTime)
    // prepare two data
    val data = Array("StRiNG", new Date())
    data.foreach {
      d => commands.foreach {
        c =>
          if (d.getClass.isAssignableFrom(c.fromClass)) {
            println("    cmd(data) = " + c.execute.asInstanceOf[Any => Any](d))
          }
      }
    }
  }
}

object Boot {
  def showDesktop() {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      def run() {
        val frame = new JFrame("Jogak");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        implicit val viewerRegistry = new SimpleViewerRegistry

        val label = new TreeViewer(new File(new io.File("C:\\")), viewerRegistry);
        frame.getContentPane().add(label);

        frame.pack();
        frame.setVisible(true);
      }
    });
  }

  def main(args: Array[String]) {
    showDesktop();
  }
}