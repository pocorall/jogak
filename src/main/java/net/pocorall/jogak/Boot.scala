package net.pocorall.jogak

import javax.swing._
import table.TableModel
import java.io
import event.TableModelListener
import io._
import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseAdapter}
import java.awt.{Desktop, Graphics, Dimension, BorderLayout}
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
  def lookup(thing: Any): Array[Command] = {
    val result = new mutable.MutableList[Command]
    thing match {
      case f: File if (f.file.isDirectory) => result += new Filter("Explore directory", new TreeViewer(f, new SimpleViewerRegistry))

      case _ =>
        thing match {
          case f: File =>
            result += new Command("open", Desktop.getDesktop().open(f.file))
            result += new Command("edit", Desktop.getDesktop().edit(f.file))
            result += new Command("print", Desktop.getDesktop().print(f.file))
          case _ =>
        }
        thing match {
          case n: NamedInputStream => {
            val name = n.name.toLowerCase
            val dot = name.lastIndexOf('.')
            val ext = if (dot > 0) name.substring(dot) else name

            ext match {
              case e if (Extensions.images contains e) => result += new Filter("to BufferedImage", ImageIO.read(n.inputStream))
              case e if (Extensions.texts contains e) => result += new Filter("to Reader", new InputStreamReader(n.inputStream))
              case _ =>
            }

            result += new Filter("to InputStream", n.inputStream)
          }
          case _ =>
        }
    }

    thing match {
      case f: InputStream => result += new Filter("to hex String", simpleInputStreamToHexString(f))
      case _ =>
    }
    thing match {
      case f: Reader => result += new Filter("to String", simpleReaderToString(f))
      case _ =>
    }
    thing match {
      case f: BufferedImage => result += new Filter("SimpleImageViewer", new SimpleImageViewer(f))
      case _ =>
    }
    thing match {
      case s: String =>
        result += new Filter("SimpleStringViewer", new SimpleStringViewer(s))
        result += new Command("to lowercase", s.toLowerCase)
        result += new Command("to uppercase", s.toUpperCase)
        result += new Command("trim", s.trim)
        result += new Command("save as...", saveStringAs(s))

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
    class MyCommand[-T, +R](val name: String, val execute: T => R)
    // prepare two commands
    val commands = new mutable.MutableList[MyCommand[Nothing, Any]]
    commands += new MyCommand[String, String]("lower", s => s.toLowerCase())
    commands += new MyCommand[Date, Long]("time", d => d.getTime)
    // prepare two data
    val data = Array("StRiNG", new Date())
    data.foreach {
      d => commands.foreach {
        c =>
        //          c.execute match {
        //            case m: (d.getClass => Any) => println(c.execute(d))
        //          }
        // println(c.execute(d)) if d is applicable to c.execute().
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