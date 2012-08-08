package net.pocorall.jogak

import javax.swing._
import java.io
import io._
import viewer.{EverythingViewer, TreeViewer, SimpleImageViewer, SimpleStringViewer}
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

class File(val file: java.io.File = new java.io.File("/")) extends TreeNodeModel {
  def parent() = if (file.getParent == null) null else new File(file.getParentFile)

  def child() = if (file.isDirectory && file.listFiles != null) file.listFiles.map(new File(_)) else Array[TreeNodeModel]()

  override def toString() = file.toString
}

class NamedFileInputStream(val file: java.io.File) extends NamedInputStream {

  def name = file.getName

  def inputStream = new BufferedInputStream(new FileInputStream(file))
}

object Extensions {
  val images = Array(".jpg", ".gif", ".png", ".bmp")
  val texts = Array(".txt", ".csv", ".pem", ".xml", ".htm", ".php", ".java", ".html", ".cpp", ".bat", ".h")
}

import net.pocorall.jogak.SimpleFunctions._


class CommandRegistry {
  def applyDefaultFilterChain(thing: Any): Any = {
    lookup(thing).find(c => c.isInstanceOf[Filter[Nothing]]) match {
      case Some(c) => applyDefaultFilterChain(c.execute(thing))
      case None => thing
    }
  }

  def getDefaultViewer(thing: Any): Viewer = {
    applyDefaultFilterChain(thing) match {
      case v: Viewer => v
      case t => new EverythingViewer(t)
    }
  }

  def lookup(thing: Any): Array[Command[Nothing]] = {
    val result = new mutable.MutableList[Command[Nothing]]
    thing match {
      case f: File => result += filter
      case _ =>
    }

    thing match {
      case f: java.io.File =>
        if (f.isDirectory) {
          result += explore
        } else {
          result += namedFileInputStream
        }
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

object Boot {
  def showDesktop() {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      def run() {
        val frame = new JFrame("Jogak")
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        implicit val viewerRegistry = new CommandRegistry

        val label = new TreeViewer(new File(new io.File("/")), viewerRegistry)
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