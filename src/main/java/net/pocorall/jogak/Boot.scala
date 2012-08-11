package net.pocorall.jogak

import javax.swing._
import java.io
import io._
import viewer.TreeViewer
import collection.mutable
import java.lang.reflect.Method
import org.specs2.internal.scalaz.Digit._0


trait TreeNode {
  def parent(): TreeNode

  def child(): List[TreeNode]
}

trait NamedInputStream {
  def name: String

  def inputStream: InputStream
}

class File(val file: java.io.File = new java.io.File("/")) extends TreeNode {
  def parent() = if (file.getParent == null) null else new File(file.getParentFile)

  def child() = if (file.isDirectory && file.listFiles != null) file.listFiles.toList.map(new File(_)) else List[TreeNode]()

  override def toString() = file.toString
}

class NamedFileInputStream(val file: java.io.File) extends NamedInputStream {

  def name = file.getName

  def inputStream = new BufferedInputStream(new FileInputStream(file))
}

object Extensions {
  val images = Array(".jpg", ".gif", ".png", ".bmp")
  val texts = Array(".txt", ".csv", ".pem", ".xml", ".htm", ".php", ".java", ".html", ".cpp", ".bat", ".h")

  def getExtension(n: String): String = {
    val name = n.toLowerCase
    val dot = name.lastIndexOf('.')
    if (dot > 0) name.substring(dot) else name
  }
}

import net.pocorall.jogak.SimpleFunctions._


class SimpleStaticCommandRegistry extends CommandRegistry {
  def lookup(thing: Any): Array[Command[Nothing]] = {
    val result = new mutable.MutableList[Command[Nothing]]
    def addIfAvailable(command: Command[Nothing]) {
      if (command.isAssignable(thing)) {
        result += command
      }
    }
    addIfAvailable(file)
    addIfAvailable(explore)
    addIfAvailable(namedFileInputStream)
    addIfAvailable(open)
    addIfAvailable(edit)
    addIfAvailable(print)
    addIfAvailable(toBufferedImage)
    addIfAvailable(toReader)
    addIfAvailable(toInputStream)
    addIfAvailable(simpleToHexString)
    addIfAvailable(simpleToByteHexString)
    addIfAvailable(simpleToString)
    addIfAvailable(simpleImageViewer)
    addIfAvailable(darken)
    addIfAvailable(brighten)
    addIfAvailable(grayscaleSpace)
    addIfAvailable(rgbaSpace)
    addIfAvailable(simpleStringViewer)
    addIfAvailable(trim)
    addIfAvailable(saveStrAs)
    addIfAvailable(listView)
    addIfAvailable(PPTtoImageList)
    addIfAvailable(NIStoPDFBox)
    addIfAvailable(pdPages)
    addIfAvailable(pdPageToImage)

    result.toArray
  }
}

class IntrospectionCommandRegistry extends CommandRegistry {
  val parent = new SimpleStaticCommandRegistry

  def getFullName(method: Method): String = {
    method.getName + method.getParameterTypes.map(_.getSimpleName).mkString("(", ", ", ")") +
      ": " + method.getReturnType.getSimpleName
  }

  def lookup(thing: Any): Array[Command[Nothing]] = {
    val result = new mutable.MutableList[Command[Nothing]]
    result ++= parent.lookup(thing)

    val clazz = thing.asInstanceOf[AnyRef].getClass
    val methods = clazz.getDeclaredMethods
    methods.foreach(
      method =>
        result += new Command[Any](
          "." + getFullName(method), obj => method.invoke(obj)
        )
    )

    result.toArray
  }
}

object Boot {
  def showDesktop() {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      def run() {
        val frame = new JFrame("Jogak")
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        implicit val viewerRegistry = new IntrospectionCommandRegistry

        val label = new TreeViewer(new File(new io.File("/")))
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