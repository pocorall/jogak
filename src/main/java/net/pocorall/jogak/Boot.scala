package net.pocorall.jogak

import javax.swing._
import table.TableModel
import java.io
import event.TableModelListener
import io._
import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseAdapter}
import java.awt.{Desktop, Graphics, Dimension, BorderLayout}
import viewer.SimpleStringViewer
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

/**marker class */
abstract class Viewer extends JPanel(new BorderLayout);

class TreeViewer(var model: TreeNodeModel, implicit val viewerRegistry: ViewerRegistry) extends Viewer {
  val tableModel = new TableModel() {
    def getRowCount = model.child().length

    def getColumnCount = 1

    def getColumnName(columnIndex: Int) = model.toString()

    def getColumnClass(columnIndex: Int) = classOf[String]

    def isCellEditable(rowIndex: Int, columnIndex: Int) = false

    def getValueAt(rowIndex: Int, columnIndex: Int) = model.child()(rowIndex).toString()

    def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int) {}

    def addTableModelListener(l: TableModelListener) {}

    def removeTableModelListener(l: TableModelListener) {}
  }

  val table = new JTable(tableModel);



  table.addMouseListener(new MouseAdapter() {
    override def mouseClicked(e: MouseEvent) {
      val p = e.getPoint
      val row = table rowAtPoint p
      if (row < 0) return
      val newModel = model.child()(row)

      if (e.getButton == MouseEvent.BUTTON1) {
        if (e.getClickCount == 2) {
          setModel(newModel)
        } else {
          spane.setRightComponent(viewerRegistry.lookup(newModel))
        }
      } else {
        val cr = new CommandRegistry()
        val menu = new JPopupMenu()
        def buildMenu(obj: Any, menu: JComponent) {
          cr.lookup(obj).foreach {
            com =>
              com match {
                case c: Filter =>
                  val result = com.run()
                  result match {
                    case f: Viewer =>
                      val item = new JMenuItem(com.name)
                      item.addActionListener(new ActionListener() {
                        def actionPerformed(e: ActionEvent) {
                          spane.setRightComponent(f)
                        }
                      })
                      menu.add(item)
                    case _ =>
                      val m = new JMenu(com.name)
                      //                  println(result.getClass().toString)
                      buildMenu(result, m)
                      menu.add(m)
                  }
                case c: Command =>
                  val item = new JMenuItem(com.name)
                  item.addActionListener(new ActionListener() {
                    def actionPerformed(e: ActionEvent) {
                      spane.setRightComponent(new SimpleViewerRegistry().lookup(com.run()))
                    }
                  })
                  menu.add(item)
                case _ =>
              }

          }
        }
        buildMenu(newModel, menu)

        menu.show(e.getComponent(), e.getX(), e.getY());
      }
    }
  })


  private val leftPane = new JPanel(new BorderLayout)
  leftPane.setMinimumSize(new Dimension(150, 150))

  private val upButton = new JButton("Up")
  upButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      setModel(model.parent())
    }
  })

  leftPane.add(upButton, BorderLayout.NORTH)

  leftPane.add(new JScrollPane(table), BorderLayout.CENTER)

  def setModel(newModel: TreeNodeModel) {
    model = newModel
    table.getColumnModel().getColumn(0).setHeaderValue(model.toString)
    upButton.setEnabled(model.parent != null)

    repaint()
  }

  setModel(model)

  private val spane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
    leftPane, new JLabel("Hello"));

  add(spane, BorderLayout.CENTER);
}


class EverythingViewer(thing: Any) extends Viewer {
  add(new JLabel(thing.toString))
}

trait ViewerRegistry {
  def lookup(thing: Any): Viewer
}

object simpleInputStreamToHexString extends Function1[InputStream, String] {
  def apply(in: InputStream) = {
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
}

object simpleReaderToString extends Function1[Reader, String] {
  def apply(in: Reader) = {
    val bin = new BufferedReader(in)
    val out = new StringBuilder()
    var ind: Int = 0
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
}

object saveStringAs {
  def apply(in: String) {
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


class SimpleImageViewer(img: BufferedImage) extends Viewer {
  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    g.drawImage(img, 0, 0, null)
  }
}

object Extensions {
  val images = Array(".jpg", ".gif", ".png", ".bmp")
  val texts = Array(".txt", ".csv", ".pem", ".xml", ".htm", ".php", ".java", ".html", ".cpp", ".bat", ".h")
}


class Command(val name: String, result: => Any) {
  def run() = result
}

class Filter(name: String, result: => Any) extends Command(name, result)


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