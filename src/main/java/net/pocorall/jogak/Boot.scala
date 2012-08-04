package net.pocorall.jogak

import javax.swing._
import table.TableModel
import java.io
import event.TableModelListener
import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseAdapter}
import java.awt.BorderLayout


trait TreeNodeModel {
  def parent(): TreeNodeModel

  def child(): Array[TreeNodeModel]
}


class File(file: java.io.File = new java.io.File("/")) extends TreeNodeModel {
  def parent() = new File(file.getParentFile)

  def child() = file.listFiles.map(new File(_))

  override def toString() = file.toString
}

/**marker class */
abstract class Viewer extends JPanel(new BorderLayout);

class TreeViewer(var model: TreeNodeModel, implicit val viewerRegistry: ViewerRegistry) extends Viewer {
  val tableModel = new TableModel() {
    def getRowCount = model.child().size

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
      model = model.child()(row)
      //      table.setModel(tableModel)
      table.repaint()
    }
  })

  val viewPanel = new JPanel()


  private val leftPane = new JPanel(new BorderLayout)
  private val upButton = new JButton("up")
  upButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {

    }
  })

  leftPane.add(upButton, BorderLayout.NORTH)

  leftPane.add(new JScrollPane(table), BorderLayout.CENTER)

  private val spane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
    leftPane, new JScrollPane(viewPanel));

  add(spane, BorderLayout.CENTER);
}

class BinaryFileViewer(file: java.io.File) extends Viewer {
  add(new JLabel("Binary:" + file.toString))
}

class EverythingViewer(thing: Any) extends Viewer {
  add(new JLabel(thing.toString))
}

trait ViewerRegistry {
  def lookup(thing: Any): Viewer
}

class SimpleViewerRegistry extends ViewerRegistry {
  override def lookup(thing: Any): Viewer = (thing) match {
    case f: java.io.File if (f.isDirectory) => new TreeViewer(new File(f), this)
    case f: java.io.File => new BinaryFileViewer(f)
    case f => new EverythingViewer(f)
  }
}

object Boot {
  def showDesktop() {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      def run() {
        val frame = new JFrame("HelloWorldSwing");
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