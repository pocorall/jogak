package net.pocorall.jogak.viewer

import javax.swing.table.TableModel
import javax.swing.event.TableModelListener
import javax.swing._
import java.awt.event.{MouseEvent, MouseAdapter, ActionListener, ActionEvent}
import java.awt.{Dimension, BorderLayout}
import net.pocorall.jogak._

class TreeViewer(var model: TreeNode, implicit val viewerRegistry: CommandRegistry) extends Viewer {
  override def thing = model

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

  implicit def toActionListener(f: ActionEvent => Unit) = new ActionListener {
    def actionPerformed(e: ActionEvent) {
      f(e)
    }
  }


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
          spane.setRightComponent(viewerRegistry.getDefaultViewer(newModel))
        }
      } else {
        val menu = new JPopupMenu()
        SimpleFunctions.buildMenu(newModel, menu, spane.setRightComponent(_))

        menu.show(e.getComponent(), e.getX(), e.getY());
      }
    }
  })


  private val leftPane = new JPanel(new BorderLayout)
  leftPane.setMinimumSize(new Dimension(150, 150))

  private val upButton = new JButton("Up")
  upButton.addActionListener {
    e: ActionEvent => setModel(model.parent())
  }

  leftPane.add(upButton, BorderLayout.NORTH)

  private val closeButton = new JButton("Close")


  leftPane.add(closeButton, BorderLayout.SOUTH)

  leftPane.add(new JScrollPane(table), BorderLayout.CENTER)

  def setModel(newModel: TreeNode) {
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