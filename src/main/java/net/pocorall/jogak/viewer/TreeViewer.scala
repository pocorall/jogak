package net.pocorall.jogak.viewer

import javax.swing.table.TableModel
import javax.swing.event.TableModelListener
import javax.swing._
import java.awt.event.{MouseEvent, MouseAdapter, ActionListener, ActionEvent}
import java.awt.{Dimension, BorderLayout}
import net.pocorall.jogak._

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
        def buildMenu(obj: Any, menu: JComponent) {
          cr.lookup(obj).foreach {
            com => com match {
              case c: Filter[Nothing] =>
                val result = com.execute(obj)
                result match {
                  case f: Viewer =>
                    addMenuItem(menu, com.name, _ => spane.setRightComponent(f))
                  case _ =>
                    val m = new JMenu(com.name)
                    //                  println(result.getClass().toString)
                    buildMenu(result, m)
                    menu.add(m)
                }
              case c: Command[Nothing] =>
                addMenuItem(menu, com.name, _ => spane.setRightComponent(viewerRegistry.lookup(com.execute(obj))))
              case _ =>
            }

          }
        }
        val menu = new JPopupMenu()
        buildMenu(newModel, menu)

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