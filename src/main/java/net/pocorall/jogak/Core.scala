package net.pocorall.jogak

import javax.swing._
import java.awt.{Dimension, Component, BorderLayout}
import java.awt.event.{MouseEvent, MouseAdapter}
import viewer.EverythingViewer


abstract class Viewer(implicit commandRegistry: CommandRegistry) extends JPanel(new BorderLayout) {
  def thing: Any

  def showContextMenu(component: Component, x: Int, y: Int, model: Any = thing) {
    val menu = new JPopupMenu()
    SimpleFunctions.buildMenu(model, menu, showView(_))

    menu.show(component, x, y)
  }

  def addContextMenuListener(component: JComponent) {
    component.addMouseListener(new MouseAdapter {
      override def mouseReleased(e: MouseEvent) {
        if (SwingUtilities.isRightMouseButton(e)) {
          showContextMenu(e.getComponent, e.getX, e.getY)
        }
      }
    })
  }

  def showView(view: Viewer) {
    val previous = (getLayout.asInstanceOf[BorderLayout]).getLayoutComponent(BorderLayout.CENTER)
    if (previous != null) remove(previous)
    add(view, BorderLayout.CENTER)
    revalidate()
  }
}

abstract class SplitViewer(implicit commandRegistry: CommandRegistry) extends Viewer {

  protected val leftPane = new JPanel(new BorderLayout)
  leftPane.setMinimumSize(new Dimension(200, 150))

  protected val spane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPane, new JLabel("Hello"))

  add(spane, BorderLayout.CENTER)

  override def showView(view: Viewer) {
    spane.setRightComponent(view)
  }
}

trait ViewerRegistry {
  def lookup(thing: Any): Viewer
}

class Command[-T: Manifest](val name: String, val command: T => Any, val enabled: T => Boolean = ((_: T) => true)) {
  val fromClass = manifest[T].erasure

  def execute(param: Any): Any = {
    if (isAssignable(param)) {
      return command.asInstanceOf[Any => Any](param)
    }
    null
  }

  def isAssignable(param: Any): Boolean = fromClass.isAssignableFrom(param.getClass) && enabled.asInstanceOf[Any => Boolean](param)
}

class Filter[-T: Manifest](name: String, override val command: T => Any, override val enabled: T => Boolean = ((_: T) => true)) extends Command[T](name, command, enabled)

abstract class CommandRegistry {
  def applyDefaultFilterChain(thing: Any): Any = {
    lookup(thing).find(c => c.isInstanceOf[Filter[Nothing]]) match {
      case Some(c) => applyDefaultFilterChain(c.execute(thing))
      case None => thing
    }
  }

  def getDefaultViewer(thing: Any): Viewer = {
    applyDefaultFilterChain(thing) match {
      case v: Viewer => v
      case t => new EverythingViewer(t)(this)
    }
  }

  def lookup(thing: Any): Array[Command[Nothing]]
}