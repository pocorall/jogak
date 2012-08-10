package net.pocorall.jogak

import javax.swing._
import java.awt.{Component, BorderLayout}
import java.awt.event.{MouseEvent, MouseAdapter}
import viewer.EverythingViewer


abstract class Viewer extends JPanel(new BorderLayout) {
  def thing: Any

  def showContextMenu(component: Component, x: Int, y: Int) {
    val menu = new JPopupMenu()
    SimpleFunctions.buildMenu(thing, menu, showView(_))

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
      case t => new EverythingViewer(t)
    }
  }

  def lookup(thing: Any): Array[Command[Nothing]]
}