package net.pocorall.jogak

import javax.swing.{JMenu, SwingUtilities, JPanel}
import java.awt.BorderLayout
import java.awt.event.{MouseEvent, MouseAdapter}

/**marker class */
abstract class Viewer extends JPanel(new BorderLayout) {
  def thing: Any

  addMouseListener(new MouseAdapter {
    override def mouseReleased(e: MouseEvent) {
      if (SwingUtilities.isRightMouseButton(e)) {
        val m = new JMenu(thing.toString)
        //        buildMenu(thing, m, spane)
        //        menu.add(m)
      }
    }
  })
}


trait ViewerRegistry {
  def lookup(thing: Any): Viewer
}

class Command[-T: Manifest](val name: String, val command: T => Any) {
  val fromClass = manifest[T].erasure

  def execute(param: Any): Any = {
    if (isAssignable(param)) {
      return command.asInstanceOf[Any => Any](param)
    }
    return null
  }

  def isAssignable(param: Any): Boolean = fromClass.isAssignableFrom(param.getClass)

}

class Filter[-T: Manifest](name: String, val result: T => Any) extends Command[T](name, result)
