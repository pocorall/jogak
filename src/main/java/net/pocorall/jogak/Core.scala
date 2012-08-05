package net.pocorall.jogak

import javax.swing.JPanel
import java.awt.BorderLayout

/**marker class */
abstract class Viewer extends JPanel(new BorderLayout);


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
