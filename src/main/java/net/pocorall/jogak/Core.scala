package net.pocorall.jogak

import javax.swing.JPanel
import java.awt.BorderLayout

/**marker class */
abstract class Viewer extends JPanel(new BorderLayout);


trait ViewerRegistry {
  def lookup(thing: Any): Viewer
}

class Command(val name: String, result: => Any) {
  def run() = result
}

class Filter(name: String, result: => Any) extends Command(name, result)
