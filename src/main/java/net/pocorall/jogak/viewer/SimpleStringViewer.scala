package net.pocorall.jogak.viewer

import net.pocorall.jogak.{Viewer}
import javax.swing.{JScrollPane, JTextArea}

class SimpleStringViewer(str: String) extends Viewer {
  private val textArea = new JTextArea(str)
  textArea.setEditable(false)
  add(new JScrollPane(textArea))
}
