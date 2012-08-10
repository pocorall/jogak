package net.pocorall.jogak.viewer

import net.pocorall.jogak.Viewer
import javax.swing.{SwingUtilities, JLabel, JScrollPane, JTextArea}
import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Graphics}
import java.awt.event.{MouseEvent, MouseAdapter}

class SimpleStringViewer(str: String) extends Viewer {
  override def thing = str

  private val textArea = new JTextArea(str)
  textArea.setEditable(false)
  addContextMenuListener(textArea)

  add(new JScrollPane(textArea), BorderLayout.CENTER)
}

class EverythingViewer(obj: Any) extends Viewer {
  override def thing = obj
  addContextMenuListener(this)
  add(new JLabel("Anything: " + obj.toString))
}

class SimpleImageViewer(img: BufferedImage) extends Viewer {
  override def thing = img
  addContextMenuListener(this)

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    g.drawImage(img, 0, 0, null)
  }
}